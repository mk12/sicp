-- Copyright 2021 Mitchell Kember. Subject to the MIT License.

-- Used for reading and writing Pandoc metadata.
local vars = {}

-- The luaposix modules are loaded only if needed.
local S = nil  -- posix.sys.socket
local U = nil  -- posix.unistd

-- Connection to the katex server.
local socket = nil

-- Pre-renders math with KaTeX.
function render_math(el)
    vars.math = true
    if not socket then
        S = require("posix.sys.socket")
        U = require("posix.unistd")
        socket = assert(S.socket(S.AF_UNIX, S.SOCK_STREAM, 0))
        assert(S.connect(socket, {family = S.AF_UNIX, path = "katex.sock"}))
    end
    local request = el.text .. "\x00"
    if el.mathtype == "DisplayMath" then
        request = "display:" .. request
    end
    local i = 1
    while i < #request do
        i = assert(S.send(socket, request:sub(i))) + 1
    end
    local chunks = {}
    while true do
        chunk = assert(S.recv(socket, 1024))
        if chunk:sub(#chunk) == "\x00" then
            table.insert(chunks, chunk:sub(1, #chunk - 1))
            break
        end
        table.insert(chunks, chunk)
    end
    local response = table.concat(chunks)
    return pandoc.RawInline("html", response)
end

-- Closes the katex socket, if it was opened.
function close_socket()
    if socket then
        assert(U.close(socket))
    end
end

-- Reads metatdata set on the command line by docgen.
function read_meta(meta)
    vars.id = meta.id
    -- Relative path to the root of the website.
    vars.root = meta.id:gsub("[^/]+", ".."):gsub("..$", "", 1)
end

-- Number of "::: highlight" divs seen so far.
local highlight_count = 0
-- True if the page has a forward reference from highlights to notes.
local highlight_fwd = false
-- True if the page has a backward reference from notes to highlights.
local highlight_bwd = false

-- Processes "::: exercises" and "::: highlight" divs.
function div(el)
    assert(#el.classes == 1, "bad number of classes")
    if el.classes[1] == "exercises" then
        return exercises_div(el)
    elseif el.classes[1] == "highlight" then
        return highlight_div(el)
    end
    assert(false, "bad div class: " .. el.classes[1])
end

-- Converts a "::: exercises" div to a list of exercise links.
function exercises_div(el)
    assert(#el.content == 1, "bad div size: " .. #el.content)
    assert(el.content[1].t == "Para", "bad tag: " .. el.content[1].t)
    local range = pandoc.utils.stringify(el)
    local chap, from, to = range:match("(%d)%.(%d+)-(%d+)")
    assert(chap, "bad exercise range: " .. range)
    local links = {}
    chap = tonumber(chap)
    from = tonumber(from)
    to = tonumber(to)
    for ex = from, to do
        local target, frag = exercise_target(chap, ex)
        local href = relpath(vars.id .. ".html", target) .. frag
        table.insert(links,
            string.format(
                '<li class="flat__item"><a href="%s">%d.%d</a>%s</li>',
                href, chap, ex, ex == to and "" or ", "))
    end
    return {pandoc.RawBlock("html",
        '<aside><h4>Exercises:</h4> <ul class="flat">'
        .. table.concat(links) .. '</ul></aside>')}
end

-- Styles and links blockquotes marked with the "::: highlight" div
function highlight_div(el)
    assert(#el.content == 1, "bad div size: " .. #el.content)
    assert(el.content[1].t == "BlockQuote", "bad tag: " .. el.content[1].t)
    local aria, href, content
    if #el.identifier > 0 then
        highlight_fwd = true
        aria = "view quote in notes"
        href = el.identifier
            :gsub("^(%d)%-", "%1/index-", 1):gsub("%.", "/", 1)
            :gsub("^(.*)-(q%d+)$", "%1.html#%2", 1)
        content = (
            'Notes <svg class="circle-arrow" width="18" height="18"'
            .. ' aria-hidden="true"><use xlink:href="#circle-right"/></svg>'
        )
    else
        highlight_bwd = true
        highlight_count = highlight_count + 1
        el.identifier = "q" .. tostring(highlight_count)
        aria = "view quote in highlights page"
        local frag = vars.id
            :gsub("^.-/", "", 1):gsub("/index$", "", 1):gsub("/", ".", 1)
            .. "-" .. el.identifier
        href = vars.root:sub(4) .. "highlight.html#" .. frag
        content = (
            '<svg class="circle-arrow" width="18" height="18"'
            .. ' aria-hidden="true"><use xlink:href="#circle-left"/></svg>'
            .. ' Highlights'
        )
    end
    local link = (
        '<a class="highlight__link link" href="' .. href .. '"'
        .. ' aria-label="' .. aria .. '">' .. content .. '</a>'
    )
    table.insert(el.content, 1, pandoc.RawBlock("html", link))
    return el
end

-- Writes metadata variables used in template.html.
function write_meta(meta)
    meta[meta.id:gsub("/.*$", "")] = true
    meta.root = vars.root
    meta.math = vars.math
    meta.pagenav = meta.up
    meta.arrows = meta.up
    meta.external = meta.up and meta.prev
    meta.circle_left = highlight_bwd
    meta.circle_right = highlight_fwd
    meta.svg_defs = (
        meta.arrows or meta.external or meta.circle_left or meta.circle_right
    )
    return meta
end

-- Returns the relative path from src to dst.
function relpath(src, dst)
    if src == dst then
        return ""
    end
    -- Find the longest common prefix ending in a slash.
    local i = 1
    local j = 0
    while i < #src and i < #dst do
        if src:sub(i, i) ~= dst:sub(i, i) then
            break
        end
        if src:sub(i, i) == "/" then
            j = i
        end
        i = i + 1
    end
    -- Take the differing suffix of dst, and prepend "../" once for every slash
    -- in the differing suffix of src.
    j = j + 1
    local rel = dst:sub(j)
    while j < #src do
        if src:sub(j, j) == "/" then
            rel = "../" .. rel
        end
        j = j + 1
    end
    return rel
end

-- Usage: last_exercise_in_section[chapter][section].
local last_exercise_in_section = {
    { 8, 28, 46 },
    { 16, 52, 72, 76, 97 },
    { 8, 11, 37, 49, 82 },
    { 24, 34, 54, 79 },
    { 6, 19, 22, 30, 52 },
}

-- Returns the path and fragment for an exercise.
function exercise_target(chap, ex)
    local sec
    for i, last in ipairs(last_exercise_in_section[chap]) do
        if ex <= last then
            sec = i
            break
        end
    end
    local num = string.format('%d.%d', chap, ex)
    assert(sec, "exercise " .. num .. " out of range")
    return "exercise/" .. tostring(chap) .. "/" .. tostring(sec) .. ".html",
        "#ex" .. num
end

-- Returns the path and fragment for an internal link.
function internal_target(sigil, num)
    if sigil == "@" then
        local lecture, rest = num:match("^(%d+[ab])(.*)$")
        if lecture then
            return "lecture/" .. lecture .. ".html", rest:gsub("%.", "#", 1)
        end
        if #num == 1 then
            return "text/" .. num .. "/index.html", ""
        end
        local chap, sec = num:match("^(%d)%.(%d)")
        return "text/" .. chap .. "/" .. sec .. ".html",
            #num > 3 and "#" .. num or ""
    end
    if sigil == ":" then
        if #num == 1 then
            return "exercise/" .. num .. "/index.html", ""
        end
        local chap, sec = num:match("^(%d)%.(%d)")
        return "exercise/" .. chap .. "/" .. sec .. ".html",
            #num > 3 and "#" .. num or ""
    end
    assert(sigil == "?", "invalid sigil " .. sigil)
    local chap, ex = num:match("^(%d)%.(%d+)$")
    return exercise_target(tonumber(chap), tonumber(ex))
end

-- Converts links like [](:1.2.3), [](1a), and [](?1.23) -- textbook sections,
-- lectures, and exercises respectively. If the link content [] is empty, sets
-- it automatically to § 1.2.3, Lecture 1A, Exercise 1.23, etc.
function internal_link(el)
    local sigil = el.target:sub(1, 1)
    local num = el.target:sub(2)
    local prefix
    local ident = num
    if sigil == "@" then
        local lecture = num:match("^(%d+[ab])")
        if lecture then
            prefix = "Lecture"
            ident = lecture:upper()
        else
            prefix = num:find("%.") and "§" or "Chapter"
        end
    elseif sigil == ":" then
        prefix = num:find("%.") and "§" or "Chapter"
    elseif sigil == "?" then
        prefix = "Exercise"
    else
        return
    end
    local target, frag = internal_target(sigil, num)
    el.target = relpath(vars.id .. ".html", target) .. frag
    if el.target == "" then
        el.target = "#"
    end
    if #el.content == 0 then
        el.content = {pandoc.RawInline("html", prefix .. "&nbsp;" .. ident)}
    end
    return el
end

-- Adds the scheme class to CodeBlock elements.
function code_block(el)
    if #el.classes == 0 then
        el.classes = {"scheme"}
        return el
    end
end

-- Adds the scheme class to all Code elements within el. We do this, rather than
-- transforming every Code in the document, to avoid applying it in headings.
function walk_inline_code(el)
    return pandoc.walk_block(el, {Code = function(el)
        -- Only highlight inline code if it has a parenthesis (procedure
        -- application) or guillemet for meta-variables. Otherwise notes with
        -- lots of bits of code is too noisy, and also blue functions by
        -- themselves end up looking like links.
        if #el.classes == 0 and el.text:find("%(") or el.text:find("«") then
            el.classes = {"scheme"}
            return el
        end
    end })
end

-- These correspond to definitions in docgen.c.
local text_url_base =
    "https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H"
local lecture_url_base =
    "https://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-001-structure-and-interpretation-of-computer-programs-spring-2005/video-lectures"

-- Citation info for the frontmatter sections.
local frontmatter_info = {
    ["dedication"] = {
        person = true,
        text = "Alan J. Perlis",
        title = "SICP Dedication",
        page = 3,
    },
    ["foreword"] = {
        person = true,
        text = "Alan J. Perlis",
        title = "SICP Foreword",
        page = 5,
    },
    ["preface"] = {
        person = false,
        text = "Preface",
        title = "SICP Preface",
        page = 7,
    },
}

-- The ID for chapter C, footnote N is "footnote_Temp_" .. footnote_nums[C][N].
local footnote_nums = {
    {
        7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 23, 24, 25, 26,
        32, 33, 34, 35, 36, 40, 42, 44, 45, 46, 47, 48, 51, 53, 54, 57, 59, 62,
        63, 64, 68, 70, 71, 72, 75, 77, 78, 80, 81, 90, 91, 95, 96, 99, 101,
        104, 105, 107, 108, 114, 115, 117, 118, 119, 121, 122, 123,
    },
    {
        131, 133, 135, 136, 140, 154, 155, 156, 157, 158, 164, 166, 170, 182,
        183, 186, 190, 194, 195, 196, 197, 202, 204, 207, 208, 211, 215, 216,
        220, 221, 225, 227, 228, 229, 230, 233, 240, 248, 249, 250, 254, 258,
        268, 269, 270, 271, 272, 276, 283, 285, 286, 289, 295, 298, 299, 300,
        301, 303, 304, 314, 317, 320,
    },
    {
        321, 322, 323, 324, 325, 330, 331, 333, 335, 337, 339, 342, 343, 345,
        346, 349, 350, 351, 356, 357, 364, 366, 370, 371, 372, 379, 385, 390,
        391, 392, 394, 398, 404, 405, 406, 407, 409, 410, 411, 415, 421, 422,
        423, 428, 429, 430, 431, 435, 439, 440, 441, 442, 443, 444, 445, 446,
        448, 450, 453, 455, 456, 458, 459, 465, 472, 477, 478, 479, 485, 487,
        494, 500, 504, 505, 506, 507,
    },
    {
        508, 509, 510, 511, 518, 520, 523, 524, 526, 527, 528, 530, 531, 544,
        545, 549, 550, 551, 553, 554, 555, 556, 558, 559, 560, 565, 568, 569,
        570, 571, 575, 576, 577, 580, 581, 582, 585, 587, 592, 593, 594, 598,
        599, 600, 601, 602, 603, 609, 616, 619, 620, 621, 626, 628, 629, 632,
        636, 645, 646, 647, 648, 651, 654, 655, 658, 659, 670, 672, 673, 674,
        676, 677, 680, 682, 683, 684, 686, 688, 689, 700, 702,
    },
    {
        715, 717, 718, 727, 744, 745, 747, 748, 749, 750, 752, 753, 758, 759,
        760, 762, 763, 764, 765, 767, 770, 771, 772, 773, 776, 777, 778, 781,
        784, 785, 786, 793, 794, 795, 797, 803, 806, 809, 812, 813, 814, 815,
        822, 823, 824, 826, 830, 833, 834, 835, 837, 838, 839, 840,
    },
}

-- Returns the URL to a specific part of the online SICP textbook.
function text_url(chapter, section, subsection, footnote)
    chapter = tonumber(chapter)
    section = section and tonumber(section) or 0
    subsection = subsection and tonumber(subsection)
    footnote = footnote and tonumber(footnote)
    assert(not (subsection and footnote))
    local num = 8 + chapter + ({0, 3, 8, 13, 17})[chapter] + section
    local frag = ""
    if subsection then
        frag = "#%25_sec_" .. chapter .. "." .. section .. "." .. subsection
    elseif footnote then
        frag = "#footnote_Temp_" .. footnote_nums[chapter][footnote]
    end
    return text_url_base .. "-" .. num .. ".html" .. frag
end

-- For each lecture, the speaker (A for Abelson, B for Sussman) and the path to
-- the transcript PDF relative to lecture_url_base.
local lectures = {
    ["1a"] = { speaker = "A", path = "1a-overview-and-introduction-to-lisp/-J_xL4IGhJA.pdf" },
    ["1b"] = { speaker = "S", path = "1b-procedures-and-processes-substitution-model/V_7mmwpgJHU.pdf" },
    ["2a"] = { speaker = "S", path = "2a-higher-order-procedures/eJeMOEiHv8c.pdf" },
    ["2b"] = { speaker = "A", path = "2b-compound-data/DrFkf-T-6Co.pdf" },
    ["3a"] = { speaker = "A", path = "3a-henderson-escher-example/PEwZL3H2oKg.pdf" },
    ["3b"] = { speaker = "S", path = "3b-symbolic-differentiation-quotation/bV87UzKMRtE.pdf" },
    ["4a"] = { speaker = "S", path = "4a-pattern-matching-and-rule-based-substitution/fXQ1SwKjDg.pdf" },
    ["4b"] = { speaker = "A", path = "4b-generic-operators/OscT4N2qq7o.pdf" },
    ["5a"] = { speaker = "S", path = "5a-assignment-state-and-side-effects/dO1aqPBJCPg.pdf" },
    ["5b"] = { speaker = "S", path = "5b-computational-objects/yedzRWhi-9E.pdf" },
    ["6a"] = { speaker = "A", path = "6a-streams-part-1/JkGKLILLy0I.pdf" },
    ["6b"] = { speaker = "A", path = "6b-streams-part-2/qp05AtXbOP0.pdf" },
    ["7a"] = { speaker = "S", path = "7a-metacircular-evaluator-part-1/aAlR3cezPJg.pdf" },
    ["7b"] = { speaker = "S", path = "7b-metacircular-evaluator-part-2/QVEOq5k6Xi0.pdf" },
    ["8a"] = { speaker = "A", path = "8a-logic-programming-part-1/rCqMiPk1BJE.pdf" },
    ["8b"] = { speaker = "A", path = "8b-logic-programming-part-2/GReBwkGFZcs.pdf" },
    ["9a"] = { speaker = "S", path = "9a-register-machines/cIc8ZBMcqAc.pdf" },
    ["9b"] = { speaker = "A", path = "9b-explicit-control-evaluator/Z8-qWEEwTCk.pdf" },
    ["10a"] = { speaker = "A", path = "10a-compilation/TqO6V3qR9Ws.pdf" },
    ["10b"] = { speaker = "S", path = "10b-storage-allocation-and-garbage-collection/AbK4bZhUk48.pdf" },
}

-- Returns citation information for a given [@id].
function citation_info(id)
    local info = frontmatter_info[id]
    if info then
        info.href = text_url_base .. "-" .. info.page .. ".html"
        return info
    end
    info = {person = false}
    if id:find("^%d$") then
        info.text = "Chapter&nbsp;" .. id
        info.title = "SICP Chapter " .. id
        info.href = text_url(id)
        return info
    end
    local chapter, section = id:match("^(%d).(%d)$")
    local subsection
    if not chapter then
        chapter, section, subsection = id:match("^(%d).(%d).(%d)$")
    end
    if chapter then
        info.text = "Section&nbsp;" .. id
        info.title = "SICP Section " .. id
        info.href = text_url(chapter, section, subsection)
        return info
    end
    local chapter, footnote = id:match("^(%d).fn(%d+)$")
    if not chapter then
        chapter, section, footnote = id:match("^(%d).(%d).fn(%d+)$")
    end
    if chapter then
        info.text = "Footnote&nbsp;" .. chapter .. "." .. footnote
        info.title = "SICP Chapter " .. chapter .. ", Footnote " .. footnote
        info.href = text_url(chapter, section, nil, footnote)
        return info
    end
    local num, page = id:match("^(%d+[ab])%.p(%d+)")
    if num then
        lec = assert(lectures[num])
        info.person = true
        info.text = ({A = "Abelson", S = "Sussman"})[lec.speaker]
        info.title =
            "SICP Lecture " .. num:upper() .. " transcript, page " .. page
        info.href = lecture_url_base .. "/" .. lec.path .. "#page=" .. page
        return info
    end
    assert(num, "bad citation id: " .. id)
end

-- Formats citations at the end of blockquotes.
function format_citation(el)
    assert(#el.citations == 1)
    local info = citation_info(el.citations[1].id)
    local html
    if info.person then
        html = '<span class="citation">—<a href="' .. info.href
            .. '" title="' .. info.title .. '">' .. info.text .. '</a></span>'
    else
        html = '<a class="citation" href="' .. info.href
            .. '" title="' .. info.title .. '">(' .. info.text .. ')</a>'
    end
    return pandoc.RawInline("html", html)
end

return {
    {Math = render_math},
    {Pandoc = close_socket},
    {Meta = read_meta},
    {Div = div},
    {Meta = write_meta},
    {Link = internal_link},
    {CodeBlock = code_block},
    {Para = walk_inline_code},
    {BulletList = walk_inline_code},
    {OrderedList = walk_inline_code},
    {Cite = format_citation}
}
