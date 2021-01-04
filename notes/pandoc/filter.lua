-- Copyright 2020 Mitchell Kember. Subject to the MIT License.

-- Used for reading and writing Pandoc metadata.
local vars = {}

-- These luaposix modules are loaded only if needed.
local S = nil  -- posix.sys.socket
local U = nil  -- posix.unistd

-- Connection to the katex server, used for rendering math.
local socket = nil

-- Number of "::: highlight" divs seen so far.
local highlight_count = 1
-- True if the page has a forward reference from highlights to notes.
local highlight_fwd = false
-- True if the page has a backward reference from notes to highlights.
local highlight_bwd = false

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

-- Styles and links blockquotes marked with the "::: highlight" div.
function highlight_div(el)
    assert(el.classes[1] == "highlight", "bad class: " .. el.classes[1])
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

-- Converts links like [](:1.2.3), [](1a), and [](?1.23) -- textbook sections,
-- lectures, and exercises respectively. If the link content [] is empty, sets
-- it automatically to § 1.2.3, Lecture 1A, Exercise 1.23, etc.
function internal_link(el)
    local sigil = el.target:sub(1, 1)
    local num = el.target:sub(2)
    local title, target
    local frag = ""
    if sigil == ":" then
        local lecture, rest = num:match("^(%d+[ab])(.*)$")
        if lecture then
            title = "Lecture&nbps;" .. lecture:upper()
            target = "lecture/" .. lecture .. ".html"
            frag = rest:gsub("%.", "#", 1)
        else
            title = "§&nbsp;" .. num
            if #num == 1 then
                target = "text/" .. num .. "/index.html"
            else
                chap, sec = num:match("^(%d)%.(%d)")
                target = "text/" .. chap .. "/" .. sec .. ".html"
                if #num > 3 then
                    frag = "#" .. num
                end
            end
        end
    elseif sigil == "?" then
        title = "Exercise&nbps;" .. num
        assert(false, "not implemented")
    else
        return
    end
    el.target = relpath(vars.id .. ".html", target) .. frag
    if el.target == "" then
        el.target = "#"
    end
    if #el.content == 0 then
        el.content = {pandoc.RawInline("html", title)}
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

-- function format_citation(el)
--     -- rPrint(el)
--     return pandoc.RawInline("html", "<cite>Nice</cite>")
-- end

return {
    {Math = render_math},
    {Pandoc = close_socket},
    {Meta = read_meta},
    {Div = highlight_div},
    {Meta = write_meta},
    {Link = internal_link},
    {CodeBlock = code_block},
    {Para = walk_inline_code},
    {BulletList = walk_inline_code},
    {OrderedList = walk_inline_code},
    -- {Cite = format_citation}
}
