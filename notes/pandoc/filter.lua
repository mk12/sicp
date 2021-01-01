-- Copyright 2020 Mitchell Kember. Subject to the MIT License.

local vars = {}
local highlight_idx = 0
local highlight_fwd = false
local highlight_bwd = false

-- Reads metatdata set on the command line by docgen.
function read_meta(meta)
    vars.id = meta.id:gsub("^docs/", "", 1):gsub("%.html$", "", 1)
    vars.root = vars.id:gsub("[^/]+", ".."):gsub("..$", "", 1):gsub("^/$", "", 1)
end

-- Styles and links blockquotes marked with the "::: highlight" div.
function highlight_div(el)
    assert(el.classes[1] == "highlight", "bad class: " .. el.classes[1])
    assert(#el.content == 1, "bad div size: " .. #el.content)
    assert(el.content[1].t == "BlockQuote", "bad tag: " .. el.content[1].t)
    local aria, href, content
    if #el.identifier > 0 then
        highlight_fwd = true
        href = el.identifier
            :gsub("^(%d)%-", "%1/index-", 1):gsub("%.", "/", 1)
            :gsub("^(.*)-(q%d+)$", "%1.html#%2", 1)
        content = (
            'Notes <svg alt="" class="circle-arrow" width="18" height="18">'
            .. '<use xlink:href="#circle-right"/></svg>'
        )
        aria = "view quote in notes"
    else
        highlight_bwd = true
        highlight_idx = highlight_idx + 1
        el.identifier = "q" .. tostring(highlight_idx)
        local frag = vars.id
            :gsub("^.-/", "", 1):gsub("/index$", "", 1):gsub("/", ".", 1)
            .. "-" .. el.identifier
        href = vars.root:sub(4) .. "highlight.html#" .. frag
        content = (
            '<svg alt="" class="circle-arrow" width="18" height="18">'
            .. '<use xlink:href="#circle-left"/></svg> Highlights'
        )
        aria = "view quote in highlights page"
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
    meta.id = vars.id
    meta[meta.id:gsub("/.*$", "")] = true
    meta.root = vars.root
    meta.pagenav = meta.up
    meta.arrows = meta.up
    meta.external = not vars.id:find("^[%a/]*index$")
    meta.circle_left = highlight_bwd
    meta.circle_right = highlight_fwd
    meta.svg_defs = (
        meta.arrows or meta.external or meta.circle_left or meta.circle_right
    )
    return meta
end

-- Converts links like [](:1.2.3), [](1a), and [](?1.23) -- textbook sections,
-- lectures, and exercises respectively. If the link content [] is empty, sets
-- it automatically.
function internal_link(el)
    -- Determine the destination path.
    local sigil = el.target:sub(1, 1)
    local num = el.target:sub(2)
    local title, dest
    local frag = ""
    if sigil == ":" then
        local lecture, rest = num:match("^(%d+[ab])(.*)$")
        if lecture then
            title = "Lecture&nbps;" .. lecture:upper()
            dest = "lecture/" .. lecture .. ".html"
            frag = rest:gsub("%.", "#", 1)
        else
            title = "§&nbsp;" .. num
            if #num == 1 then
                dest = "text/" .. num .. "/index.html"
            else
                chap, sec = num:match("^(%d)%.(%d)")
                dest = "text/" .. chap .. "/" .. sec .. ".html"
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
    -- Compute the relative path from src to dest.
    local src = vars.id .. ".html"
    local rel
    if src == dest then
        rel = frag or "#"
    else
        local i = 1
        local j = 0
        while i < #src and i < #dest do
            if src:sub(i, i) ~= dest:sub(i, i) then
                break
            end
            if src:sub(i, i) == "/" then
                j = i
            end
            i = i + 1
        end
        rel = dest:sub(j + 1) .. frag
        while i < #src do
            if src:sub(i, i) == "/" then
                rel = "../" .. rel
            end
            i = i + 1
        end
    end
    -- Update the link.
    el.target = rel
    if #el.content == 0 then
        el.content = {pandoc.RawInline("html", title)}
    end
    return el
end

function scheme_code(el)
    if #el.classes == 0 then
        el.classes = {"scheme"}
        return el
    end
end

function scheme_code_inside(el)
    return pandoc.walk_block(el, {Code = scheme_code})
end

-- function format_citation(el)
--     -- rPrint(el)
--     return pandoc.RawInline("html", "<cite>Nice</cite>")
-- end

return {
    {Meta = read_meta},
    {Div = highlight_div},
    {Meta = write_meta},
    {Link = internal_link},
    {CodeBlock = scheme_code},
    {Para = scheme_code_inside},
    {BulletList = scheme_code_inside},
    {OrderedList = scheme_code_inside},
    -- {Cite = format_citation}
}
