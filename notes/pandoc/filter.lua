-- Copyright 2020 Mitchell Kember. Subject to the MIT License.

local vars = {}
local highlight = false

-- Reads metatdata set on the command line by docgen.
function read_meta(meta)
    vars.active = meta.active
    vars.root = meta.root
end

-- Adds the bookmark link to `::: highlight` divs surrounding blockquotes.
function highlight_div(el)
    if #el.classes == 1 then
        assert(el.classes[1] == "highlight",
            "unexpected class: " .. el.classes[1])
        highlight = true
        local href = vars.root:sub(4) .. "highlight.html"
        local bookmark = (
            '<a class="highlight__link link" href="' .. href .. '">'
            .. '<svg alt="" class="bookmark" width="20" height="20">'
            .. '<use xlink:href="#bookmark"/>'
            .. '</svg> See all highlights</a>'
        )
        table.insert(el.content, 1, pandoc.RawBlock("html", bookmark))
        return el
    end
end

-- Writes metadata variables used in template.html.
function write_meta(meta)
    meta.has_pagenav = meta.up
    meta.use_arrows = meta.up
    meta.use_bookmark = highlight
    meta.svg_defs = meta.use_arrows or meta.use_bookmark
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
    local src = PANDOC_STATE.output_file:match("^docs/(.*)$")
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
