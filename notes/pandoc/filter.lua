-- Copyright 2020 Mitchell Kember. Subject to the MIT License.

local vars = {}
local highlight = false

function read_meta(meta)
    vars.active = meta.active
    vars.root = meta.root
end

function add_bookmark(el)
    if #el.classes == 1 then
        assert(el.classes[1] == "highlight",
            "unexpected class: " .. el.classes[1])
        highlight = true
        local href = vars.root:sub(4) .. "highlight.html"
        local bookmark = (
            '<a class="highlight__link link" href="' .. href .. '">'
            .. '<svg alt="" class="bookmark" width="16" height="24">'
            .. '<use xlink:href="#bookmark"/>'
            .. '</svg></a>'
        )
        table.insert(el.content, 1, pandoc.RawBlock("html", bookmark))
        return el
    end
end

function write_meta(meta)
    meta.highlight = highlight
    meta.up_or_highlight = meta.up or highlight
    return meta
end

function fix_internal_link(el)
    print(PANDOC_STATE.output_file)
    if string.sub(el.target, 1, 1) == ":" then
        -- need more info to create the relative link, not just "root"
        -- also should use [](#1.1) empty content, and do § for text, Lecture
        -- for Lecture, etc. 1.1/1a unambig but exercises ambig.
        -- could do [](:1.1), [](?1.1), [](:1a) <--- :-)
        -- if vars.active == "index" then
        -- elseif vars.active == "lecture" then
        -- elseif vars.active == "exercise" then
        -- el.target = vars["root"] .. "index.html"
        return el
    end
end

function add_scheme_class(el)
    if #el.classes == 0 then
        el.classes = {"scheme"}
        return el
    end
end

function walk_add_scheme_class(el)
    return pandoc.walk_block(el, {Code = add_scheme_class})
end

-- function format_citation(el)
--     -- rPrint(el)
--     return pandoc.RawInline("html", "<cite>Nice</cite>")
-- end

return {
    {Meta = read_meta},
    {Div = add_bookmark},
    {Meta = write_meta},
    {Link = fix_internal_link},
    {CodeBlock = add_scheme_class},
    {Para = walk_add_scheme_class},
    {BulletList = walk_add_scheme_class},
    {OrderedList = walk_add_scheme_class},
    -- {Cite = format_citation}
}
