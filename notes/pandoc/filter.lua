function add_scheme_class(el)
    if #el.classes == 0 and string.find(el.text, " ") then
        el.classes = {"scheme"}
        return el
    end
end

function format_citation(el)
    -- TODO: use pandoc @cite things instead.
    -- (and then they can actually link etc.)
    local node = el
    while node.content do
        node = node.content[#node.content]
    end
    if node.tag == "Str" and string.find(node.text, "^%(.+%)$") then
        node.text = node.text .. " FOO"
        return el
    end
end

return {
    {Code = add_scheme_class},
    {CodeBlock = add_scheme_class},
    {BlockQuote = format_citation},
}
