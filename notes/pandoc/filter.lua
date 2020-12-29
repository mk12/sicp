-- Copyright 2020 Mitchell Kember. Subject to the MIT License.

function add_scheme_class(el)
    if #el.classes == 0 then
        el.classes = {"scheme"}
        return el
    end
end

function walk_add_scheme_class(el)
    return pandoc.walk_block(el, {Code = add_scheme_class})
end

--[[ rPrint(struct, [limit], [indent])   Recursively print arbitrary data. 
	Set limit (default 100) to stanch infinite loops.
	Indents tables as [KEY] VALUE, nested tables as [KEY] [KEY]...[KEY] VALUE
	Set indent ("") to prefix each line:    Mytable [KEY] [KEY]...[KEY] VALUE
--]]
function rPrint(s, l, i) -- recursive Print (structure, limit, indent)
	l = (l) or 100; i = i or "";	-- default item limit, indent string
	if (l<1) then print "ERROR: Item limit reached."; return l-1 end;
	local ts = type(s);
	if (ts ~= "table") then print (i,ts,s); return l-1 end
	print (i,ts);           -- print "table"
	for k,v in pairs(s) do  -- print "[KEY] VALUE"
		l = rPrint(v, l, i.."\t["..tostring(k).."]");
		if (l < 0) then break end
	end
	return l
end

function format_citation(el)
    -- rPrint(el)
    return pandoc.RawInline("html", "<cite>Nice</cite>")
end

return {
    {Para = walk_add_scheme_class},
    {BulletList = walk_add_scheme_class},
    {OrderedList = walk_add_scheme_class},
    {CodeBlock = add_scheme_class},
    {Cite = format_citation}
}
