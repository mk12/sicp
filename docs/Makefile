src := notes.md
dst := index.html
css := style.css

.PHONY: all watch

all: $(dst)

watch:
	ls $(src) $(css) | entr make $(dst)

$(dst): $(src) $(css)
	pandoc $(src) -o $(dst) -s --css=$(css) --katex --toc --toc-depth=3
