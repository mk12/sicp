src := notes.md
dst := out.html
css := style.css

.PHONY: all clean watch

all: $(dst)

clean:
	rm -f $(dst)

watch:
	ls $(src) $(css) | entr make $(dst)

$(dst): $(src) $(css)
	pandoc $(src) -o $(dst) -s --css=$(css) --katex --toc --toc-depth=3
