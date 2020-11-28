src := $(wildcard *.md)
dst := $(src:%.md=%.html)
css := style.css
before := before.html

flags :=

.PHONY: all help watch

all: $(dst)

help:
	@echo "Targets:"
	@echo "help   show this help message"
	@echo "all    convert all .md to .html"
	@echo "watch  rebuild automatically using entr"

watch:
	ls $(src) $(css) $(before) | entr make

%.html: %.md $(css) $(before)
	pandoc -o $@ $< --standalone --css=$(css) --katex $(flags) -B $(before)

textbook.html: flags += --toc --toc-depth=3
lecture.html: flags += --toc --toc-depth=1
quote.html: flags += --toc --toc-depth=3
