src := $(wildcard *.md)
dst := $(src:%.md=%.html)
css := style.css
before := before.html

flags :=

.PHONY: all watch

all: $(dst)

watch:
	ls $(src) $(css) $(before) | entr make

%.html: %.md $(css) $(before)
	pandoc -o $@ $< --standalone --css=$(css) --katex $(flags) -B $(before)

textbook.html lecture.html: flags += --toc
textbook.html: flags += --toc-depth=3
lecture.html: flags += --toc-depth=1
