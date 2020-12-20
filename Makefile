CFLAGS := -std=c11 -W -Wall -O3

doc_config := notes/pandoc.yml
doc_template := notes/template.html
doc_flags := --defaults $(doc_config) --template $(doc_template)
doc_deps := $(doc_config) $(doc_template)
doc_pages := index lecture quote textbook

.PHONY: all help test docs lint spell check clean vscode

# Ordered from fastest to slowest for early feedback.
all: lint check spell docs test

help:
	@echo "Targets:"
	@echo "all     build and test everything"
	@echo "help    show this help message"
	@echo "test    run tests in Chez, Guile, and Racket"
	@echo "docs    build the website in docs/"
	@echo "lint    lint Scheme and Markdown files"
	@echo "spell   spellcheck Markdown files"
	@echo "check   run shellcheck on scripts"
	@echo "clean   remove compilation artifacts"
	@echo "vscode  install vscode tasks"

test:
	./run.sh all

docs: $(patsubst %,docs/%.html,$(doc_pages))

docs/%.html: notes/%.md $(doc_deps)
	pandoc -o $@ $< $(doc_flags)

docs/lecture.html: doc_flags += --toc --toc-depth=1
docs/quote.html: doc_flags += --toc --toc-depth=3
docs/textbook.html: doc_flags += --toc --toc-depth=3

lint: linter
	find . -type f \( -name "*.ss" -o -name "*.md" \) | xargs ./$<

linter: linter.c
	$(CC) $(CFLAGS) -o $@ $<

spell:
	@echo TODO

check:
	find . -type f -name "*.sh" | xargs shellcheck 

clean:
	find src -type d -name compiled -exec rm -rf {} +

vscode: .vscode/tasks.json

.vscode/%.json: %.json
	mkdir -p .vscode
	cp $< $@
