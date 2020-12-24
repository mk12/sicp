CFLAGS := -std=c11 -W -Wall $(if $(DEBUG),-O0 -g,-O3)

doc_sec_1 := 1/index 1/1 1/2 1/3
doc_sec_2 := 2/index 2/1 2/2 2/3 2/4 2/5
doc_sec_3 := 3/index 3/1 3/2 3/3 3/4 3/5
doc_sec_4 := 4/index 4/1 4/2 4/3 4/4
doc_sec_5 := 5/index 5/1 5/2 5/3 5/4 5/5
doc_sections := $(doc_sec_1) $(doc_sec_2) $(doc_sec_3) $(doc_sec_4) $(doc_sec_5)
doc_lecture_no := 1a 1b 2a 2b 3a 3b 4a 4b 5a 5b 6a 6b 7a 7b 8a 8b 9a 9b 10a 10b
doc_index := docs/index.html
doc_text := $(patsubst %,docs/text/%.html,index front $(doc_sections))
doc_lecture := $(patsubst %,docs/lecture/%.html,index $(doc_lecture_no))
doc_exercise := $(patsubst %,docs/exercise/%.html,index $(doc_sections))
doc_quote := $(patsubst %,docs/%/quote.html,text lecture)
doc_html := $(doc_index) $(doc_text) $(doc_lecture) $(doc_exercise) $(doc_quote)
doc_assets := $(patsubst %,docs/assets/%,style.css wizard.svg)

.PHONY: all help test docs lint spell check clean vscode

# Ordered from fastest to slowest, for early feedback.
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

docs: $(doc_assets) $(doc_html)

docgen linter: %: %.c
	$(CC) $(CFLAGS) -o $@ $<

$(doc_assets): docs/assets/%: notes/assets/%
	mkdir -p docs/assets
	-ln -s ../../$< $@

$(doc_html): docgen notes/template.html notes/assets/github.svg
	./docgen $@

$(doc_index): notes/index.md
$(doc_text): notes/text.md
$(doc_lecture): notes/lecture.md
$(doc_quote): notes/quote.md
docs/exercise/index.html: src/sicp/*.ss
$(patsubst %,docs/exercise/%.html,$(doc_sec_1)): src/sicp/chapter-1.ss
$(patsubst %,docs/exercise/%.html,$(doc_sec_2)): src/sicp/chapter-2.ss
$(patsubst %,docs/exercise/%.html,$(doc_sec_3)): src/sicp/chapter-3.ss
$(patsubst %,docs/exercise/%.html,$(doc_sec_4)): src/sicp/chapter-4.ss
$(patsubst %,docs/exercise/%.html,$(doc_sec_5)): src/sicp/chapter-5.ss

lint: linter
	find . -type f \( -name "*.ss" -o -name "*.md" \) | xargs ./$<

spell:
	@echo TODO

check:
	find . -type f -name "*.sh" | xargs shellcheck 

clean:
	find src -type d -name compiled -exec rm -rf {} +
	-rm -rf *.dSYM

vscode: .vscode/tasks.json

.vscode/%.json: %.json
	mkdir -p .vscode
	cp $< $@
