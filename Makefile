SHELL := /bin/bash

CFLAGS := -std=c11 -W -Wall $(if $(DEBUG),-O0 -g,-O3)
DENOFLAGS := --unstable --allow-read --allow-write

sicp_src := $(patsubst %,src/sicp/chapter-%.ss,1 2 3 4 5)

doc_sec_1 := 1/index 1/1 1/2 1/3
doc_sec_2 := 2/index 2/1 2/2 2/3 2/4 2/5
doc_sec_3 := 3/index 3/1 3/2 3/3 3/4 3/5
doc_sec_4 := 4/index 4/1 4/2 4/3 4/4
doc_sec_5 := 5/index 5/1 5/2 5/3 5/4 5/5
doc_sec_all := $(doc_sec_1) $(doc_sec_2) $(doc_sec_3) $(doc_sec_4) $(doc_sec_5)
doc_lec_nums := 1a 1b 2a 2b 3a 3b 4a 4b 5a 5b 6a 6b 7a 7b 8a 8b 9a 9b 10a 10b

doc_index := docs/index.html
doc_text := $(patsubst %,docs/text/%.html,index front highlight $(doc_sec_all))
doc_lecture := $(patsubst %,docs/lecture/%.html,index highlight $(doc_lec_nums))
doc_exercise := $(patsubst %,docs/exercise/%.html,index language $(doc_sec_all))
doc_html := $(doc_index) $(doc_text) $(doc_lecture) $(doc_exercise)

doc_assets := $(patsubst %,notes/assets/%.svg,\
	arrows circle-left circle-right external github)
doc_pandoc_aux := $(patsubst %,notes/pandoc/%,\
	config.yml filter.lua katex.ts pagenav.html scheme.xml template.html)

katex_src := notes/pandoc/katex.ts
katex_sock := katex.sock

# Made-up headings that are allowed in chapter-*.ss.
heading_exceptions := \
	A sample simulation\|One-dimensional tables\|Primitive procedures

# HTML validation errors to ignore.
validate_exceptions := \
	'.*“mrow” not allowed as child of element “mo”.*'

.PHONY: all help test docs katex fmt lint lintscheme spell validate clean vscode

# Ordered from fastest to slowest, for early feedback.
all: lint fmt spell docs validate test

help:
	@echo "Targets:"
	@echo "all       build and test everything"
	@echo "help      show this help message"
	@echo "test      run tests in Chez, Guile, and Racket"
	@echo "docs      build the website in docs/"
	@echo "katex     run the katex server"
	@echo "fmt       format C and TypeScript code"
	@echo "lint      lint all source files"
	@echo "spell     spellcheck Markdown files"
	@echo "validate  validate generated HTML files"
	@echo "clean     remove compilation artifacts"
	@echo "vscode    install vscode tasks"

test:
	./run.sh all

docs: $(doc_html)

$(doc_html): docgen $(doc_assets) $(doc_pandoc_aux) | $(katex_sock)
	./docgen $@

$(doc_index): notes/index.md notes/assets/wizard.svg
$(doc_text): notes/text.md
$(doc_lecture): notes/lecture.md
docs/exercise/index.html: notes/exercise.md $(sicp_src)
docs/exercise/language.html: notes/exercise.md
$(patsubst %,docs/exercise/%.html,$(doc_sec_1)): src/sicp/chapter-1.ss
$(patsubst %,docs/exercise/%.html,$(doc_sec_2)): src/sicp/chapter-2.ss
$(patsubst %,docs/exercise/%.html,$(doc_sec_3)): src/sicp/chapter-3.ss
$(patsubst %,docs/exercise/%.html,$(doc_sec_4)): src/sicp/chapter-4.ss
$(patsubst %,docs/exercise/%.html,$(doc_sec_5)): src/sicp/chapter-5.ss

katex:
	deno run $(DENOFLAGS) $(katex_src) $(katex_sock)

ifneq (,$(wildcard ./katex.sock))
$(katex_sock): $(katex_src)
	$(error \
A server is already running on $(katex_sock), but it is using outdated code.\
Try again after terminating or restarting it)
else
.INTERMEDIATE: $(katex_sock)
$(katex_sock):
	deno run $(DENOFLAGS) $(katex_src) $@ &
	deno run $(DENOFLAGS) $(katex_src) --wait $@
endif

fmt:
	find . -type f -name "*.ts" | xargs deno fmt

lint: lintscheme
	find . -type f -name "*.sh" | xargs shellcheck
	find . -type f -name "*.ts" | xargs deno lint --unstable
	# Ensure all headings in the code appear in text.md.
	! comm -13 \
	<(grep '^#' notes/text.md \
		| sed -E 's/^#+ ([0-9.]+: )?//' | sort) \
	<(grep '^(\(Chapter\|Section\)' $(sicp_src) \
		| sed 's/^.*"\(.*\)".*$$/\1/;' | sort) \
	| grep -v '^$(heading_exceptions)$$' | grep '^'

lintscheme: linter
	find . -type f \( -name "*.ss" -o -name "*.md" \) | xargs ./$<

spell:
	@echo TODO

validate:
	find docs -type f -name "*.html" \
	| xargs vnu --filterpattern $(validate_exceptions)

clean:
	find src -type d -name compiled -exec rm -rf {} +
	-rm -rf *.dSYM

vscode: .vscode/tasks.json

.vscode/%.json: %.json
	mkdir -p .vscode
	cp $< $@
