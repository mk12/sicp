SHELL := /bin/bash

CFLAGS := -std=c11 -W -Wall $(if $(DEBUG),-O0 -g,-O3)
DENOFLAGS := --unstable --allow-read --allow-write --allow-run
lua_version := 5.4

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
	config.yml filter.lua render.ts pagenav.html scheme.xml template.html)

render_src := notes/pandoc/render.ts
render_sock := render.sock

# Made-up headings that are allowed in chapter-*.ss.
heading_exceptions := \
	A sample simulation\|One-dimensional tables\|Primitive procedures

# HTML validation errors to ignore.
validate_exceptions := \
	'.*not allowed as child of element “mo”.*'

define usage
Targets:
	all        Build and test everything
	help       Show this help message
	test       Run tests in Chez, Guile, and Racket
	docs       Build the website in docs/
	render     Run the render.ts server
	fmt        Format C and TypeScript code
	lint       Lint all source files
	lintss     Lint only Scheme code
	spell      Spellcheck Markdown and Scheme files
	validate   Validate generated HTML files
	clean      Remove compilation artifacts
	vscode     Install vscode tasks
	sicp_html  Download SICP HTML files
endef

.PHONY: all help test docs lua_env render fmt lint lintss spell validate clean \
	vscode sicp_html

# Ordered from fastest to slowest, for early feedback.
all: lint fmt spell docs validate test

help:
	$(info $(usage))
	@:

test:
	./run.sh all

docs: $(doc_html)

$(doc_html): docgen $(doc_assets) $(doc_pandoc_aux) | lua_env $(render_sock)
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

# This target ensures that we shell out only if needed, and at most once.
lua_env:
	$(eval export LUA_PATH := \
		$$(shell luarocks path --lua-version=$(lua_version) --lr-path))
	$(eval export LUA_CPATH := \
		$$(shell luarocks path --lua-version=$(lua_version) --lr-cpath))

render:
	deno run $(DENOFLAGS) $(render_src) $(render_sock)

ifneq (,$(wildcard $(render_sock)))
$(render_sock): $(render_src)
	$(error \
A server is already running on $(render_sock), but it is using outdated code.\
Try again after terminating or restarting it)
else
.INTERMEDIATE: $(render_sock)
$(render_sock): DENOFLAGS += --no-check
$(render_sock):
	deno run $(DENOFLAGS) $(render_src) $@ &
	deno run $(DENOFLAGS) $(render_src) --wait $@
endif

fmt:
	find . -type f \( -name "*.c" -o -name "*.m" \) | xargs clang-format -i
	find . -type f -name "*.ts" | xargs deno fmt

lint: lintss
	find . -type f -name "*.sh" | xargs shellcheck
	find . -type f -name "*.ts" | xargs deno lint --unstable
	# Ensure all headings in the code appear in text.md.
	! comm -13 \
	<(grep '^#' notes/text.md \
		| sed -E 's/^#+ ([0-9.]+: )?//' | sort) \
	<(grep '^(\(Chapter\|Section\)' $(sicp_src) \
		| sed 's/^.*"\(.*\)".*$$/\1/;' | sort) \
	| grep -v '^$(heading_exceptions)$$' | grep '^'

lintss: linter
	find . -type f \( -name "*.ss" -o -name "*.md" \) | xargs ./$<

spell: spellc
	#./$^ notes/{index,text,lecture,exercise}.md src/sicp/chapter-{1,2,3,4,5}.ss

spellc: %: %.m
	$(CC) -o $@ $(CFLAGS) -fmodules -fobjc-arc $^

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

# Download SICP HTML files locally. Useful for development.
sicp_url_prefix := \
	https://mitpress.mit.edu/sites/default/files/sicp/full-text/book/book-Z-H
sicp_html:
	mkdir $@
	num_sec=(0 3 5 5 4 5); \
	offset=(0 0 3 8 13 17); \
	for c in 1 2 3 4 5; do \
		echo "Chapter $$c ..."; \
		for s in $$(seq 0 $${num_sec[$$c]}); do \
			o=$${offset[$$c]}; \
			n=$$(( 8 + c + o + s )); \
			curl -s $(sicp_url_prefix)-$$n.html > $@/$$c.$$s.html & \
		done; \
		wait; \
	done
