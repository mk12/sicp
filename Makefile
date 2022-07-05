# Copyright 2022 Mitchell Kember. Subject to the MIT License.

define usage
Targets:
	all        Build and test everything
	help       Show this help message
	test       Run tests in all supported Schemes
	docs       Build the website in docs/
	render     Run the render.ts server
	fmt        Format source files
	lint       Lint all source files
	lintss     Lint only Scheme code
	spell      Spellcheck source files
	validate   Validate generated HTML files
	tools      Build tools
	clean      Remove compilation artifacts
	vscode     Set up VS Code support
	clangd     Write compile_commands.json
	sicp-html  Download SICP HTML files
endef

.PHONY: all help test docs render fmt lint lintss spell validate tools clean \
	vscode clangd lua-env

.SUFFIXES:

SHELL := /bin/bash

CFLAGS := -std=c11 -W -Wall $(if $(DEBUG),-O0 -g,-O3)
OBJCFLAGS := -fmodules -fobjc-arc
DENOFLAGS := --unstable --allow-{read,write}=render.sock,render.fifo \
	--allow-run=svgbob
lua_version := 5.4

sicp_src := $(patsubst %,src/sicp/chapter-%.ss,1 2 3 4 5)
notes_src := $(patsubst %,notes/%.md,index text lecture exercise)

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

tools := $(patsubst %,bin/%,docgen highlight.so lint spell)

vscode_files := $(patsubst %,.vscode/%.json,settings tasks)

# HTML validation errors to ignore.
validate_exceptions := \
	'.*not allowed as child of element “mo”.*'

# Ordered from fastest to slowest, for early feedback.
all: lint fmt docs validate test

help:
	$(info $(usage))
	@:

test:
	./run.sh all --plain

docs: $(doc_html)

$(doc_html): bin/docgen tools/render.ts \
		$(wildcard notes/assets/*.svg) $(wildcard notes/pandoc/*) \
		| lua-env render.sock
	$< $@

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
lua-env:
	$(eval export LUA_PATH := \
		$$(shell luarocks path --lua-version=$(lua_version) --lr-path))
	$(eval export LUA_CPATH := \
		$$(shell luarocks path --lua-version=$(lua_version) --lr-cpath))

render:
	deno run $(DENOFLAGS) tools/render.ts render.sock

ifeq (,$(wildcard render.sock))
.INTERMEDIATE: render.sock render.fifo
render.sock: render.fifo
	deno run $(DENOFLAGS) tools/render.ts $@ $< &
	< $<
render.fifo:
	mkfifo $@
else
render.sock: tools/render.ts
	$(error \
A server is already running on render.sock, but it is using outdated code.\
Try again after terminating or restarting it)
endif

fmt:
	find . -type f \( -name "*.c" -o -name "*.m" \) | xargs clang-format -i
	find . -type f -name "*.ts" | xargs deno fmt

lint: lintss
	find . -type f -name "*.sh" | xargs shellcheck
	find . -type f -name "*.ts" | xargs deno lint --unstable
	scripts/lint-headings.sh

lintss: bin/lint
	find . -type f \( -name "*.ss" -o -name "*.md" \) | xargs $<

spell: bin/spell
	$^ $(notes_src) $(sicp_src)

validate:
	find docs -type f -name "*.html" \
		| xargs vnu --filterpattern $(validate_exceptions)
	# Ensure special characters used for syntax highlighting are stripped.
	! grep -qRE '«|»|‹|›|```' docs

tools: $(tools)

$(tools): | bin

bin:
	mkdir -p $@

bin/docgen bin/lint: bin/%: tools/%.c
	$(CC) $(CFLAGS) -o $@ $^

bin/highlight.so: tools/highlight.c
	$(CC) $(CFLAGS) -I/opt/homebrew/include/lua5.4 -shared -o $@ $^

bin/spell: tools/spell.m
	$(CC) $(CFLAGS) $(OBJCFLAGS) -o $@ $^

clean:
	find src -type d -name compiled -exec rm -rf {} +
	find src -type f -name *.so -exec rm -f {} +
	-rm -rf bin

vscode: $(vscode_files) clangd

$(vscode_files): .vscode/%.json: .vscode/%.default.json
	cp $< $@

clangd: compile_commands.json

compile_commands.json: scripts/gen-compile-commands.sh $(MAKEFILE_LIST)
	$< $(tools) > $@

sicp-html: scripts/download-sicp-html.sh
	$< $@

print-%:
	@echo $($*)
