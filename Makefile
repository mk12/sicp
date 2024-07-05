# Copyright 2022 Mitchell Kember. Subject to the MIT License.

define usage
Targets:
	all        Build and test everything
	help       Show this help message
	test       Run tests in all supported Schemes
	docs       Build the website in docs/
	render     Run the render.ts server
	fmt        Format source files
	lint       Lint source files
	spell      Spellcheck source files
	validate   Validate generated HTML files
	tools      Build tools
	clean      Remove compilation artifacts
	vscode     Set up VS Code support
	clangd     Write compile_commands.json
	sicp-html  Download SICP HTML files
endef

.PHONY: all help test docs render fmt lint spell validate tools clean vscode \
	clangd

CFLAGS := -std=c11 -W -Wall $(if $(DEBUG),-O0 -g,-O3)
OBJCFLAGS := -fmodules -fobjc-arc
DENOFLAGS := --allow-{read,write}=render.sock,render.fifo --allow-run=svgbob

LUA_VERSION := 5.4
export LUA_CPATH := ./lib/?.so

project_md := README.md LICENSE.md

sicp_src := $(patsubst %,src/sicp/chapter-%.ss,1 2 3 4 5)
notes_src := $(patsubst %,notes/%.md,index text lecture exercise)

num_sec_1 := 1/index 1/1 1/2 1/3
num_sec_2 := 2/index 2/1 2/2 2/3 2/4 2/5
num_sec_3 := 3/index 3/1 3/2 3/3 3/4 3/5
num_sec_4 := 4/index 4/1 4/2 4/3 4/4
num_sec_5 := 5/index 5/1 5/2 5/3 5/4 5/5
num_sec_all := $(num_sec_1) $(num_sec_2) $(num_sec_3) $(num_sec_4) $(num_sec_5)
num_lecture := 1a 1b 2a 2b 3a 3b 4a 4b 5a 5b 6a 6b 7a 7b 8a 8b 9a 9b 10a 10b

doc_index := docs/index.html
doc_text := $(patsubst %,docs/text/%.html,index front highlight $(num_sec_all))
doc_lecture := $(patsubst %,docs/lecture/%.html,index highlight $(num_lecture))
doc_exercise := $(patsubst %,docs/exercise/%.html,index language $(num_sec_all))
doc_html := $(doc_index) $(doc_text) $(doc_lecture) $(doc_exercise)

validate_exceptions := \
	'.*not allowed as child of element “mo”.*'

c_tools := $(patsubst %,bin/%,docgen lint)
objc_tools := $(patsubst %,bin/%,spell)
lua_c_tools := $(patsubst %,lib/%.so,ntsp schemehl)
tools := $(c_tools) $(objc_tools) $(lua_c_tools)

vscode_files := $(patsubst %,.vscode/%.json,settings tasks)

.SUFFIXES:

# Ordered from fastest to slowest, for early feedback.
all: lint fmt docs validate test

help:
	$(info $(usage))
	@:

test:
	./run.sh all --plain

docs: $(doc_html)

$(doc_html): bin/docgen $(lua_c_tools) tools/render.ts \
		$(wildcard pandoc/*) $(wildcard pandoc/assets/*.svg) \
		| render.sock
	$< $@

$(doc_index): notes/index.md
$(doc_text): notes/text.md
$(doc_lecture): notes/lecture.md
docs/exercise/index.html: notes/exercise.md $(sicp_src)
docs/exercise/language.html: notes/exercise.md
$(num_sec_1:%=docs/exercise/%.html): src/sicp/chapter-1.ss
$(num_sec_2:%=docs/exercise/%.html): src/sicp/chapter-2.ss
$(num_sec_3:%=docs/exercise/%.html): src/sicp/chapter-3.ss
$(num_sec_4:%=docs/exercise/%.html): src/sicp/chapter-4.ss
$(num_sec_5:%=docs/exercise/%.html): src/sicp/chapter-5.ss

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

lint .PHONY: lint-ss lint-sh lint-ts lint-headings

lint-ss: bin/lint
	find . -type f \( -name "*.ss" -o -name "*.md" \) | xargs $<

lint-sh:
	find . -type f -name "*.sh" | xargs shellcheck

lint-ts:
	find . -type f -name "*.ts" | xargs deno lint --unstable

lint-headings:
	scripts/lint-headings.sh

spell: bin/spell
	$^ $(project_md) $(notes_src) $(sicp_src)

validate .PHONY: validate-vnu validate-links validate-other

validate-vnu:
	find docs -type f -name "*.html" \
		| xargs vnu --filterpattern $(validate_exceptions)

validate-links:
	{ echo $(project_md); find docs -type f -name "*.html"; } \
		| xargs scripts/check-links.py

validate-other:
	# Ensure special characters used for syntax highlighting are stripped.
	! grep -qRE '«|»|‹|›|```' docs

tools: $(tools)

$(c_tools): bin/%: tools/%.c | bin
	$(CC) $(CFLAGS) -o $@ $^

$(objc_tools): bin/%: tools/%.m | bin
	$(CC) $(CFLAGS) $(OBJCFLAGS) -o $@ $^

$(lua_c_tools): lib/%.so: tools/lua/%.zig | lib
	zig build-lib -O ReleaseSmall -dynamic -fsingle-threaded -fallow-shlib-undefined -femit-bin=$@ $^

bin lib:
	mkdir -p $@

clean:
	find src -type d -name compiled -exec rm -rf {} +
	find src -type f -name *.so -exec rm -f {} +
	rm -rf bin lib

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
