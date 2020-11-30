CFLAGS := -std=c11 -W -Wall -O3

.PHONY: help lint test clean

help:
	@echo "Targets:"
	@echo "help   show this help message"
	@echo "lint   lint all Scheme files"
	@echo "test   run exercises in all Schemes"
	@echo "clean  remove compilation artifacts"
	@echo
	@echo "See also: make -C docs help"

lint: linter
	@find . -type f -name "*.ss" -o -name "*.md" -print0 | xargs -0 ./$<

linter: linter.c
	$(CC) $(CFLAGS) -o $@ $^

test:
	./run.sh all

clean:
	find . -type d -name compiled -exec rm -rf {} +
