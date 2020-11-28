.PHONY: help lint test clean

help:
	@echo "Targets:"
	@echo "help   show this help message"
	@echo "lint   lint all Scheme files"
	@echo "test   run exercises in all Schemes"
	@echo "clean  remove compilation artifacts"
	@echo
	@echo "See also: make -C docs help"

lint:
	@echo TODO

test:
	./run.sh all

clean:
	find . -type d -name compiled -exec rm -rf {} +
