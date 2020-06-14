SCHEME ?= chez

main := main.scm

.PHONY: default help chez guile racket

default: $(SCHEME)

help:
	@echo "Targets:"
	@echo "default  build the $$SCHEME target (if not set, chez)"
	@echo "help     show this help message"
	@echo "chez     run with chez"
	@echo "guile    run with guile"
	@echo "racket   run with racket"

chez:
	chez --program $(main)

guile:
	guile --r6rs -L . -x .ss $(main)

racket:
	plt-r6rs ++path . $(main)
