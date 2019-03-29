.PHONY: build
build:
	stack build

.PHONY: test
test:
	stack test

.PHONY: install
install:
	stack install
