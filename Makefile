.PHONY: build
build:
	stack build

.PHONY: test
test:
	stack test

.PHONY: install
install:
	stack install

.PHONY: build/linux
build/linux:
	docker run -it -v `pwd`:/tmp/hoge hoge /bin/bash -c "cd /tmp/hoge; stack build"
