execpath := $(shell cabal list-bin zwirn-loom)
start:
	cabal build
	-rm -r build
	mkdir build
	cp $(execpath) build
	cp -r static build
	npm start
