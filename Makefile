.PHONY: all build doc


all: build compile home

clean:
	stack clean

build:
	stack build

compile:
	stack exec Blog -- compile

home:
	mv public/* ~/siskam.link
	cp 404.html public/
