.PHONY: all build doc


all: compile home

clean:
	stack clean

build:
	stack build

compile:
	stack exec Blog -- compile

home:
	mv public/* ~/siskam.link
	cp index.html public/
