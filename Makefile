.PHONY: all build doc


all: build compile sitemap home

clean:
	stack clean

build:
	stack build

compile:
	stack exec Blog -- compile

sitemap:
	ls public | grep html > public/sitemap.txt
	sed /^/s/^/'https\:\/\/siskam\.link\/'/ public/sitemap.txt > public/temp
	mv public/temp public/sitemap.txt

home:
	mv public/* ~/siskam.link
	cp res/404.html public/
