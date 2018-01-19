.PHONY: all build format

all: build

build:
	elm-make --yes

format:
	elm-format --yes src
