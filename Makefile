.PHONY: all build format

all: build

build:
	elm-make --yes --warn --docs docs.json

format:
	elm-format --yes src tests

test:
	elm-test
