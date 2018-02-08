.PHONY: all build format test verify-examples

all: build

build:
	elm-make --yes --warn --docs docs.json

format:
	elm-format --yes src tests

test:
	elm-test

verify-examples:
	elm-verify-examples
