.PHONY: all
all: test build run

GHC_OPTIONS = '-W -Werror=incomplete-patterns -Werror=incomplete-uni-patterns -Werror=incomplete-record-updates'


.PHONY: ghci
ghci:
	stack ghci --ghc-options $(GHC_OPTIONS)
	# then :load Main
	# stack ghci --ghc-options '-W'

.PHONY: test
test:
	stack test

.PHONY: build
build:
	stack build \
		--no-keep-going \
		--ghc-options $(GHC_OPTIONS)

.PHONY: run
run: build
	@echo "Running:"
	@stack exec wordle-solver-exe

.PHONY: clean
	stack clean
