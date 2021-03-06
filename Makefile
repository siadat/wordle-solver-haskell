.PHONY: all
all: test build

GHC_OPTIONS = '-W -Werror=incomplete-patterns -Werror=incomplete-uni-patterns -Werror=incomplete-record-updates'

words.txt:
	curl https://gist.githubusercontent.com/cfreshman/a03ef2cba789d8cf00c08f767e0fad7b/raw/5d752e5f0702da315298a6bb5a771586d6ff445c/wordle-answers-alphabetical.txt > words.txt

.PHONY: ghci
ghci:
	stack ghci --ghc-options $(GHC_OPTIONS)
	# then :load Main
	# stack ghci --ghc-options '-W'

.PHONY: test
test:
	stack test
	stack exec hlint > /dev/null || stack install hlint
	stack exec hlint src -- --report=report.html

.PHONY: build
build:
	@stack build \
		--no-keep-going \
		--ghc-options $(GHC_OPTIONS)

.PHONY: run
run: build words.txt
	@stack exec wordle-solver-exe

.PHONY: clean
clean:
	stack clean
	rm words.txt
