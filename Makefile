EXE := $(shell cabal exec which maelstromHs)

build: hpack
	cabal --ghc-options='${GHC_OPTIONS}' build

hpack:
	hpack .

test: hpack
	cabal --ghc-options='${GHC_OPTIONS}' test

run: hpack
	cabal --ghc-options='${GHC_OPTIONS}' run

format-haskell:
	find app/ src/ test/ -name "*.hs" -exec fourmolu -i {} +

format-nix:
	alejandra --quiet .

format: format-nix format-haskell

ghcid: hpack
	ghcid -c "cabal --ghc-options='${GHC_OPTIONS}' repl"

hlint: hpack
	hlint .

maelstrom: build
	maelstrom test \
		-w echo --bin $(EXE) \
		--nodes n1 --time-limit 10 --log-stderr

.PHONY: build hpack test run format-haskell format-nix format ghcid hlint maelstrom
