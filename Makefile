SHELL = bash
HASKELL_SRC=$(shell find src/ test/ -type f -name '*.hs')

.PHONY: format
format: $(HASKELL_SRC)
	fourmolu --mode inplace $^
