HSLEX := hslex
HSYACC := hsyacc
MHC := mhc

.PHONY: all
all:	examples/lambda/Lexing.hs \
	examples/lambda/Parsing.hs \
	Data/JSON/Lexing.hs \
	Data/JSON/Parsing.hs \
	Data/Scheme/Lexing.hs \
	Data/Scheme/Parsing.hs \
	Language/HsLex/Lexing.hs \
	Language/HsLex/Parsing.hs \
	Language/HsYacc/Lexing.hs \
	Language/HsYacc/Parsing.hs \
	Language/Haskell2010/Lexing.hs \
	Language/Haskell2010/Parsing.hs

%/Lexing.hs: %/Lexing.hsl
	$(HSLEX) < $< > $@

%/Parsing.hs: %/Parsing.hsy
	$(HSYACC) < $< > $@
