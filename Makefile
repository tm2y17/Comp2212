# 
# Rules for compiling and linking the typechecker/evaluator
#
# Type
#   make         to rebuild the executable files
#   make clean   to remove all intermediate and temporary files
#   

# Files that need to be generated from other files
DEPEND += SplTokens.hs SplGrammar.hs SplEval.hs

# When "make" is invoked with no arguments, we build an executable 
#  after building everything that it depends on
all: $(DEPEND) myinterpreter

# Build an executable for Toy interpreter
myinterpreter: $(DEPEND) Main.hs
	ghc -o myinterpreter Main.hs

# Generate ML files from a parser definition file
SplGrammar.hs : SplGrammar.y
	@rm -f SplGrammar.hs
	happy SplGrammar.y
	@chmod -w SplGrammar.hs

# Generate ML files from a lexer definition file
SplTokens.hs : SplTokens.x
	@rm -f SplTokens.hs
	alex SplTokens.x
	@chmod -w SplTokens.hs

# Clean up the directory
clean::
	rm -rf SplTokens.hs SplGrammar.hs *.hi *.o *.info myinterpreter

