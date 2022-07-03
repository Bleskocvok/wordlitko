
HSFLAGS ?= -O2

APP_SRC = main.py
EVAL_SRC = evaluation.py
SOLVER_SRC = solver.hs
SOLVER_GARBAGE = solver.hi solver.o

ANSWERS = data/answers.txt
WORDS = data/possible.txt
DATA = $(WORDS)
CACHE = .solver_cache


ifeq ($(OS),Windows_NT)
	WIN = true
endif

# # doesnt really work as intended
# ifdef WSL
# 	WIN = true
# endif


ifdef WIN
	DRIVER = .\driver\chromedriver.exe
	SOLVER = .\solver.exe
	PYTHON = python3.exe
	GHC    = ghc.exe
else
	DRIVER = ./driver/chromedriver
	SOLVER = ./solver
	PYTHON = python3
	GHC    = ghc
endif


all: $(SOLVER)

$(SOLVER): $(SOLVER_SRC)
	$(GHC) $(HSFLAGS) -o $@ $^


time: $(SOLVER)
	time -p $(SOLVER) '' "$(DATA)" | tail -n5

run: $(SOLVER) $(CACHE)
	$(PYTHON) $(APP_SRC) "$(DRIVER)" "$(SOLVER)" "$(DATA)"

evaluate: $(SOLVER)
	time -p $(PYTHON) $(EVAL_SRC) "$(ANSWERS)" "$(WORDS)" "$(SOLVER)"

$(CACHE): $(SOLVER)
	$(SOLVER) '....' "$(WORDS)" > "$(CACHE)"
	echo done


clean:
	$(RM) $(SOLVER_GARBAGE) $(CACHE)

distclean: clean
	$(RM) $(SOLVER)


.PHONY: all clean distclean run evaluate time
