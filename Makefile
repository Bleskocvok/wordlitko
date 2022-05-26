
APP_SRC = main.py
EVAL_SRC = evaluation.py
SOLVER_SRC = solver.hs
SOLVER_GARBAGE = solver.hi solver.o


ifeq ($(OS),Windows_NT)
	WIN = true
endif

ifdef WSL
	WIN = true
endif


ifdef WIN
	DRIVER = ./chromedriver.exe
	SOLVER = ./solver.exe
	PYTHON = python3.exe
	GHC    = ghc.exe
else
	DRIVER = ./chromedriver
	SOLVER = ./solver
	PYTHON = python3
	GHC    = ghc
endif


all: $(SOLVER)

$(SOLVER): $(SOLVER_SRC)
	$(GHC) -O2 -o $@ $^

run: $(SOLVER)
	$(PYTHON) $(APP_SRC) "$(DRIVER)" "$(SOLVER)"

evaluate: $(SOLVER)
	$(PYTHON) $(EVAL_SRC) "data/answers.txt" "data/possible.txt" "$(SOLVER)"

clean:
	$(RM) $(SOLVER_GARBAGE)

distclean: clean
	$(RM) $(SOLVER)

.PHONY: all clean distclean run evaluate
