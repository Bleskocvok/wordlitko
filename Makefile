
HSFLAGS ?= -O2

CXXFLAGS ?= -std=c++20 -Wall -Wextra -O3 -march=native

APP_SRC = main.py
EVAL_SRC = evaluation.py
SOLVER_SRC = solver.hs
SOLVER_GARBAGE = solver.hi solver.o
SEND_DC = send_dc.py

ANSWERS = data/answers.txt
WORDS = data/possible.txt
DATA = $(WORDS)
CACHE = .solver_cache

DRIVER ?= chrome

ifeq ($(OS),Windows_NT)
	WIN = true
endif

ifdef WIN
	CHROME_DRIVER = .\driver\chromedriver.exe
	FIREFOX_DRIVER = .\driver\geckodriver.exe
	SOLVER = .\solver.exe
	PYTHON = python3.exe
	GHC    = ghc.exe
else
	CHROME_DRIVER = ./driver/chromedriver
	FIREFOX_DRIVER = ./driver/geckodriver
	SOLVER = ./solver
	PYTHON = python3
	GHC    = ghc
endif


all: $(SOLVER)

$(SOLVER): $(SOLVER_SRC)
	$(GHC) $(HSFLAGS) -o $@ $^


time: $(SOLVER)
	time -p $(SOLVER) '' "$(DATA)" | tail -n5

solve: $(SOLVER) $(CACHE)
	export DRIVER=$(DRIVER); \
	export MOZ_HEADLESS=0; \
	export FIREFOX_DRIVER=$(FIREFOX_DRIVER); \
	export CHROME_DRIVER=$(CHROME_DRIVER); \
	$(PYTHON) $(APP_SRC) "$(SOLVER)" "$(DATA)" | tee score

csolve: solver.cpp
	$(CXX) $(CXXFLAGS) -o $@ $^

run: solve score
	python3 $(SEND_DC) score chan

evaluate: $(SOLVER)
	time -p $(PYTHON) $(EVAL_SRC) "$(ANSWERS)" "$(WORDS)" "$(SOLVER)"

cevaluate: $(SOLVER)
	time -p $(PYTHON) $(EVAL_SRC) "$(ANSWERS)" "$(WORDS)" "./csolve"

$(CACHE): $(SOLVER)
	$(SOLVER) '....' "$(WORDS)" > "$(CACHE)"
	@echo done


clean:
	$(RM) $(SOLVER_GARBAGE) $(CACHE)

distclean: clean
	$(RM) $(SOLVER)


.PHONY: all clean distclean run solve evaluate cevaluate time
