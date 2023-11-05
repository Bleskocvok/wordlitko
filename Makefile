
HSFLAGS ?= -O2

CXXFLAGS ?= -std=c++20 -Wall -Wextra -O3 -march=native

APP_SRC = main.py
EVAL_SRC = evaluation.py
CSOLVER_SRC = solver.cpp
HSOLVER_SRC = solver.hs
HSOLVER_GARBAGE = hsolve.hi hsolve.o
SEND_DC = send_dc.py

ANSWERS = data/answers.txt
WORDS = data/possible.txt
DATA = $(WORDS)

DRIVER ?= chrome

ifeq ($(OS),Windows_NT)
	WIN = true
endif

ifdef WIN
	CHROME_DRIVER = .\driver\chromedriver.exe
	FIREFOX_DRIVER = .\driver\geckodriver.exe
	HSOLVER = .\hsolve.exe
	CSOLVER = .\csolve.exe
	PYTHON = python3.exe
	GHC    = ghc.exe
else
	CHROME_DRIVER = ./driver/chromedriver
	FIREFOX_DRIVER = ./driver/geckodriver
	HSOLVER = ./hsolve
	CSOLVER = ./csolve
	PYTHON = python3
	GHC    = ghc
endif


all: $(HSOLVER)

$(HSOLVER): $(HSOLVER_SRC)
	$(GHC) $(HSFLAGS) -o $@ $^

$(CSOLVER): $(CSOLVER_SRC)
	$(CXX) $(CXXFLAGS) -o $@ $^


htime: $(HSOLVER)
	time -p $(HSOLVER) '' "$(DATA)" | tail -n5

ctime: $(HSOLVER)
	time -p $(HSOLVER) '' "$(DATA)" | tail -n5

solve: $(HSOLVER)
	export DRIVER=$(DRIVER); \
	export MOZ_HEADLESS=1; \
	export FIREFOX_DRIVER=$(FIREFOX_DRIVER); \
	export CHROME_DRIVER=$(CHROME_DRIVER); \
	$(PYTHON) $(APP_SRC) "$(HSOLVER)" "$(DATA)" | tee score

run: solve score
	python3 $(SEND_DC) score chan

evaluate: $(HSOLVER)
	time -p $(PYTHON) $(EVAL_SRC) "$(ANSWERS)" "$(WORDS)" "$(HSOLVER)"

cevaluate: $(CSOLVER)
	time -p $(PYTHON) $(EVAL_SRC) "$(ANSWERS)" "$(WORDS)" "$(CSOLVER)"

clean:
	$(RM) $(HSOLVER_GARBAGE)

distclean: clean
	$(RM) $(HSOLVER)


.PHONY: all clean distclean run solve evaluate cevaluate time
