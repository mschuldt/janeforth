
#BUILD_ID_NONE := -Wl,--build-id=none 
BUILD_ID_NONE := 

SHELL	:= /bin/bash

all:	janeforth

janeforth:  janeforth.S
	gcc -m32 -nostdlib -static $(BUILD_ID_NONE) -o  $@ $< -o janeforth-core
	./make-janeforth-script.sh

run:
	cat janeforth.f $(PROG) - | ./janeforth

clean:
	rm -f janeforth janeforth-core perf_dupdrop *~ core .test_*

install:
	cp janeforth /bin/

# Tests.

TESTS	:= $(patsubst %.f,%.test,$(wildcard test_*.f))

test check: $(TESTS)

test_%.test: test_%.f janeforth-core
	@echo -n "$< ... "
	@rm -f .$@
	@cat <(echo ': TEST-MODE ;') janeforth.f $< <(echo 'TEST') | \
	  ./janeforth-core 2>&1 | \
	  sed 's/DSP=[0-9]*//g' > .$@
	@diff -u .$@ $<.out
	@rm -f .$@
	@echo "ok"

# Performance.

perf_dupdrop: perf_dupdrop.c
	gcc -O3 -Wall -Werror -o $@ $<

run_perf_dupdrop: janeforth
	cat <(echo ': TEST-MODE ;') janeforth.f perf_dupdrop.f | ./janeforth-core

.SUFFIXES: .f .test
.PHONY: test check run run_perf_dupdrop
