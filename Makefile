SHELL  = /bin/bash
CC     = clang
CC_OPTS= -std=c11 -Wall -Wpedantic -Wextra

execs =

all: runtime.o $(execs)

pngs:
	for dotfile in $$(ls tests | grep .dot$$); do \
		dot -Tpng tests/$$dotfile > "$${dotfile%.dot}.png"; \
	done;
	for dotfile in $$(ls . | grep .dot$$); do \
		dot -Tpng $$dotfile > "$${dotfile%.dot}.png"; \
	done;

test: test1 | pngs

test1: runtime.o
	racket test.rkt

%.o: %.c
	$(CC) $^ -c -g -o $@ $(CC_OPTS)

%.o: %.s
	$(CC) $^ -c -g -o $@

%.exe: %.o runtime.o
	$(CC) $^ -o $@

clean:
	rm -f *.o
	rm -f $(execs)
	rm -f tests/*.s
	rm -f tests/*.png
	rm -f tests/*.dot
	rm -f *.png
	rm -f *.dot
	rm -f *.hi
