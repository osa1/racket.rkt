SHELL := /bin/bash
CC := gcc

execs = test.exe

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

runtime.o: public/runtime.c
	$(CC) $^ -c -g -o $@

%.o: %.c
	$(CC) $^ -c -g -o $@

%.o: %.s
	$(CC) $^ -c -g -o $@

%.exe: %.o runtime.o
	$(CC) $^ -o $@

clean:
	rm -f *.o
	rm -f $(execs)
	rm -r tests/*.s
	rm -f tests/*.png
	rm -f tests/*.dot
	rm -f *.png
	rm -f *.dot
