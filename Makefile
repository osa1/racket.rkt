CC := gcc

execs = test.exe

all: runtime.o $(execs)

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
