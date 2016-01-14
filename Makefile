CC := gcc

execs = test.exe

all: runtime.o $(execs)

%.o: %.c
	$(CC) $^ -c -g -o $@

%.o: %.s
	$(CC) $^ -c -g -o $@

%.exe: %.o runtime.o
	$(CC) $^ -o $@

clean:
	rm -f *.o
	rm -f $(execs)
