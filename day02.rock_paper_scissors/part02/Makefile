all: program

program: program.o
	g++ -nostartfiles -o program program.o

%.o: %.s
	as -o $@ $<

clean:
	rm *.o program
