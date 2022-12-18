all: solution

solution: solution.o
	g++ -pthread -o solution solution.o

%.o: %.s
	g++ -std=c++17 -Wall -Werror -pedantic -Wunused-variable -c $@ $<

clean:
	rm *.o solution
