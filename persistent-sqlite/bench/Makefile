
.DEFAULT: benchmark

benchmark : select.c
	gcc -Wall -Werror -O2 -o benchmark select.c -lpthread -ldl

run : benchmark
	./benchmark
