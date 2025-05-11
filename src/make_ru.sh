cp defs.ok defs.h
cp prof.ok prof.h
cp wam.ok wam.bp
gcc -std=c11 -o ru *.c -lm