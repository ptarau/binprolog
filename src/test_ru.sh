cp defs.ok defs.h
cp wam.ok wam.bp
gcc -DW64 -DTHREADS=1 -m64   -lpthread -o boo *.c 