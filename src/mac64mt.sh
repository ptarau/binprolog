echo ----------------------
echo generates BinProlog

mv -f wam.c wam_c.txt
mv -f wam.h wam_h.txt
mv -f binpro.c binpro_c.txt
mv -f binpro.h binpro_h.txt
gcc -o ru *.c 
./ru remake
./ru "and(cboot,halt)"
mv -f stub.c stub.txt

echo makes static library in ../lib
gcc -DW64 -DTHREADS=1 -m64 -arch x86_64 -g3 -O3 -Wall -fomit-frame-pointer -fmessage-length=0 -lpthread -c *.c 
gcc -DW64 -DTHREADS=1 -m64 -arch x86_64 -g3 -O3  -Wall -fomit-frame-pointer -fmessage-length=0 -lpthread -o bp *.o 

echo ---------------------
echo calls ar
ar -q libbps.a *.o
mv -f bp ../bin
mv -f libbps.a ../lib

#cp c_defs.h global.h ../lib

echo ------------------------
echo clean up
mv -f stub.txt stub.c
rm -f wam_c.txt
rm -f wam_h.txt
rm -f binpro_c.txt
rm -f binpro_h.txt
rm -f *.o
rm -f *.a
rm -f *.so



