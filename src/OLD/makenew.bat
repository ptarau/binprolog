mv -f wam.c wam_c.txt
mv -f wam.h wam_h.txt
mv -f binpro.c binpro_c.txt
mv -f binpro.h binpro_h.txt
gcc -DAIX -fomit-frame-pointer -g -o ru.exe *.c -lm
ru.exe remake
ru.exe "and(cboot,halt)"
mv -f stub.c stub.txt
gcc -DAIX -fomit-frame-pointer -g -O3 -c *.c -lm
gcc -fomit-frame-pointer -g -O3 -o bp.exe *.o -lm
gcc -DAIX -DNOMAIN -DEXTERNAL_TSTORE -fomit-frame-pointer -g -O3 -c builtins.c c.c -lm
ar -q libbps.a *.o
mv -f libbps.a ../lib
cp c_defs.h global.h ../lib
mv -f stub.txt stub.c
rm -f wam_c.txt
rm -f wam_h.txt
rm -f binpro_c.txt
rm -f binpro_h.txt
:end




