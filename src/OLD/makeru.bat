del /Q *.obj
del /Q wam.c
del /Q wam.h
del /Q binpro.c
del /Q binpro.h
cl.exe -Feru.exe -DTHREADS=1 -DVCC=1 -MT -GF -TC -W2 -O2x -DWIN32 -DNDEBUG -D_CONSOLE -D_WINDOWS -DMBCS -nologo ru.c sym.c load.c engine.c builtins.c dict.c io.c socket.c float.c debug.c gc.c term.c termStore.c main.c stub.c c.c -link /DEFAULTLIB:wsock32 /DEFAULTLIB:advapi32
