del /Q *.obj
cl.exe -Fe%1 %2 %3 %4 %5 %7 %8 %9 -DTHREADS=1 -DVCC=1 -MT -GF -TC -W2 -O2x -Oy -DWIN32 -DNDEBUG -D_CONSOLE -D_WINDOWS -DMBCS -nologo ru.c sym.c load.c engine.c builtins.c dict.c io.c socket.c float.c debug.c gc.c term.c termStore.c main.c wam.c binpro.c c.c -link /DEFAULTLIB:wsock32 /DEFAULTLIB:advapi32
