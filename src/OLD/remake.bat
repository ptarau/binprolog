del /Q wam.c
del /Q wam.h
del /Q binpro.c
del /Q binpro.h
del /Q *.obj
del /Q *.exe
del /Q *.dll
del /Q *.exp
del /Q *.lib
copy full.pro wam.pro
bp [remake]
CALL cboot.bat
REM copy bp.exe \bin
REM CALL MAKEBPX pbp.exe "-DPROF=3" "-DTRACE=1" "-DTRACE_EXEC=1"
del /Q ru.exe
del /Q *.obj
