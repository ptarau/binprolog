CALL makebpx.bat bp.exe -GA
copy /Y *.h ..\sdist
copy /Y *.c ..\sdist
del /Q ..\sdist\stub.c


