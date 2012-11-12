del /Q *.dll
del /Q *.exp
del /Q *.lib
CALL makebpx.bat %1 -D"VCCDLL=1" -LD -Gd -D_USRDLL
