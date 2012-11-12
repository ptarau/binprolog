del /Q *.dll
del /Q *.exp
del /Q *.lib
link.exe -lib *.obj -out:..\lib\%1
