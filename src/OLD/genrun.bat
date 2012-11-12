copy run.pro wam.pro
bp.exe call((cmake,halt))
@echo wam.c generated
CALL makebpr.bat
@echo bpr.exe generated
CALL makedllr.bat
copy full.pro wam.pro
del /Q wam.c
del /Q wam.h
del /Q binpro.c
del /Q binpro.h
