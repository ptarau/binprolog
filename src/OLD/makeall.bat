CALL remake.bat
copy global.h ..\lib
copy c_defs.h ..\lib
copy c_defs.h ..\lib
copy c.c ..\lib
copy c.obj ..\lib
copy bp.exe ..\bin
call makedll.bat
copy bp_lib.* ..\BP_DLL
CALL makelib.bat bp.lib
CALL genrun.bat
copy bpr.exe ..\bin
copy bpr_lib.* ..\BP_DLL
CALL makelib.bat bpr.lib
CALL cboot.bat
del /Q *.obj
del /Q *.exe
del /Q *.dll
del /Q *.lib
del /Q *.exp

