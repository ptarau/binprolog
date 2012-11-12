cd src
CALL makeall.bat
cd ..
call binbuild.bat
cd ..\csocks
CALL winmake.bat
cd ..
@echo DONE!
