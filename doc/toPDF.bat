set PROJ=%1
CALL clean.bat %PROJ%
del /Q %PROJ%.pdf
latex %PROJ%.tex
bibtex %PROJ%
latex %PROJ%.tex
pdflatex %PROJ%.tex
CALL clean.bat %PROJ%
REM %PROJ%.pdf

