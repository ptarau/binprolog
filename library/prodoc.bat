set PROJ=%1
set BP_PATH=\tarau\BinProlog\library
bp -q3 -b0 -h40000 -t10000 -s10000 -a20 -d21 prodoc.pl "xref(%1),halt"
del /Q %PROJ%.pdf
latex %PROJ%.tex
bibtex %PROJ%
latex %PROJ%.tex
tth.exe <%PROJ%.tex >%PROJ%.html -L%PROJ% -a -e1
pdflatex %PROJ%.tex
del /Q %PROJ%.aux
del /Q %PROJ%.bbl
del /Q %PROJ%.log
del /Q %PROJ%.blg
del /Q %PROJ%.dvi
del /Q %PROJ%.tex
REM %PROJ%.html