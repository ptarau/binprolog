#PROJ=art
#PROJ=abs
MAKEPROG=nmake
it:	all

one:
	$(MAKEPROG) ps html pdf PROJ=user

all: help 
	@echo USE $(MAKEPROG) !!!
	$(MAKEPROG) ps html pdf PROJ=user
	$(MAKEPROG) ps html pdf PROJ=advanced
	$(MAKEPROG) ps html pdf PROJ=interface
	$(MAKEPROG) ps html pdf PROJ=internet
	$(MAKEPROG) ps html pdf PROJ=crossref
	@echo gzip *.ps
	@echo mv -f *.ps.gz *.html /bp_doc
	$(MAKEPROG) clean

help:
	bp helpgen.pro
	rm -f help.txt

htmls:
	$(MAKEPROG) html PROJ=user
	$(MAKEPROG) html PROJ=advanced
	$(MAKEPROG) html PROJ=interface
	$(MAKEPROG) html PROJ=internet
	$(MAKEPROG) html PROJ=crossref
	@echo TYPE mv -f *.html c:/bp_doc

pdfs:
	$(MAKEPROG) pdf PROJ=user
	$(MAKEPROG) pdf PROJ=advanced
	$(MAKEPROG) pdf PROJ=interface
	$(MAKEPROG) pdf PROJ=internet
	$(MAKEPROG) pdf PROJ=crossref
	@echo TYPE mv -f *.pdf c:/bp_doc


$(PROJ).dvi:	$(PROJ).bbl $(PROJ).blg $(PROJ).aux $(PROJ).tex 
		latex $(PROJ)
		
bib $(PROJ).bbl $(PROJ).blg $(PROJ).aux: $(PROJ).tex
		latex $(PROJ)
		bibtex $(PROJ)
		latex $(PROJ)

$(PROJ).tex:
		latex $(PROJ)

$(PROJ).ps:		$(PROJ).dvi
#		dvips -f $(PROJ) >$(PROJ).ps
		dvips $(PROJ)

dvi:		$(PROJ).dvi
		yap $(PROJ).dvi

ps:		$(PROJ).ps

psview:
		#ghostview -swap -magstep 0 $(PROJ).ps
		#gw $(PROJ).ps
		#pageview $(PROJ).ps

txt:		$(PROJ).txt

$(PROJ).txt:
		dvi2tty -l -w112 $(PROJ).dvi > $(PROJ).txt

pdf $(PROJ).pdf: $(PROJ).tex
		toPDF.bat $(PROJ)

html $(PROJ).html: $(PROJ).tex
		tth.exe <$(PROJ).tex >$(PROJ).html -L$(PROJ) -a -e1

preds preds.tex: ../src/headers.pl
		bp -h40000 -s2000 -t2000 -b40000 preds.pro
		
		
oldhtml:	art.tex preds.tex
		rm -f -r html
		latex2html -split 2 -link 2 -contents_in_navigation -show_section_numbers -bottom_navigation art.tex
		mv art html
		(cd html; ln -s ../icons .)
		(cd html; perl -n -i.bak -e "s{/opt/tools/latex2html-95.3/}{}g;print;" *.html)
		(cd html; rm -f *.bak)


print:		art.ps
		lpr art.ps

see:		art.tex
		dvi2tty -l -w112 art

wc:
		dvi2tty -l -w132 art | wc

clean:
		rm -f crossref.ps *.aux *.blg *.log *.dlog *.bbl *.dvi

realclean:	clean
		rm -f *.bbl *.dvi *.ps *.html

