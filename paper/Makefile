main.pdf:
	pdflatex main

bib:
	pdflatex main; bibtex main

.PHONY: main.pdf bib

clean:
	rm -f *~ {main,lei,roman}.{pdf,log,aux,bbl,blg,dvi}
