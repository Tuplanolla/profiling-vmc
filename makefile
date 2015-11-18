build: VMC.pdf

clean:
	$(RM) VMC.aux VMC.log VMC.out VMC.pdf VMC.ptb VMC.tex

%.pdf: %.lhs.tex
	pdflatex -jobname $* $<

%.lhs.tex: %.lhs
	lhs2TeX -o $@ $<
