SRCS := $(wildcard *.qmd)

diff: diff.pdf

diff.pdf: diff.tex
	pdflatex $<

diff.tex: A-utility-based-approach-to-modeling-systemic-resilience-of-highway-networks-with-an-application-in-Utah.tex
	latexdiff resilience_submitted.tex $< > $@

A-utility-based-approach-to-modeling-systemic-resilience-of-highway-networks-with-an-application-in-Utah.tex: $(SRCS)
	quarto render --to asce.pdf