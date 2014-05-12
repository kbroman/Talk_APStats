TALK = ctc2014

${TALK}.pdf: ${TALK}.tex Stuff/header.tex Figs/cross1.pdf Figs/growth1.pdf Figs/rate1.pdf
	xelatex ${TALK}

${TALK}.tex: ${TALK}.Rnw
	R -e 'library(knitr);knit("${TALK}.Rnw")'

Figs/cross1.pdf: R/plot_crosses.R
	cd R;R CMD BATCH plot_crosses.R

Figs/growth1.pdf: R/growth_curves.R
	cd R;R CMD BATCH growth_curves.R

Figs/rate1.pdf: R/growth_rate.R
	cd R;R CMD BATCH growth_rate.R

notes: ${TALK}_withnotes.pdf
pdf: ${TALK}.pdf notes
all: ${TALK}.pdf notes web dropbox

${TALK}_withnotes.pdf: ${TALK}_withnotes.tex Stuff/header.tex
	xelatex ${TALK}_withnotes
	pdfnup ${TALK}_withnotes.pdf --nup 1x2 --no-landscape --paper letterpaper --frame true --scale 0.9
	mv ${TALK}_withnotes-nup.pdf ${TALK}_withnotes.pdf

${TALK}_withnotes.tex: ${TALK}.tex Stuff/Ruby/createVersionWithNotes.rb
	Stuff/Ruby/createVersionWithNotes.rb ${TALK}.tex ${TALK}_withnotes.tex

clean:
	rm *.aux *.log *.nav *.out *.snm *.toc *.vrb

