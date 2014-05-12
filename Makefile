TALK = ctc2014

${TALK}.pdf: ${TALK}.tex Stuff/header.tex Figs/cross1.pdf
	xelatex ${TALK}

Figs/cross1.pdf: R/plot_crosses.R
	cd R;R CMD BATCH plot_crosses.R

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

