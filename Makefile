TALK = ctc2014

${TALK}.pdf: DerivedFiles/${TALK}.tex Stuff/header.tex Figs/cross1.pdf Figs/growth1.pdf Figs/rate1.pdf
	cd DerivedFiles;xelatex ${TALK}
	mv DerivedFiles/${TALK}.pdf .

DerivedFiles/${TALK}.tex: ${TALK}.Rnw
	cd DerivedFiles;R -e 'library(knitr);knit("../${TALK}.Rnw")'

Figs/cross1.pdf: R/plot_crosses.R
	cd R;R CMD BATCH plot_crosses.R

Figs/growth1.pdf: R/growth_curves.R
	cd R;R CMD BATCH growth_curves.R

Figs/rate1.pdf: R/growth_rate.R
	cd R;R CMD BATCH growth_rate.R

notes: ${TALK}_withnotes.pdf
pdf: ${TALK}.pdf notes
all: ${TALK}.pdf notes web dropbox

${TALK}_withnotes.pdf: DerivedFiles/${TALK}_withnotes.tex Stuff/header.tex
	cd DerivedFiles;xelatex ${TALK}_withnotes
	cd DerivedFiles;pdfnup ${TALK}_withnotes.pdf --nup 1x2 --no-landscape --paper letterpaper --frame true --scale 0.9
	mv DerivedFiles/${TALK}_withnotes-nup.pdf ${TALK}_withnotes.pdf
	rm DerivedFiles/${TALK}_withnotes.pdf

DerivedFiles/${TALK}_withnotes.tex: DerivedFiles/${TALK}.tex Stuff/Ruby/createVersionWithNotes.rb
	Stuff/Ruby/createVersionWithNotes.rb DerivedFiles/${TALK}.tex DerivedFiles/${TALK}_withnotes.tex

clean:
	rm DerivedFiles/*
	cd DerivedFiles;ln -s ../Figs;ln -s ../Stuff

