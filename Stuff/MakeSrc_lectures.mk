# Common portion of the Makefile for each lecture:
# everything except the definition of LEC
#   (Thanks, Jianan!)

${LEC}.pdf: ${LEC}.tex Stuff/header.tex
	xelatex ${LEC}

notes: ${LEC}_withnotes.pdf
pdf: ${LEC}.pdf notes
all: ${LEC}.pdf notes web dropbox

${LEC}_withnotes.pdf: ${LEC}_withnotes.tex Stuff/header.tex
	xelatex ${LEC}_withnotes
	pdfnup ${LEC}_withnotes.pdf --nup 1x2 --no-landscape --paper letterpaper --frame true --scale 0.9
	mv ${LEC}_withnotes-nup.pdf ${LEC}_withnotes.pdf

${LEC}_withnotes.tex: ${LEC}.tex Stuff/Ruby/createVersionWithNotes.rb
	Stuff/Ruby/createVersionWithNotes.rb ${LEC}.tex ${LEC}_withnotes.tex

clean:
	rm *.aux *.log *.nav *.out *.snm *.toc *.vrb
