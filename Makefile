TALK = apstats2017

all: ${TALK}.pdf notes html

${TALK}.pdf: DerivedFiles/${TALK}.tex Stuff/header.tex Figs/cross1.pdf Figs/growth1.pdf Figs/rate1.pdf Figs/GI_v_W.pdf Figs/geno_image.png Figs/pheno_v_geno.pdf Figs/permtest_illustration.pdf Figs/permtest_hist.pdf
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

Figs/GI_v_W.pdf: R/GI_v_W.R
	cd R;R CMD BATCH GI_v_W.R

Figs/geno_image.png: R/geno_image.R
	cd R;R CMD BATCH geno_image.R

Figs/pheno_v_geno.pdf: R/pheno_v_geno.R
	cd R;R CMD BATCH pheno_v_geno.R

Figs/permtest_illustration.pdf: R/permtest_illustration.R
	cd R;R CMD BATCH permtest_illustration.R

Figs/permtest_hist.pdf: R/permtest_hist.R
	cd R;R CMD BATCH permtest_hist.R

notes: ${TALK}_withnotes.pdf
pdf: ${TALK}.pdf notes

${TALK}_withnotes.pdf: DerivedFiles/${TALK}_withnotes.tex Stuff/header.tex
	cd DerivedFiles;xelatex ${TALK}_withnotes
	cd DerivedFiles;pdfnup ${TALK}_withnotes.pdf --nup 1x2 --no-landscape --paper letterpaper --frame true --scale 0.9
	mv DerivedFiles/${TALK}_withnotes-nup.pdf ${TALK}_withnotes.pdf
	rm DerivedFiles/${TALK}_withnotes.pdf

DerivedFiles/${TALK}_withnotes.tex: DerivedFiles/${TALK}.tex Stuff/Ruby/createVersionWithNotes.rb
	Stuff/Ruby/createVersionWithNotes.rb DerivedFiles/${TALK}.tex DerivedFiles/${TALK}_withnotes.tex

html: iplot_bodyweight.html

iplot_bodyweight.html: R/iplot.R
	cd R;R CMD BATCH iplot.R

clean:
	rm DerivedFiles/*
	cd DerivedFiles;ln -s ../Figs;ln -s ../Stuff

web: iplot_bodyweight.html ${TALK}.pdf ${TALK}_withnotes.pdf
	scp iplot_bodyweight.html iplot_deriv_bodyweight.html ${TALK}.pdf ${TALK}_withnotes.pdf broman-10.biostat.wisc.edu:Website/presentations/APStats2017/
