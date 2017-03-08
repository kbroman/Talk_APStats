# image of genotypes

load("~/Projects/Payseur_Gough/DerivedData/goughF2_simple_v4.RData")

library(qtl)
marker_subset <- NULL
for(i in names(f2$geno))
    marker_subset <- c(marker_subset,
                       pickMarkerSubset(pull.map(f2, chr=i)[[1]], 1))

color <- brocolors("crayons")[c("Black", "Jungle Green", "Dandelion", "Purple Mountain's Majesty")]

males <- which(f2$pheno$sex=="M")
females <- which(f2$pheno$sex=="F")
males <- males[order(growthSm[males,16])]
females <- females[order(growthSm[females,16])]

ind <- c(females[1:50], males[1:50], rev(rev(females)[1:50]), rev(rev(males)[1:50]))

png("../Figs/geno_image.png", height=650, width=1000, pointsize=22)
par(bg=bg, fg="white", col="white", col.axis="white", col.lab="white", col.main="white")
par(mar=c(5.1,4.1,3.6,0.6))
geno.image(pull.markers(f2, marker_subset)[,ind], col=color, main="", ylab="Mice")
mtext(side=3, "Chromosomes", line=2.2, cex=1.2)
dev.off()
