bg <- brocolors("bg")

file <- "perms.RData"
if(file.exists(file)) {
    load(file)
} else {
    load("~/Projects/Payseur_Gough/DerivedData/goughF2_simple_v4.RData")
    phe <- growthSm[,5,drop=FALSE]

    marker_subset <- NULL
    for(i in names(f2$geno))
        marker_subset <- c(marker_subset,
                           pickMarkerSubset(pull.map(f2, chr=i)[[1]], 0.5))

    f2 <- pull.markers(f2, marker_subset)

    # permutation test just on the autosomes
    f2 <- calc.genoprob(f2["-X",], step=1, stepwidth="max", err=0.002, map.function="c-f")
    f2$pheno <- phe
    operm <- scanone(f2, method="hk", n.perm=10000, n.cluster=parallel::detectCores())
    save(operm, file=file)
}


pdf(file="../Figs/permtest_hist.pdf", width=9, height=5.5, pointsize=14)
par(fg="white",col="white",col.axis="white",col.lab="white",
    bg=bg, bty="n")

par(mar=c(5.1, 4.1, 1.1, 1.1), las=1)

hist(unclass(operm), breaks=sqrt(10000)*2, main="", xlab="Maximum LOD score")

dev.off()
