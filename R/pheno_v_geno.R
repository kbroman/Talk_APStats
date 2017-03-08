library(broman)

color <- brocolors("crayons")[c("Jungle Green", "Dandelion", "Purple Mountain's Majesty")]

markers <- c("UNC3333536", "UNC14857054", "UNC18857464")
chr <- c(2, 8, 10)

file <- "phe_v_gen.RData"
if(file.exists(file)) {
    load(file)
} else {
    load("~/Projects/Payseur_Gough/DerivedData/goughF2_simple_v4.RData")
    phe <- growthSm[,5,drop=FALSE]
    f2 <- fill.geno(f2[chr,], err=0.002, map.function="c-f", method="argmax")
    g <- pull.geno(f2)[,markers]
    g <- matrix(c("GG", "GW", "WW")[g], ncol=3)
    save(g, phe, file=file)
}

bg <- brocolors("bg")

pdf("../Figs/pheno_v_geno.pdf", height=5.5, width=10, pointsize=14)
yli <- range(phe, na.rm=TRUE)
par(bg=bg, fg="white", col="white", col.axis="white", col.lab="white", col.main="white")
par(mfrow=c(1,3), las=1, mar=c(4.1, 4.1, 2.6, 0.6))
for(i in 1:3) {
    col <- color[match(g[,i], c("GG", "GW", "WW"))]
    dotplot(g[,i], phe, jiggle=jiggle(g[,i], phe, hnum=100, vnum=80),
            ylim=yli, main=paste0(markers[i], " (Chr ", chr[i], ")"),
            col="black", bg=col, cex=0.8,
            ylab="Body weight (g) at 5 weeks",
            xlab="Genotype")
    me <- tapply(phe, g[,i], mean, na.rm=TRUE)
    segments(1:3-0.2, me, 1:3+0.2, me, lwd=3, col="violetred")
}
dev.off()
