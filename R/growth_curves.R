load("~/Projects/Payseur_Gough/DerivedData/goughF2_simple_v4.RData")
phe <- read.csv("~/Projects/Payseur_Gough/RawData/OtherData/Gough_phenotypes_012714.csv", as.is=TRUE, check.names=FALSE)
wkcol <- grep('^wk\\d+$', colnames(phe))
for(i in wkcol) phe[,i] <- as.numeric(phe[,i])
parents <- phe[phe$Direction=="GG" | phe$Direction=="WW",]
par_strain <- match(parents$Direction, c("GG", "WW"))
par_sex <- match(parents$Sex, c("F", "M"))
ymax <- max(c(unlist(phe[,wkcol]), unlist(f2$pheno[,1:16])), na.rm=TRUE)

library(broman)
bg <- brocolors("bg")
col <- brocolors("crayons")[c("Tickle Me Pink", "Blue Gray",
                              "Razzmatazz", "Midnight Blue",
                              "Wild Strawberry", "Green Blue",
                              "Razzle Dazzle Rose", "Blue Violet")]
colalpha <- colwalpha(col, alpha=0.3)

gmeanM <- colMeans(parents[par_strain==1 & par_sex==2,wkcol], na.rm=TRUE)
wmeanM <- colMeans(parents[par_strain==2 & par_sex==2,wkcol], na.rm=TRUE)
gmeanF <- colMeans(parents[par_strain==1 & par_sex==1,wkcol], na.rm=TRUE)
wmeanF <- colMeans(parents[par_strain==2 & par_sex==1,wkcol], na.rm=TRUE)
f2meanM <- colMeans(f2$pheno[f2$pheno$sex=="M",1:16], na.rm=TRUE)
f2meanF <- colMeans(f2$pheno[f2$pheno$sex=="F",1:16], na.rm=TRUE)

pdf("../Figs/growth1.pdf", height=5.5, width=10, pointsize=16)
par(bg=bg, fg="white", col="white", col.axis="white", col.lab="white", col.main="white")
par(las=1, mar=c(4.1, 4.1, 2.6, 0.6),
    mfrow=c(1,2))
xat <- c(5, 10, 15)
yat <- seq(0, 40, by=5)
grayplot(0,0,type="n", xlim=c(1, 16), xaxs="i", ylim=c(0, ymax*1.02), yaxs="i",
         xlab="Week", ylab="Body weight (g)",
         xat=c(1,5,10,15), vlines=xat, vlines.col="gray70",vlines.lwd=3,
         yat=yat, hlines=yat, main="Males")
for(i in sample(which(f2$pheno$sex=="M"))) {
  y <- f2$pheno[i,1:16]
  x <- (1:16)[!is.na(y)]
  y <- y[!is.na(y)]
  lines(x, y, lwd=1, col=colalpha[2])
}
lines(1:16, f2meanM, col=col[4], lwd=2)

grayplot(0,0,type="n", xlim=c(1, 16), xaxs="i", ylim=c(0, ymax*1.02), yaxs="i",
         xlab="Week", ylab="Body weight (g)",
         xat=c(1,5,10,15), vlines=xat, vlines.col="gray70",vlines.lwd=3,
         yat=yat, hlines=yat, main="Females")
for(i in sample(which(f2$pheno$sex=="F"))) {
  y <- f2$pheno[i,1:16]
  x <- (1:16)[!is.na(y)]
  y <- y[!is.na(y)]
  lines(x, y, lwd=1, col=colalpha[1])
}
lines(1:16, f2meanF, col=col[3], lwd=2)
dev.off()




pdf("../Figs/growth2.pdf", height=5.5, width=10, pointsize=16)
par(bg=bg, fg="white", col="white", col.axis="white", col.lab="white", col.main="white")
par(las=1, mar=c(4.1, 4.1, 2.6, 0.6),
    mfrow=c(1,2))
xat <- c(5, 10, 15)
yat <- seq(0, 40, by=5)
grayplot(0,0,type="n", xlim=c(1, 16), xaxs="i", ylim=c(0, ymax*1.02), yaxs="i",
         xlab="Week", ylab="Body weight (g)",
         xat=c(1,5,10,15), vlines=xat, vlines.col="gray70",vlines.lwd=3,
         yat=yat, hlines=yat, main="Males")
for(i in sample(which(f2$pheno$sex=="M"))) {
  y <- f2$pheno[i,1:16]
  x <- (1:16)[!is.na(y)]
  y <- y[!is.na(y)]
  lines(x, y, lwd=1, col=colalpha[2])
}
lines(1:16, f2meanF, col=col[3], lwd=2)
lines(1:16, f2meanM, col=col[4], lwd=2)

grayplot(0,0,type="n", xlim=c(1, 16), xaxs="i", ylim=c(0, ymax*1.02), yaxs="i",
         xlab="Week", ylab="Body weight (g)",
         xat=c(1,5,10,15), vlines=xat, vlines.col="gray70",vlines.lwd=3,
         yat=yat, hlines=yat, main="Females")
for(i in sample(which(f2$pheno$sex=="F"))) {
  y <- f2$pheno[i,1:16]
  x <- (1:16)[!is.na(y)]
  y <- y[!is.na(y)]
  lines(x, y, lwd=1, col=colalpha[1])
}
lines(1:16, f2meanM, col=col[4], lwd=2)
lines(1:16, f2meanF, col=col[3], lwd=2)
dev.off()


pdf("../Figs/growth3.pdf", height=5.5, width=10, pointsize=16)
par(bg=bg, fg="white", col="white", col.axis="white", col.lab="white", col.main="white")
par(las=1, mar=c(4.1, 4.1, 2.6, 0.6),
    mfrow=c(1,2))
xat <- c(5, 10, 15)
yat <- seq(0, 40, by=5)
grayplot(0,0,type="n", xlim=c(1, 16), xaxs="i", ylim=c(0, ymax*1.02), yaxs="i",
         xlab="Week", ylab="Body weight (g)",
         xat=c(1,5,10,15), vlines=xat, vlines.col="gray70",vlines.lwd=3,
         yat=yat, hlines=yat, main="Males")
for(i in sample(which(f2$pheno$sex=="M"))) {
  y <- f2$pheno[i,1:16]
  x <- (1:16)[!is.na(y)]
  y <- y[!is.na(y)]
  lines(x, y, lwd=1, col=colalpha[2])
}
lines(1:16, f2meanM, col=col[4], lwd=2)
lines(1:16, gmeanM, col=col[6], lwd=2)
lines(1:16, wmeanM, col=col[8], lwd=2)
text(9, 22.5, expression(F[2]), col=col[4], adj=c(0, 0.5))
text(8, 27.5, "Gough", col=col[6], adj=c(1, 0.5))
text(11, 13.5, "WSB", col=col[8], adj=c(0, 0.5))

grayplot(0,0,type="n", xlim=c(1, 16), xaxs="i", ylim=c(0, ymax*1.02), yaxs="i",
         xlab="Week", ylab="Body weight (g)",
         xat=c(1,5,10,15), vlines=xat, vlines.col="gray70",vlines.lwd=3,
         yat=yat, hlines=yat, main="Females")
for(i in sample(which(f2$pheno$sex=="F"))) {
  y <- f2$pheno[i,1:16]
  x <- (1:16)[!is.na(y)]
  y <- y[!is.na(y)]
  lines(x, y, lwd=1, col=colalpha[1])
}
lines(1:16, f2meanF, col=col[3], lwd=2)
lines(1:16, gmeanF, col=col[5], lwd=2)
lines(1:16, wmeanF, col=col[7], lwd=2)
text(9, 19, expression(F[2]), col=col[3], adj=c(0, 0.5))
text(7, 22.5, "Gough", col=col[5], adj=c(1, 0.5))
text(11, 12.5, "WSB", col=col[7], adj=c(0, 0.5))
dev.off()
