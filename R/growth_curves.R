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

pdf("../Figs/growth1.pdf", height=6.5, width=10, pointsize=20)
par(bg=bg, fg="white", col="white", col.axis="white", col.lab="white")
par(las=1, mar=c(4.1, 4.1, 0.6, 3.1))
xat <- c(5, 10, 15)
yat <- seq(0, 40, by=5)
grayplot(0,0,type="n", xlim=c(1, 16), xaxs="i", ylim=c(0, ymax*1.02), yaxs="i",
         xlab="Week", ylab="Body weight (g)",
         xat=c(1,5,10,15), vlines=xat, vlines.col="gray70",vlines.lwd=3,
         yat=yat, hlines=yat)
col <- brocolors("crayons")[c("Tickle Me Pink", "Blue Gray", "Razzmatazz", "Midnight Blue", "Wild Strawberry", "Green Blue")]
colalpha <- colwalpha(col, alpha=0.3)
for(i in sample(nrow(parents))) {
  y <- parents[i,wkcol]
  x <- (1:16)[!is.na(y)]
  y <- y[!is.na(y)]
  lines(x, y, lwd=2, col=col[par_sex[i] + 2*(par_strain[i]-1)])
}
text(11.5, 12.5, "WSB", adj=c(0, 0.5), col="black")
text(6, 34, "Gough", adj=c(0, 1), col="gray40")
dev.off()    


pdf("../Figs/growth2.pdf", height=6.5, width=10, pointsize=20)
par(bg=bg, fg="white", col="white", col.axis="white", col.lab="white")
par(las=1, mar=c(4.1, 4.1, 0.6, 3.1))
xat <- c(5, 10, 15)
yat <- seq(0, 40, by=5)
grayplot(0,0,type="n", xlim=c(1, 16), xaxs="i", ylim=c(0, ymax*1.02), yaxs="i",
         xlab="Week", ylab="Body weight (g)",
         xat=c(1,5,10,15), vlines=xat, vlines.col="gray70",vlines.lwd=3,
         yat=yat, hlines=yat)
for(i in sample(which(f2$pheno$sex=="M"))) {
  y <- f2$pheno[i,1:16]
  x <- (1:16)[!is.na(y)]
  y <- y[!is.na(y)]
  lines(x, y, lwd=1, col=colalpha[2])
}
dev.off()    


pdf("../Figs/growth3.pdf", height=6.5, width=10, pointsize=20)
par(bg=bg, fg="white", col="white", col.axis="white", col.lab="white")
par(las=1, mar=c(4.1, 4.1, 0.6, 3.1))
xat <- c(5, 10, 15)
yat <- seq(0, 40, by=5)
grayplot(0,0,type="n", xlim=c(1, 16), xaxs="i", ylim=c(0, ymax*1.02), yaxs="i",
         xlab="Week", ylab="Body weight (g)",
         xat=c(1,5,10,15), vlines=xat, vlines.col="gray70",vlines.lwd=3,
         yat=yat, hlines=yat)
for(i in sample(which(f2$pheno$sex=="M"))) {
  y <- f2$pheno[i,1:16]
  x <- (1:16)[!is.na(y)]
  y <- y[!is.na(y)]
  lines(x, y, lwd=1, col=colalpha[2])
}
gmean <- colMeans(parents[par_strain==1 & par_sex==2,wkcol], na.rm=TRUE)
wmean <- colMeans(parents[par_strain==2 & par_sex==2,wkcol], na.rm=TRUE)
lines(1:16, gmean, col=col[6], lwd=2)
lines(1:16, wmean, col=col[4], lwd=2)
f2mean <- colMeans(f2$pheno[f2$pheno$sex=="M",1:16], na.rm=TRUE)
lines(1:16, f2mean, col=brocolors("crayons")["Navy Blue"], lwd=2)
axis(side=4, at=c(gmean[16], f2mean[16], wmean[16]), label=c("Gough", expression(F[2]), "WSB"),
     tick=FALSE, mgp=c(0, 0.3, 0))
dev.off()    


pdf("../Figs/growth4.pdf", height=6.5, width=10, pointsize=20)
par(bg=bg, fg="white", col="white", col.axis="white", col.lab="white")
par(las=1, mar=c(4.1, 4.1, 0.6, 3.1))
xat <- c(5, 10, 15)
yat <- seq(0, 40, by=5)
grayplot(0,0,type="n", xlim=c(1, 16), xaxs="i", ylim=c(0, ymax*1.02), yaxs="i",
         xlab="Week", ylab="Body weight (g)",
         xat=c(1,5,10,15), vlines=xat, vlines.col="gray70",vlines.lwd=3,
         yat=yat, hlines=yat)
for(i in sample(which(f2$pheno$sex=="F"))) {
  y <- f2$pheno[i,1:16]
  x <- (1:16)[!is.na(y)]
  y <- y[!is.na(y)]
  lines(x, y, lwd=1, col=colalpha[1])
}
gmean <- colMeans(parents[par_strain==1 & par_sex==1,wkcol], na.rm=TRUE)
wmean <- colMeans(parents[par_strain==2 & par_sex==1,wkcol], na.rm=TRUE)
lines(1:16, gmean, col=col[5], lwd=2)
lines(1:16, wmean, col=col[3], lwd=2)
f2mean <- colMeans(f2$pheno[f2$pheno$sex=="F",1:16], na.rm=TRUE)
lines(1:16, f2mean, col=brocolors("crayons")["Radical Red"], lwd=2)
axis(side=4, at=c(gmean[16], f2mean[16], wmean[16]-1), label=c("Gough", expression(F[2]), "WSB"),
     tick=FALSE, mgp=c(0, 0.3, 0))
dev.off()    
