load("~/Projects/Payseur_Gough/DerivedData/goughF2_simple_v4.RData")
yli <- range(growthDeriv, na.rm=TRUE)*1.02

library(broman)
bg <- brocolors("bg")

library(pspline)
phe <- read.csv("~/Projects/Payseur_Gough/RawData/OtherData/Gough_phenotypes_012714.csv", as.is=TRUE, check.names=FALSE)
wkcol <- grep('^wk\\d+$', colnames(phe))
for(i in wkcol) phe[,i] <- as.numeric(phe[,i])
parents <- phe[phe$Direction=="GG" | phe$Direction=="WW",]
par_strain <- match(parents$Direction, c("GG", "WW"))
par_sex <- match(parents$Sex, c("F", "M"))

ndf <- 7
spl <- apply(parents[,wkcol], 1, function(a,b) smooth.Pspline(b[!is.na(a)], a[!is.na(a)], method=2, df=ndf), 1:16)
par_growthSm <- t(vapply(spl, function(a,b) predict(a, b), rep(0.5, 16), 1:16))
par_growthDeriv <- t(vapply(spl, function(a,b) predict(a, b, nderiv=1), rep(0.5, 16), 1:16))

# do it again backwards, to figure out other values to make missing
#   smooth.Pspline doesn't extrapolate at the beginning but does at the end
#   I don't want to extrapolate at all, just interpolate
spl2 <- apply(parents[,wkcol], 1, function(a,b) smooth.Pspline(rev(b[!is.na(a)]), rev(a[!is.na(a)]), method=2, df=ndf), rev(1:16))
par_growthSm2 <- t(vapply(spl2, function(a,b) predict(a, b), rep(0.5, 16), 1:16))[,16:1]
par_growthSm[is.na(par_growthSm2)] <- par_growthDeriv[is.na(par_growthSm2)] <- NA
colnames(par_growthSm) <- colnames(par_growthDeriv) <- as.character(1:16)


col <- brocolors("crayons")[c("Tickle Me Pink", "Blue Gray",
                              "Razzmatazz", "Midnight Blue",
                              "Wild Strawberry", "Green Blue",
                              "Razzle Dazzle Rose", "Blue Violet")]
colalpha <- colwalpha(col, alpha=0.3)


gmeanM <- colMeans(par_growthDeriv[par_strain==1 & par_sex==2,], na.rm=TRUE)
wmeanM <- colMeans(par_growthDeriv[par_strain==2 & par_sex==2,], na.rm=TRUE)
gmeanF <- colMeans(par_growthDeriv[par_strain==1 & par_sex==1,], na.rm=TRUE)
wmeanF <- colMeans(par_growthDeriv[par_strain==2 & par_sex==1,], na.rm=TRUE)
f2meanM <- colMeans(growthDeriv[f2$pheno$sex=="M",], na.rm=TRUE)
f2meanF <- colMeans(growthDeriv[f2$pheno$sex=="F",], na.rm=TRUE)

pdf("../Figs/rate1.pdf", height=5.5, width=10, pointsize=16)
par(bg=bg, fg="white", col="white", col.axis="white", col.lab="white", col.main="white")
par(las=1, mar=c(4.1, 4.1, 2.6, 0.6), mfrow=c(1,2))
xat <- c(5, 10, 15)
yat <- seq(-2, 5, by=1)
grayplot(0,0,type="n", xlim=c(1, 16), xaxs="i", ylim=yli, yaxs="i",
         xlab="Week", ylab="Growth rate (g/week)",
         xat=c(1,5,10,15), vlines=xat, vlines.col="gray70",vlines.lwd=3,
         yat=yat, hlines=yat, main="Males")
for(i in sample(which(f2$pheno$sex=="M"))) {
  y <- growthDeriv[i,1:16]
  x <- (1:16)[!is.na(y)]
  y <- y[!is.na(y)]
  lines(x, y, lwd=1, col=colalpha[2])
}
lines(1:16, f2meanM, col=col[4], lwd=2)
grayplot(0,0,type="n", xlim=c(1, 16), xaxs="i", ylim=yli, yaxs="i",
         xlab="Week", ylab="Growth rate (g/week)",
         xat=c(1,5,10,15), vlines=xat, vlines.col="gray70",vlines.lwd=3,
         yat=yat, hlines=yat, main="Females")
for(i in sample(which(f2$pheno$sex=="F"))) {
  y <- growthDeriv[i,1:16]
  x <- (1:16)[!is.na(y)]
  y <- y[!is.na(y)]
  lines(x, y, lwd=1, col=colalpha[1])
}
lines(1:16, f2meanF, col=col[3], lwd=2)
dev.off()


pdf("../Figs/rate2.pdf", height=5.5, width=10, pointsize=16)
par(bg=bg, fg="white", col="white", col.axis="white", col.lab="white", col.main="white")
par(las=1, mar=c(4.1, 4.1, 2.6, 0.6), mfrow=c(1,2))
xat <- c(5, 10, 15)
yat <- seq(-2, 5, by=1)
grayplot(0,0,type="n", xlim=c(1, 16), xaxs="i", ylim=yli, yaxs="i",
         xlab="Week", ylab="Growth rate (g/week)",
         xat=c(1,5,10,15), vlines=xat, vlines.col="gray70",vlines.lwd=3,
         yat=yat, hlines=yat, main="Males")
for(i in sample(which(f2$pheno$sex=="M"))) {
  y <- growthDeriv[i,1:16]
  x <- (1:16)[!is.na(y)]
  y <- y[!is.na(y)]
  lines(x, y, lwd=1, col=colalpha[2])
}
lines(1:16, f2meanF, col=col[3], lwd=2)
lines(1:16, f2meanM, col=col[4], lwd=2)
grayplot(0,0,type="n", xlim=c(1, 16), xaxs="i", ylim=yli, yaxs="i",
         xlab="Week", ylab="Growth rate (g/week)",
         xat=c(1,5,10,15), vlines=xat, vlines.col="gray70",vlines.lwd=3,
         yat=yat, hlines=yat, main="Females")
for(i in sample(which(f2$pheno$sex=="F"))) {
  y <- growthDeriv[i,1:16]
  x <- (1:16)[!is.na(y)]
  y <- y[!is.na(y)]
  lines(x, y, lwd=1, col=colalpha[1])
}
lines(1:16, f2meanM, col=col[4], lwd=2)
lines(1:16, f2meanF, col=col[3], lwd=2)
dev.off()


pdf("../Figs/rate3.pdf", height=5.5, width=10, pointsize=16)
par(bg=bg, fg="white", col="white", col.axis="white", col.lab="white", col.main="white")
par(las=1, mar=c(4.1, 4.1, 2.6, 0.6), mfrow=c(1,2))
xat <- c(5, 10, 15)
yat <- seq(-2, 5, by=1)
grayplot(0,0,type="n", xlim=c(1, 16), xaxs="i", ylim=yli, yaxs="i",
         xlab="Week", ylab="Growth rate (g/week)",
         xat=c(1,5,10,15), vlines=xat, vlines.col="gray70",vlines.lwd=3,
         yat=yat, hlines=yat, main="Males")
for(i in sample(which(f2$pheno$sex=="M"))) {
  y <- growthDeriv[i,1:16]
  x <- (1:16)[!is.na(y)]
  y <- y[!is.na(y)]
  lines(x, y, lwd=1, col=colalpha[2])
}
lines(1:16, f2meanM, col=col[4], lwd=2)
lines(1:16, gmeanM, col=col[6], lwd=2)
lines(1:16, wmeanM, col=col[8], lwd=2)
text(3.8, 3.0, expression(F[2]), col=col[4], adj=c(0, 0.5))
text(5.3, 3.5, "Gough", col=col[6], adj=c(0, 0.5))
text(5.0, 0.7, "WSB", col=col[8], adj=c(1, 0.5))
grayplot(0,0,type="n", xlim=c(1, 16), xaxs="i", ylim=yli, yaxs="i",
         xlab="Week", ylab="Growth rate (g/week)",
         xat=c(1,5,10,15), vlines=xat, vlines.col="gray70",vlines.lwd=3,
         yat=yat, hlines=yat, main="Females")
for(i in sample(which(f2$pheno$sex=="F"))) {
  y <- growthDeriv[i,1:16]
  x <- (1:16)[!is.na(y)]
  y <- y[!is.na(y)]
  lines(x, y, lwd=1, col=colalpha[1])
}
lines(1:16, f2meanF, col=col[3], lwd=2)
lines(1:16, gmeanF, col=col[5], lwd=2)
lines(1:16, wmeanF, col=col[7], lwd=2)
text(1.6, 3.0, expression(F[2]), col=col[3], adj=c(0, 0.5))
text(2.8, 3.5, "Gough", col=col[5], adj=c(0, 0.5))
text(4.0, 1.2, "WSB", col=col[7], adj=c(1, 0.5))
dev.off()
