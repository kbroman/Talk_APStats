## Figure 3. Growth curves of GI (from initial sample of island mice
##           raised in the lab; not the parents of the cross) and WSB

library(broman)

phe <- read.csv("~/Projects/Payseur_Gough/RawData/OtherData/Gough_phenotypes_012714.csv", as.is=TRUE, check.names=FALSE)
wkcol <- grep('^wk\\d+$', colnames(phe))
for(i in wkcol) phe[,i] <- as.numeric(phe[,i])
parents <- phe[phe$Direction=="GG" | phe$Direction=="WW",]
par_strain <- match(parents$Direction, c("GG", "WW"))
par_sex <- match(parents$Sex, c("F", "M"))
growth_yli <- c(0, 1.02*max(c(unlist(phe[,wkcol]), unlist(f2$pheno[,1:16])), na.rm=TRUE))

load("~/Projects/Payseur_Gough/DerivedData/goughF2_simple_v4.RData")
rate_yli <- range(growthDeriv, na.rm=TRUE)*1.02

load("gw_smoothed.RData")


color <- brocolors("crayons")[c("Jungle Green", "Purple Mountain's Majesty")]
textcolor <- brocolors("crayons")[c("Pine Green", "Royal Purple")]

pdf("../Figs/GI_v_W.pdf", height=5.5, width=10, pointsize=14)
par(bg=bg, fg="white", col="white", col.axis="white", col.lab="white", col.main="white")
par(mfrow=c(1,2), las=1, mar=c(4.1, 4.1, 2.6, 0.6))
ylsm <- c(0, max(gwSm, na.rm=TRUE)*1.01)
xat <- c(1, 4, 8, 12, 16)
yat <- pretty(ylsm, n=8)
grayplot(weeks, gwSm[1,], type="n", xlab="Week", ylab="Body weight (g)",
         main="Males", ylim=growth_yli, yaxs="i", xat=xat, yat=yat,
         vlines=xat, hlines=yat, xaxs="i")
for(i in sample(which(gw$Sex=="male")))
    lines(weeks, gwSm[i,], col=color[match(gw$Type[i], c("GI", "WW"))])
text(4, 21, "GI", col=textcolor[1])
text(4, 6, "WSB", col=textcolor[2])

for(i in 1:2) lines(weeks, meSmMale[i,], col=textcolor[i], lwd=2)


grayplot(weeks, gwSm[1,], type="n", xlab="Week", ylab="Body weight (g)",
         main="Females", ylim=growth_yli, yaxs="i", xat=xat, yat=yat,
         vlines=xat, hlines=yat, xaxs="i")
for(i in sample(which(gw$Sex=="female")))
    lines(weeks, gwSm[i,], col=color[match(gw$Type[i], c("GI", "WW"))])
text(4, 19, "GI", col=textcolor[1])
text(4, 5, "WSB", col=textcolor[2])

for(i in 1:2) lines(weeks, meSmFemale[i,], col=textcolor[i], lwd=2)
dev.off()


pdf("../Figs/GI_v_W_2.pdf", height=5.5, width=10, pointsize=14)
par(bg=bg, fg="white", col="white", col.axis="white", col.lab="white", col.main="white")
par(mfrow=c(1,2), las=1, mar=c(4.1, 4.1, 2.6, 0.6))
ylsm <- c(0, max(gwSm, na.rm=TRUE)*1.01)
xat <- c(1, 4, 8, 12, 16)
yat <- pretty(ylsm, n=8)
grayplot(weeks, gwSm[1,], type="n", xlab="Week", ylab="Body weight (g)",
         main="Males", ylim=growth_yli, yaxs="i", xat=xat, yat=yat,
         vlines=xat, hlines=yat, xaxs="i")
for(i in sample(which(gw$Sex=="male")))
    lines(weeks, gwSm[i,], col=color[match(gw$Type[i], c("GI", "WW"))])
text(4, 21, "GI", col=textcolor[1])
text(4, 6, "WSB", col=textcolor[2])

for(i in 1:2) lines(weeks, meSmMale[i,], col=textcolor[i], lwd=2)
for(i in 1:2) lines(weeks, meSmFemale[i,], col=textcolor[i], lwd=2, lty=2)


grayplot(weeks, gwSm[1,], type="n", xlab="Week", ylab="Body weight (g)",
         main="Females", ylim=growth_yli, yaxs="i", xat=xat, yat=yat,
         vlines=xat, hlines=yat, xaxs="i")
for(i in sample(which(gw$Sex=="female")))
    lines(weeks, gwSm[i,], col=color[match(gw$Type[i], c("GI", "WW"))])
text(4, 19, "GI", col=textcolor[1])
text(4, 5, "WSB", col=textcolor[2])

for(i in 1:2) lines(weeks, meSmFemale[i,], col=textcolor[i], lwd=2)
for(i in 1:2) lines(weeks, meSmMale[i,], col=textcolor[i], lwd=2, lty=2)
dev.off()


pdf("../Figs/GI_v_W_rate.pdf", height=5.5, width=10, pointsize=14)
par(bg=bg, fg="white", col="white", col.axis="white", col.lab="white", col.main="white")
par(mfrow=c(1,2), las=1, mar=c(4.1, 4.1, 2.6, 0.6))
yld <- range(gwDeriv, na.rm=TRUE)
xat <- c(1, 4, 8, 12, 16)
yat <- pretty(yld)
grayplot(weeks, gwDeriv[1,], type="n", xlab="Week", ylab="Growth rate (g/week)",
         main="Males", ylim=rate_yli, yaxs="i", xat=xat, yat=yat,
         vlines=xat, hlines=yat, xaxs="i")
for(i in sample(which(gw$Sex=="male")))
    lines(weeks, gwDeriv[i,], col=color[match(gw$Type[i], c("GI", "WW"))])
text(6, 3.5, "GI", col=textcolor[1])
text(4.3, 0.3, "WSB", col=textcolor[2])

for(i in 1:2) lines(weeks, meDerivMale[i,], col=textcolor[i], lwd=2)


grayplot(weeks, gwDeriv[1,], type="n", xlab="Week", ylab="Growth rate (g/week)",
         main="Females", ylim=rate_yli, yaxs="i", xat=xat, yat=yat,
         vlines=xat, hlines=yat, xaxs="i")
for(i in sample(which(gw$Sex=="female")))
    lines(weeks, gwDeriv[i,], col=color[match(gw$Type[i], c("GI", "WW"))])
text(6.5,  2.5, "GI", col=textcolor[1])
text(3.0,  0.3, "WSB", col=textcolor[2])

for(i in 1:2) lines(weeks, meDerivFemale[i,], col=textcolor[i], lwd=2)

dev.off()


pdf("../Figs/GI_v_W_rate_2.pdf", height=5.5, width=10, pointsize=14)
par(bg=bg, fg="white", col="white", col.axis="white", col.lab="white", col.main="white")
par(mfrow=c(1,2), las=1, mar=c(4.1, 4.1, 2.6, 0.6))
yld <- range(gwDeriv, na.rm=TRUE)
xat <- c(1, 4, 8, 12, 16)
yat <- pretty(yld)
grayplot(weeks, gwDeriv[1,], type="n", xlab="Week", ylab="Growth rate (g/week)",
         main="Males", ylim=rate_yli, yaxs="i", xat=xat, yat=yat,
         vlines=xat, hlines=yat, xaxs="i")
for(i in sample(which(gw$Sex=="male")))
    lines(weeks, gwDeriv[i,], col=color[match(gw$Type[i], c("GI", "WW"))])
text(6, 3.5, "GI", col=textcolor[1])
text(4.3, 0.3, "WSB", col=textcolor[2])

for(i in 1:2) lines(weeks, meDerivMale[i,], col=textcolor[i], lwd=2)
for(i in 1:2) lines(weeks, meDerivFemale[i,], col=textcolor[i], lwd=2, lty=2)


grayplot(weeks, gwDeriv[1,], type="n", xlab="Week", ylab="Growth rate (g/week)",
         main="Females", ylim=rate_yli, yaxs="i", xat=xat, yat=yat,
         vlines=xat, hlines=yat, xaxs="i")
for(i in sample(which(gw$Sex=="female")))
    lines(weeks, gwDeriv[i,], col=color[match(gw$Type[i], c("GI", "WW"))])
text(6.5,  2.5, "GI", col=textcolor[1])
text(3.0,  0.3, "WSB", col=textcolor[2])

for(i in 1:2) lines(weeks, meDerivFemale[i,], col=textcolor[i], lwd=2)
for(i in 1:2) lines(weeks, meDerivMale[i,], col=textcolor[i], lwd=2, lty=2)

dev.off()
