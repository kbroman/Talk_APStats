library(qtl)
library(qtlcharts)

file <- "iplot_cache.RData"
if(file.exists(file)) {
  load(file)
} else {
  load("~/Projects/Payseur_Gough/DerivedData/goughF2_simple_v4.RData")
  load("~/Projects/Payseur_Gough/R/scanone_2014-04-23.RData")

  chr <- c(1,2,6,7,8,10,11,16)
  f2 <- f2[chr,]
  f2 <- reduce2grid(calc.genoprob(f2, step=0.5, err=0.002, map="c-f"))
  outsm <- subset(outsm[,1:18], chr=chr)
  outd <- subset(outd[,1:18], chr=chr)

  f2sm <- f2d <- f2
  f2sm$pheno[,1:16] <- growthSm
  f2d$pheno[,1:16] <- growthDeriv

  muFemale <- estQTLeffects(f2sm[,sex==0], phe=1:16, "means")
  muMale <- estQTLeffects(f2sm[,sex==1], phe=1:16, "means")
  mu <- cbindQTLeffects(muFemale, muMale, labels=c("female", "male"))

  effFemale <- estQTLeffects(f2sm[,sex==0], phe=1:16, "effects")
  effMale <- estQTLeffects(f2sm[,sex==1], phe=1:16, "effects")
  effFemale <- lapply(effFemale, function(a) -a)
  effMale <- lapply(effMale, function(a) -a)
  eff <- cbindQTLeffects(effFemale, effMale, labels=c("f", "m"))
  outsm_signed <- qtlcharts:::calcSignedLOD(outsm, eff, columns=c(1,3))

  dmuFemale <- estQTLeffects(f2d[,sex==0], phe=1:16, "means")
  dmuMale <- estQTLeffects(f2d[,sex==1], phe=1:16, "means")
  dmu <- cbindQTLeffects(dmuFemale, dmuMale, labels=c("f", "m"))

  deffFemale <- estQTLeffects(f2d[,sex==0], phe=1:16, "effects")
  deffMale <- estQTLeffects(f2d[,sex==1], phe=1:16, "effects")
  deffFemale <- lapply(deffFemale, function(a) -a)
  deffMale <- lapply(deffMale, function(a) -a)
  deff <- cbindQTLeffects(deffFemale, deffMale, labels=c("female", "male"))
  outd_signed <- qtlcharts:::calcSignedLOD(outd, deff, columns=c(1,3))

  save(outsm_signed, mu, outd_signed, dmu, file=file)
}

color <- brocolors("crayons")[c("Jungle Green", "Mango Tango", "Purple Mountain's Majesty",
                                "Jungle Green", "Mango Tango", "Purple Mountain's Majesty")]
names(color) <- NULL


setScreenSize(height=800, width=1066)

file <- "iplot_bodyweight.html"
if(file.exists(file)) unlink(file)
z <- iplotMScanone(outsm_signed, lod=1:16, times=1:16, effects=mu,
              chartOpts=list(eff_linecolor=color,
                             eff_ylab="Body weight (g)",
                             ylab="Week",
                             margin=list(left=60, top=40, right=60, bottom=40, inner=0)))
htmlwidgets::saveWidget(z, file=file)
system(paste("mv", file , ".."))

file <- "iplot_deriv_bodyweight.html"
if(file.exists(file)) unlink(file)
z <- iplotMScanone(outd_signed, lod=1:16, times=1:16, effects=dmu,
                   chartOpts=list(eff_linecolor=color,
                                  eff_ylab="Growth rate (g/week)",
                                  ylab="Week",
                             margin=list(left=60, top=40, right=60, bottom=40, inner=0)))
htmlwidgets::saveWidget(z, file=file)
system(paste("mv", file , ".."))
