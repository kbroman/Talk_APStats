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

  effFemale <- estQTLeffects(f2sm[,sex==0], phe=1:16, "effects")
  effMale <- estQTLeffects(f2sm[,sex==1], phe=1:16, "effects")
  effFemale <- lapply(effFemale, function(a) -a)
  effMale <- lapply(effMale, function(a) -a)
  eff <- cbindQTLeffects(effFemale, effMale, labels=c("female", "male"))
  outsm_signed <- qtlcharts:::calcSignedLOD(outsm, eff, columns=c(1,3))

  deffFemale <- estQTLeffects(f2d[,sex==0], phe=1:16, "effects")
  deffMale <- estQTLeffects(f2d[,sex==1], phe=1:16, "effects")
  deffFemale <- lapply(deffFemale, function(a) -a)
  deffMale <- lapply(deffMale, function(a) -a)
  deff <- cbindQTLeffects(deffFemale, deffMale, labels=c("female", "male"))
  outd_signed <- qtlcharts:::calcSignedLOD(outd, deff, columns=c(1,3))

  save(outsm_signed, eff, outd_signed, deff, file=file)
}

file <- "../iplot_bodyweight.html"
if(file.exists(file)) unlink(file)
iplotMScanone(outsm_signed, lod=1:16, effects=eff,
              title="Body weight", openfile=FALSE,
              onefile=TRUE, file=file,
              chartOpts=list(eff_linecolor=c("red", "pink", "blue", "lightblue"),
                             eff_ylab="QTL effect"),
              caption=c("In the heat map at the top, red indicates that the Gough allele increases body weight, ",
                "and blue indicates that the Gough allele decreases body weight. Hover over the heat map to view ",
                "individual LOD curves below and estimated QTL effects to the right. Regarding the estimated QTL effects, ",
                "on the autosomes, red and pink are for the additive and dominance effects, respectively, in females, while ",
                "blue and light blue are the additive and dominance effects, respectively, in males. ",
                "On the X chromosome, red and pink are for the additive effects in females from the G&times;W and W&times;G crosses, ",
                "respectively, while blue is for the additive effect in males. Positive values indicate that the Gough allele ",
                "increases body weight."))



file <- "../iplot_deriv_bodyweight.html"
if(file.exists(file)) unlink(file)
iplotMScanone(outd_signed, lod=1:16, effects=deff,
              title="Derivative of body weight", openfile=FALSE,
              onefile=TRUE, file=file,
              chartOpts=list(eff_linecolor=c("red", "pink", "blue", "lightblue"),
                             eff_ylab="QTL effect"),
              caption=c("In the heat map at the top, red indicates that the Gough allele increases the rate of growth, ",
                "and blue indicates that the Gough allele decreases the rate of growth. Hover over the heat map to view ",
                "individual LOD curves below and estimated QTL effects to the right. Regarding the estimated QTL effects, ",
                "on the autosomes, red and pink are for the additive and dominance effects, respectively, in females, while ",
                "blue and light blue are the additive and dominance effects, respectively, in males. ",
                "On the X chromosome, red and pink are for the additive effects in females from the G&times;W and W&times;G crosses, ",
                "respectively, while blue is for the additive effect in males. Positive values indicate that the Gough allele ",
                "increases the rate of growth."))
