# Get LOD curves and effect plot info for 5 wk body weight
# and convert to JSON file for interactive graph

library(qtl)

load("~/Projects/Payseur_Gough/DerivedData/goughF2_simple_v4.RData")

# reduce the set of markers
marker_subset <- NULL
for(i in names(f2$geno))
    marker_subset <- c(marker_subset,
                       pickMarkerSubset(pull.map(f2, chr=i)[[1]], 0.5))
f2 <- pull.markers(f2, marker_subset)
f2$pheno <- cbind(weight=growthSm[,5], f2$pheno[,c("ID", "sex", "pgm")])

phenotype <- "Weight (g) at 5 weeks"

# genome scan with sex as interactive covariate
sex <- as.numeric(f2$pheno$sex)-1
f2 <- calc.genoprob(f2, step=1, error.prob=0.002, map.function="c-f")
out <- scanone(f2, phe="weight", addcovar=sex, intcovar=sex, method="hk")

f2 <- sim.geno(f2, step=0, error.prob=0.002, map.function="c-f", n.draws=8)
mar <- markernames(f2)

library(parallel)
ncores <- detectCores()
qtleffects <- mclapply(mar, function(a) effectplot(f2, phe="weight", mname1="sex",
                                                   mname2=a, draw=FALSE), mc.cores=ncores)

for(i in seq(along=qtleffects)) {
  qtleffects[[i]]$Means <- lapply(as.list(as.data.frame(qtleffects[[i]]$Means)), function(a) {names(a) <- c("Female", "Male"); a})
  qtleffects[[i]]$SEs <- lapply(as.list(as.data.frame(qtleffects[[i]]$SEs)), function(a) {names(a) <- c("Female", "Male"); a})
  nam <- names(qtleffects[[i]]$Means)
  nam <- sapply(strsplit(nam, "\\."), function(a) a[2])
  names(qtleffects[[i]]$Means) <- names(qtleffects[[i]]$SEs) <- nam
}
names(qtleffects) <- mar

# marker index within lod curves
map <- pull.map(f2)
outspl <- split(out, out[,1])
mar <- map
for(i in seq(along=map)) {
  mar[[i]] <- match(names(map[[i]]), rownames(outspl[[i]]))-1
  names(mar[[i]]) <- names(map[[i]])
}
markers <- lapply(mar, names)

phevals <- f2$pheno$weight
f2i <- pull.geno(fill.geno(f2, err=0.002, map.function="c-f"))
g <- pull.geno(f2)
f2i[is.na(g) | f2i != g] <- -f2i[is.na(g) | f2i != g]
f2i <- as.list(as.data.frame(f2i))
individuals <- as.character(f2$pheno$ID)

# write data to JSON file
library(RJSONIO)
cat0 <- function(...) cat(..., sep="", file="../weightlod.json")
cat0a <- function(...) cat(..., sep="", file="../weightlod.json", append=TRUE)
cat0("{\n")
cat0a("\"phenotype\" : \"", phenotype, "\",\n\n")
cat0a("\"chr\" :\n", RJSONIO::toJSON(chrnames(f2)), ",\n\n")
cat0a("\"lod\" :\n", RJSONIO::toJSON(lapply(split(out, out[,1]), function(a) as.list(a[,2:3])), digits=8), ",\n\n")
cat0a("\"markerindex\" :\n", RJSONIO::toJSON(mar), ",\n\n")
cat0a("\"markers\" :\n", RJSONIO::toJSON(markers), ",\n\n")
cat0a("\"effects\" :\n", RJSONIO::toJSON(qtleffects, digits=8), ",\n\n")
cat0a("\"phevals\" :\n", RJSONIO::toJSON(phevals, digits=8), ",\n\n")
cat0a("\"sex\" :\n", RJSONIO::toJSON(sex), ",\n\n")
cat0a("\"geno\" :\n", RJSONIO::toJSON(f2i), ",\n\n")
cat0a("\"individuals\" :\n", RJSONIO::toJSON(individuals), "\n\n")
cat0a("}\n")
