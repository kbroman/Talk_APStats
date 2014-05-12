library(simcross)
library(broman)
set.seed(46002207)


mycolors <- c(CCcolors()[c(2,6,7,8,1,3,4,5)], brocolors("crayons")["Wisteria"])

pdf("../Figs/cross1.pdf", height=6.5, width=10, pointsize=18)
par(bg=brocolors("bg"), col="white", col.axis="white",
    col.main="white", col.lab="white")

# plot the 3 generations of inbreeding
L <- 100
p <- f1 <- f2 <- f3 <- vector("list", 4)
for(i in 1:4)
  p[[i]] <- create_parent(L, 1:2 + (i-1)*2)
for(j in c(0, 2)) {
  for(i in 1:2) f1[[i+j]] <- cross(p[[j+1]],   p[[j+2]])
  for(i in 1:2) f2[[i+j]] <- cross(f1[[j+1]], f1[[j+2]])
  for(i in 1:2) f3[[i+j]] <- cross(f2[[j+1]], f2[[j+2]])
}

par(mar=rep(0.1, 4), bty="n")
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n",
     xlim=c(0, 200), ylim=c(0, 200))
x <- c(30, 70, 110, 150)
y <- seq(190, 10, len=4)
for(j in c(0,2)) {
  for(i in 1:2) {
    plot_ind(p[[i+j]], c(x[i+j], y[1]), col=mycolors, chrlength=25)
    plot_ind(f1[[i+j]], c(x[i+j], y[2]), col=mycolors, chrlength=25)
    plot_ind(f2[[i+j]], c(x[i+j], y[3]), col=mycolors, chrlength=25)
    plot_ind(f3[[i+j]], c(x[i+j], y[4]), col=mycolors, chrlength=25)
  }
  for(i in 1:3)
    plot_crosslines(c(x[1+j], y[i]), c(x[2+j], y[i]), chrlength=25,
                    list(c(x[1+j], y[i+1]), c(x[2+j], y[i+1])), col="white")
}

dev.off()


pdf("../Figs/cross2.pdf", height=6.5, width=10, pointsize=18)
par(bg=brocolors("bg"), col="white", col.axis="white",
    col.main="white", col.lab="white")

par(mar=rep(0.1, 4), bty="n")
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n",
     xlim=c(0, 200), ylim=c(0, 200))
for(j in c(0,2)) {
  for(i in 1:2)
    plot_ind(f3[[i+j]], c(x[i+j], y[1]), col=mycolors, chrlength=25)
}

dev.off()


pdf("../Figs/cross3.pdf", height=6.5, width=10, pointsize=18)
par(bg=brocolors("bg"), col="white", col.axis="white",
    col.main="white", col.lab="white")

par(mar=rep(0.1, 4), bty="n")
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n",
     xlim=c(0, 200), ylim=c(0, 200))
for(j in c(0,2)) {
  for(i in 1:2)
    plot_ind(f3[[i+j]], c(x[i+j], y[1]), col=mycolors, chrlength=25)
}
x <- c(x, 190)

wsb <- create_parent(L, 9)
plot_ind(wsb, c(x[5], y[1]), col=mycolors, chrlength=25)
f4 <- list(cross(f3[[4]], wsb), cross(f3[[4]], wsb))
for(i in 1:2)
  plot_ind(f4[[i]], c(x[[3+i]], y[2]), col=mycolors, chrlength=25)
plot_crosslines(c(x[4], y[1]), c(x[5], y[1]), chrlength=25,
                list(c(x[4], y[2]), c(x[5], y[2])), col="white")

nf5 <- 10
f5 <- vector("list", nf5)
xn <- seq(5, 195, len=nf5)
for(i in 1:nf5) {
  f5[[i]] <- cross(f4[[1]], f4[[2]])
  plot_ind(f5[[i]], c(xn[i], y[3]), col=mycolors, chrlength=25)
}
plot_crosslines(c(x[4], y[2]), c(x[5], y[2]), chrlength=25,
                lapply(xn, function(a) c(a, y[3])), col="white")



dev.off()



pdf("../Figs/cross4.pdf", height=6.5, width=10, pointsize=18)
par(bg=brocolors("bg"), col="white", col.axis="white",
    col.main="white", col.lab="white")

mycolors[1:8] <- brocolors("crayons")["Blue Green"]

par(mar=rep(0.1, 4), bty="n")
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n",
     xlim=c(0, 200), ylim=c(0, 200))
for(j in c(0,2)) {
  for(i in 1:2)
    plot_ind(f3[[i+j]], c(x[i+j], y[1]), col=mycolors, chrlength=25)
}

plot_ind(wsb, c(x[5], y[1]), col=mycolors, chrlength=25)
for(i in 1:2)
  plot_ind(f4[[i]], c(x[[3+i]], y[2]), col=mycolors, chrlength=25)
plot_crosslines(c(x[4], y[1]), c(x[5], y[1]), chrlength=25,
                list(c(x[4], y[2]), c(x[5], y[2])), col="white")
for(i in 1:nf5)
  plot_ind(f5[[i]], c(xn[i], y[3]), col=mycolors, chrlength=25)
plot_crosslines(c(x[4], y[2]), c(x[5], y[2]), chrlength=25,
                lapply(xn, function(a) c(a, y[3])), col="white")
dev.off()


