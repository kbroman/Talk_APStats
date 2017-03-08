library(simcross)
library(broman)
set.seed(46002209)


mycolors <- brocolors("crayons")[c("Chestnut", "Outrageous Orange", "Macaroni and Cheese", "Antique Brass",
                                   "Shadow", "Dandelion", "Yellow Green", "Forest Green", "Robin's Egg Blue",
                                   "Green Blue", "Cornflower", "Purple Heart", "Purple Pizzazz",
                                   "Mulberry", "Pink Sherbert", "Gray", "Wisteria")]
mycolors[1:16] <- mycolors[c(3,6,11,14,  10,5,9,2,  1,8,12,16,  13,7,4,15)]
graymask <- rep(colwalpha("black", alpha=0.4), length(mycolors))

# simulate inbreeding
L <- 100
p <- f1 <- f2 <- f3 <- vector("list", 8)
for(i in 1:8)
  p[[i]] <- create_parent(L, 1:2 + (i-1)*2)
for(j in c(0, 2, 4, 6)) {
  for(i in 1:2) f1[[i+j]] <- cross(p[[j+1]],   p[[j+2]])
  for(i in 1:2) f2[[i+j]] <- cross(f1[[j+1]], f1[[j+2]])
  for(i in 1:2) f3[[i+j]] <- cross(f2[[j+1]], f2[[j+2]])
  for(i in 1:2) f4[[i+j]] <- cross(f3[[j+1]], f3[[j+2]])
}


pdf("../Figs/cross1.pdf", height=6.50, width=10.00, pointsize=18)
par(bg=brocolors("bg"), col="white", col.axis="white",
    col.main="white", col.lab="white")

par(mar=rep(0.1, 4), bty="n")
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n",
     xlim=c(0, 200), ylim=c(0, 260))
x <- seq(5, 195, length=11)[c(1,2,4,5,7,8,10,11)]
xd <- diff(x[1:2])
y <- seq(250, 10, length=5)
for(j in c(0,2,4,6)) {
  for(i in 1:2) {
    plot_ind(p[[i+j]], c(x[i+j], y[1]), col=mycolors, chrlength=25)
    plot_ind(f1[[i+j]], c(x[i+j], y[2]), col=mycolors, chrlength=25)
    plot_ind(f2[[i+j]], c(x[i+j], y[3]), col=mycolors, chrlength=25)
    plot_ind(f3[[i+j]], c(x[i+j], y[4]), col=mycolors, chrlength=25)
    plot_ind(f4[[i+j]], c(x[i+j], y[5]), col=mycolors, chrlength=25)
  }
  for(i in 1:4)
    plot_crosslines(c(x[1+j], y[i]), c(x[2+j], y[i]), chrlength=25,
                    list(c(x[1+j], y[i+1]), c(x[2+j], y[i+1])))
}

dev.off()


pdf("../Figs/cross2.pdf", height=6.50, width=10.00, pointsize=18)
par(bg=brocolors("bg"), col="white", col.axis="white",
    col.main="white", col.lab="white")

par(mar=rep(0.1, 4), bty="n")
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n",
     xlim=c(0, 200), ylim=c(0, 260))
for(j in c(0,2,4,6)) {
  for(i in 1:2) {
    plot_ind(p[[i+j]], c(x[i+j], y[1]), col=mycolors, chrlength=25)
    plot_ind(f1[[i+j]], c(x[i+j], y[2]), col=mycolors, chrlength=25)
    plot_ind(f2[[i+j]], c(x[i+j], y[3]), col=mycolors, chrlength=25)
    plot_ind(f3[[i+j]], c(x[i+j], y[4]), col=mycolors, chrlength=25)
    plot_ind(f4[[i+j]], c(x[i+j], y[5]), col=mycolors, chrlength=25)

    plot_ind(p[[1]], c(x[i+j], y[1]), col=graymask, chrlength=25)
    plot_ind(p[[1]], c(x[i+j], y[2]), col=graymask, chrlength=25)
    plot_ind(p[[1]], c(x[i+j], y[3]), col=graymask, chrlength=25)
    plot_ind(p[[1]], c(x[i+j], y[4]), col=graymask, chrlength=25)
    if(j>2)
      plot_ind(p[[1]], c(x[i+j], y[5]), col=graymask, chrlength=25)
  }
  for(i in 1:4) {
    plot_crosslines(c(x[1+j], y[i]), c(x[2+j], y[i]), chrlength=25,
                    list(c(x[1+j], y[i+1]), c(x[2+j], y[i+1])),
                    col="gray40")
  }
}

dev.off()

x <- seq(5, 195, length=7)[-c(1,4,7)]
wsbx <- c(x[1]-xd, x[2]+xd, x[3]-xd, x[4]+xd)

pdf("../Figs/cross3.pdf", height=6.5, width=10, pointsize=18)
par(bg=brocolors("bg"), col="white", col.axis="white",
    col.main="white", col.lab="white")

par(mar=rep(0.1, 4), bty="n")
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n",
     xlim=c(0, 200), ylim=c(120, 260))
for(j in c(0,2)) {
  for(i in 1:2)
    plot_ind(f4[[i+j]], c(x[i+j], y[1]), col=mycolors, chrlength=25)
}

dev.off()


for(cr in 1:4) {
  pdf(paste0("../Figs/cross", 3+cr, ".pdf"), height=6.5, width=10, pointsize=18)
  par(bg=brocolors("bg"), col="white", col.axis="white",
      col.main="white", col.lab="white")

  par(mar=rep(0.1, 4), bty="n")
  plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n",
       xlim=c(0, 200), ylim=c(120, 260))
  for(j in c(0,2)) {
    for(i in 1:2)
      plot_ind(f4[[i+j]], c(x[i+j], y[1]), col=mycolors, chrlength=25)
  }

  xx <- c(x[cr], wsbx[cr])
  wsb <- create_parent(L, 17)
  plot_ind(wsb, c(wsbx[cr], y[1]), col=mycolors, chrlength=25)
  if(cr %% 2)
    f5 <- list(cross(wsb, f4[[cr]]), cross(wsb, f4[[cr]]))
  else
    f5 <- list(cross(f4[[cr]], wsb), cross(f4[[cr]], wsb))
  for(i in 1:2)
    plot_ind(f5[[i]], c(xx[i], y[2]), col=mycolors, chrlength=25)
  plot_crosslines(c(xx[1], y[1]), c(xx[2], y[1]), chrlength=25,
                  list(c(xx[1], y[2]), c(xx[2], y[2])), col="white")

  nf6 <- 10
  f6 <- vector("list", nf6)
  xn <- seq(5, 195, len=nf6)
  for(i in 1:nf6) {
    f6[[i]] <- cross(f5[[1]], f5[[2]])
    plot_ind(f6[[i]], c(xn[i], y[3]), col=mycolors, chrlength=25)
  }
  plot_crosslines(c(xx[1], y[2]), c(xx[2], y[2]), chrlength=25,
                  lapply(xn, function(a) c(a, y[3])), col="white")

  dev.off()
}


mycolors2 <- mycolors
mycolors2[-length(mycolors2)] <- mycolors[2]

pdf("../Figs/cross8.pdf", height=6.5, width=10, pointsize=18)
par(bg=brocolors("bg"), col="white", col.axis="white",
    col.main="white", col.lab="white")

par(mar=rep(0.1, 4), bty="n")
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n",
     xlim=c(0, 200), ylim=c(120, 260))
for(j in c(0,2)) {
  for(i in 1:2)
    plot_ind(f4[[i+j]], c(x[i+j], y[1]), col=mycolors2, chrlength=25)
}

plot_ind(wsb, c(wsbx[cr], y[1]), col=mycolors2, chrlength=25)
for(i in 1:2)
  plot_ind(f5[[i]], c(xx[i], y[2]), col=mycolors2, chrlength=25)
plot_crosslines(c(xx[1], y[1]), c(xx[2], y[1]), chrlength=25,
                list(c(xx[1], y[2]), c(xx[2], y[2])), col="white")

for(i in 1:nf6) {
  plot_ind(f6[[i]], c(xn[i], y[3]), col=mycolors2, chrlength=25)
}
plot_crosslines(c(xx[1], y[2]), c(xx[2], y[2]), chrlength=25,
                lapply(xn, function(a) c(a, y[3])), col="white")
dev.off()
