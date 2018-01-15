satedfiring = read.csv("C:/Users/Kevin Caref/Desktop/Opioid Pilot/CS+ all trials sated_gsmooth3.csv")
restrictedfiring = read.csv("C:/Users/Kevin Caref/Desktop/Opioid Pilot/CS+ all trials restricted_gsmooth3.csv")


satedbaseline = sapply(seq(1, ncol(satedfiring)), function(x)
  mean(as.numeric(satedfiring[1:40,x])))

restrictedbaseline = sapply(seq(1, ncol(restrictedfiring)), function(x)
  mean(as.numeric(restrictedfiring[1:40,x])))


groups = c(satedbaseline, restrictedbaseline)
conditions = c(rep(1, length(satedbaseline)), rep(2, length(restrictedbaseline)))
summary(aov(groups~conditions))


par(mai = c(3,3,3,3), pty = "s")
plot.new()
plot.window(xlim = c(0,1), ylim = c(0,9))
barplot(c(mean(satedbaseline), mean(restrictedbaseline)))

axis(1, at = seq(0,10, 2), cex.axis = 2)
axis(2, at = seq(0,10, 2), cex.axis = 2)

mtext("Pre-injection Baseline (Hz)", side = 1, cex = 2, line=4)
mtext("Post-injection Baseline (Hz)", side = 2, cex = 2, line=4)

mtext("Red = unity line, gray = regression for all points", side = 3, cex = 2, line=4)
