load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/unineuralhist.rFunc")
load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/KC.sigbins_inhib_uni.Rfunc")

side = "ipsi"


prefiring = unineuralhist.rFunc(path = "C:/Users/Kevin Caref/Google Drive/Opioid Pilot/Neural Data/sorted/unilateral ctap/", startt = 0
, endt = 2000, binw = 50, psthmin = 5, psthmax = 1, event = 4, cueexonly = T, side)

cueexidx = prefiring[[2]]

prefiring = unineuralhist.rFunc(path = "C:/Users/Kevin Caref/Google Drive/Opioid Pilot/Neural Data/sorted/unilateral ctap/", startt = 0
, endt = 2000, binw = 50, psthmin = 5, psthmax = 1, event = 1, cueexonly = F, side)

postfiring = unineuralhist.rFunc(path = "C:/Users/Kevin Caref/Google Drive/Opioid Pilot/Neural Data/sorted/unilateral ctap/", startt = 2720
, endt = 4720, binw = 50, psthmin = 5, psthmax = 1, event = 1, cueexonly = F, side)


#cueexidx = prefiring[[2]]
#prefiring = prefiring[[1]]

cuein = KC.sigbins_inhib_uni.Rfunc(path = "C:/Users/Kevin Caref/Google Drive/Opioid Pilot/Neural Data/sorted/unilateral ctap/", startt=0, endt=2000, event=1, side)
cueinidx = which(colSums(cuein) > 0)


prebaseline = sapply(seq(1, ncol(prefiring)), function(x)
  mean(as.numeric(prefiring[1:100,x])))

postbaseline = sapply(seq(1, ncol(postfiring)), function(x)
  mean(as.numeric(postfiring[1:100,x])))


#peaksort = order(sapply(seq(1,ncol(prefiring)), function(x) mean(prefiring[c(51:56),x])))
#cueencoding = peaksort[21:53]
otheridx = which(seq(1:length(prebaseline)) %in% c(cueexidx, cueinidx)== F)

unipre = prebaseline
unipost = postbaseline
bipre = prebaseline
bipost = postbaseline

bl.lm = lm(c(unipost, bipost) ~ c(unipre, bipre)) 
confint(bl.lm, level = .95)


#bl.lm = lm(postbaseline[-16]~prebaseline[-16])   #remove contra outlier
#confint(bl.lm, level = .95)


cueex.lm = lm(postbaseline[cueexidx]~prebaseline[cueexidx])
confint(cueex.lm, level = .95)

cuein.lm = lm(postbaseline[cueinidx]~prebaseline[cueinidx])
confint(cuein.lm, level = .95)

other.lm = lm(postbaseline[otheridx]~prebaseline[otheridx])
confint(other.lm, level = .95)

#remove contra outlier
#cueinidx = cueinidx[-1] 


par(pty = "s")
plot.new()
plot.window(xlim = c(0,10), ylim = c(0,10))

#uni points
points(prebaseline[otheridx], postbaseline[otheridx], pch = 19, cex = 2, col = "black")
points(prebaseline[cueexidx], postbaseline[cueexidx], pch = 19, cex = 2, col = "firebrick1")
points(prebaseline[cueinidx], postbaseline[cueinidx], pch = 19, cex = 2, col = "blue")

#bi points
points(prebaseline[otheridx], postbaseline[otheridx], pch = 18, cex = 2, col = "black")
points(prebaseline[cueexidx], postbaseline[cueexidx], pch = 18, cex = 2, col = "firebrick1")
points(prebaseline[cueinidx], postbaseline[cueinidx], pch = 18, cex = 2, col = "blue")

#abline(a = 0, b = 1)
segments(0,0,10,10, lwd = 2,col = "black", lty=2)
#abline(cue.lm, col = "gray", lwd = 2)
#abline(other.lm, col = "black", lwd = 2)
abline(bl.lm, col = "gray", lwd = 2)

axis(1, at = seq(0,10, 2), cex.axis = 1.75)
axis(2, at = seq(0,10, 2), cex.axis = 1.75, las = 2, tcl = -.8)

mtext("Pre-injection Baseline (Hz)", side = 1, cex = 1.75, line=3)
mtext("Post-injection Baseline (Hz)", side = 2, cex = 1.75, line=3)
mtext("Cue-excited", side = 3, cex = 1.5, line = -2, at = 2, col = "firebrick1")
mtext("Cue-inhibited", side = 3, cex = 1.5, line = -3, at = 2, col = "blue")
mtext("Other", side = 3, cex = 1.5, line = -4, at = 2, col = "black")


mtext("Red = unity line, gray = regression for all points", side = 3, cex = 2, line=4)
