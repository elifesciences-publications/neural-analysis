library(matrixStats)
load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/neuralhist.rFunc")
load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/bigbinFR.rFunc")

filepath = "C:/Users/Kevin Caref/Google Drive/Opioid Pilot/Neural Data/sorted/"
binw = 200
psthmin = 10
psthmax = 1
event = 4
cueexonly = T
startt = 0
endt = 7200
basebins = psthmin/(binw/1000)   #number of bins
baseline = seq(1,basebins)

sated = neuralhist.rFunc(path = paste(filepath, "sated response analysis/", sep = ""), startt,
 endt, binw, psthmin, psthmax, event, cueexonly)
 
satedtestbin = bigbinFR.rFunc(path = paste(filepath, "sated response analysis/", sep = ""), startt,
 endt, binw, psthmin, psthmax, event, cueexonly)
 
satedtestbin = satedtestbin[sated[[2]]]  
 

restricted = neuralhist.rFunc(path = paste(filepath, "restricted response analysis/", sep = ""), startt=0,
 endt=7200, binw, psthmin, psthmax, event, cueexonly=T)
 
resttestbin = bigbinFR.rFunc(path = paste(filepath, "restricted response analysis/", sep = ""), startt=0,
 endt=7200, binw, psthmin, psthmax, event, cueexonly=T) 

resttestbin = resttestbin[restricted[[2]]]

restricted = restricted[[1]][,restricted[[2]]]
sated = sated[[1]][,sated[[2]]]

#calculates the mean and standard deviation for each neuron
satedsds = sapply(seq(1, ncol(sated)), function(x) sd(as.numeric(sated[baseline,x])))
satedmeans = sapply(seq(1, ncol(sated)), function(x) mean(as.numeric(sated[baseline,x])))

#calculates a z-score for each bin for each neuron
satedzresp = (satedtestbin-satedmeans)/satedsds
  
  

#calculates the mean and standard deviation for each neuron
restsds = sapply(seq(1, ncol(restricted)), function(x) sd(as.numeric(restricted[baseline,x])))
restmeans = sapply(seq(1, ncol(restricted)), function(x) mean(as.numeric(restricted[baseline,x])))

#calculates a z-score for each bin for each neuron
restzresp = (resttestbin-restmeans)/restsds

#detect any missing values for sessions with no trials
#torem = which(is.na(restzresp[1,]))


meansatedfiring = mean(satedzresp)
meanrestfiring = mean(restzresp)


satederror = sd(satedzresp)/sqrt(length(satedzresp))
resterror = sd(restzresp)/sqrt(length(restzresp))

satediqr = boxplot(satedzresp, plot= F)$stats[c(2,4)]
restiqr = boxplot(restzresp, plot= F)$stats[c(2,4)]

par(pty = "s")
plot.new()
plot.window(xlim = c(0,2), ylim = c(0,25))

segments(.5, satediqr[2], .5, satediqr[1], lwd = 2)

segments(1, restiqr[2], 1, restiqr[1], lwd = 2)


points(.5, median(satedzresp), col = "blue", pch = 19, cex = 3)
points(1, median(restzresp), col = "firebrick1", pch = 19, cex = 3)

#axis(1, at = c(.5,1), labels = c("Sated", "Restr."), cex.axis = 1.75)
axis(2, at = c(seq(0, 25,5)), las = 2, cex.axis = 1.75, tcl= -.8)

mtext("Z-Score", side = 2, line = 3.5, cex = 1.75)


p = wilcox.test(satedzresp, restzresp, paired = F)$p.value

text(paste("p=",p,sep = ""), x = .5, y = 20)





