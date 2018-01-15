library(matrixStats)
load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/unineuralhist.rFunc")
load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/bigbinFR_uni.rFunc")

filepath = "C:/Users/Kevin Caref/Google Drive/Opioid Pilot/Neural Data/sorted/"
binw = 200
psthmin = 10
psthmax = 1
event = 4
cueexonly = T
startt = 0
endt = 2000
side = "contra"
basebins = psthmin/(binw/1000)   #number of bins
baseline = seq(1,basebins)


preraw = unineuralhist.rFunc(path = paste(filepath, "unilateral ctap/", sep = ""), startt,
 endt, binw, psthmin, psthmax, event, cueexonly, side)
 
pretestbin = bigbinFR_uni.rFunc(path = paste(filepath, "unilateral ctap/", sep = ""), startt,
 endt, binw, psthmin, psthmax, event, side, cueexonly) 
 
postraw = unineuralhist.rFunc(path = paste(filepath, "unilateral ctap/", sep = ""), startt=2720,
 endt=4720, binw, psthmin, psthmax, event, cueexonly=F, side)
 
posttestbin = bigbinFR_uni.rFunc(path = paste(filepath, "unilateral ctap/", sep = ""), startt=2720,
 endt=4720, binw, psthmin, psthmax, event, side, cueexonly=F) 
 
 

cueexneurons = preraw[[2]]

pretestbin = pretestbin[cueexneurons]
posttestbin = posttestbin[cueexneurons]
 
pre = preraw[[1]]
post = postraw[,cueexneurons] 

#calculates the mean and standard deviation for each neuron
presds = sapply(seq(1, ncol(pre)), function(x) sd(as.numeric(pre[baseline,x])))
premeans = sapply(seq(1, ncol(pre)), function(x) mean(as.numeric(pre[baseline,x])))

#calculates a z-score for each bin for each neuron
prezresp = (pretestbin-premeans)/presds

  
  

#calculates the mean and standard deviation for each neuron
postsds = sapply(seq(1, ncol(post)), function(x) sd(as.numeric(post[baseline,x])))
postmeans = sapply(seq(1, ncol(post)), function(x) mean(as.numeric(post[baseline,x])))

#calculates a z-score for each bin for each neuron
postzresp = (posttestbin-postmeans)/postsds



preiqr = boxplot(prezresp, plot= F)$stats[c(2,4)]
postiqr = boxplot(postzresp, plot= F)$stats[c(2,4)]

par(pty = "s")
plot.new()
plot.window(xlim = c(0,2), ylim = c(0,10))

segments(.5, preiqr[2], .5, preiqr[1], lwd = 2)

segments(1, postiqr[2], 1, postiqr[1], lwd = 2)


points(.5, median(prezresp), col = "blue", pch = 19, cex = 3)
points(1, median(postzresp), col = "firebrick1", pch = 19, cex = 3)

#axis(1, at = c(.5,1), labels = c("Sated", "Restr."), cex.axis = 1.75)
axis(2, at = c(seq(0, 10,5)), las = 2, cex.axis = 1.75, tcl= -.8)

mtext("Z-Score", side = 2, line = 3.5, cex = 1.75)


p=wilcox.test(prezresp, postzresp, paired = T)$p.value
text(paste("p=",p,sep = ""), x = .5, y = 10)

