library(matrixStats)
load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/neuralhist.rFunc")
load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/bigbinFR.rFunc")

filepath = "C:/Users/Kevin Caref/Google Drive/Opioid Pilot/Neural Data/sorted/"
binw = 200
psthmin = 10
psthmax = 1
event = 1
cueexonly = T
startt = 0
endt = 2000
basebins = psthmin/(binw/1000)   #number of bins
baseline = seq(1,basebins)

pre = neuralhist.rFunc(path = paste(filepath, "sated response analysis/", sep = ""), startt,
 endt, binw, psthmin, psthmax, event, cueexonly)
 
pretestbin = bigbinFR.rFunc(path = paste(filepath, "sated response analysis/", sep = ""), startt,
 endt, binw, psthmin, psthmax, event, cueexonly=F)
 
pretestbin = pretestbin[pre[[2]]]  
 

post = neuralhist.rFunc(path = paste(filepath, "sated response analysis/", sep = ""), startt=2720,
 endt=4720, binw, psthmin, psthmax, event, cueexonly=F)
 
posttestbin = bigbinFR.rFunc(path = paste(filepath, "sated response analysis/", sep = ""), startt=2720,
 endt=4720, binw, psthmin, psthmax, event, cueexonly=F) 

posttestbin = posttestbin[pre[[2]]]


post = post[,pre[[2]]]
pre = pre[[1]][,pre[[2]]]

##for sated response analysis
#pre = pre[,-c(2,8,9,10,11,12,18,19,20,23)]  #computed by finding which(pre[[2]] %in% torem) 
#post = post[,-c(2,8,9,10,11,12,18,19,20,23)]
#pretestbin = pretestbin[-c(2,8,9,10,11,12,18,19,20,23)] 
#posttestbin = posttestbin[-c(2,8,9,10,11,12,18,19,20,23)] 


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

#detect any missing values for sessions with no trials
#torem = which(is.na(restzresp[1,]))


meanprefiring = mean(prezresp)
meanpostfiring = mean(postzresp)


preerror = sd(prezresp)/sqrt(length(prezresp))
posterror = sd(postzresp)/sqrt(length(postzresp))

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


p = wilcox.test(prezresp, postzresp, paired = T)$p.value

text(paste("p=",p,sep = ""), x = .5, y = 10)





