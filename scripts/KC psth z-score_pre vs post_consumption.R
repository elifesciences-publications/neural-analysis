library(matrixStats)
load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/neuralhist.rFunc")

filepath = "C:/Users/Kevin Caref/Google Drive/Opioid Pilot/Neural Data/sorted/"
binw = 50
psthmin = 5
psthmax = 5
event = 6
cueexonly = F
startt = 0
endt = 2000
basebins = 20   #number of bins
baseline = seq(1,basebins)

pre = neuralhist.rFunc(path = paste(filepath, "restricted ctap/", sep = ""), startt,
 endt, binw, psthmin, psthmax, event, cueexonly)
 

post = neuralhist.rFunc(path = paste(filepath, "restricted ctap/", sep = ""), startt=2720,
 endt=4720, binw, psthmin, psthmax, event, cueexonly=F)

if(cueexonly == T) ({
post = post[,pre[[2]]]
pre = pre[[1]][,pre[[2]]]
})

#calculates the mean and standard deviation for each neuron
presds = sapply(seq(1, ncol(pre)), function(x) sd(as.numeric(pre[baseline,x])))
premeans = sapply(seq(1, ncol(pre)), function(x) mean(as.numeric(pre[baseline,x])))

#calculates a z-score for each bin for each neuron
prezresp = as.matrix(sapply(seq(1, ncol(pre)), function(x)
  (as.numeric(pre[,x])-premeans[x])/presds[x]))
  
  

#calculates the mean and standard deviation for each neuron
postsds = sapply(seq(1, ncol(post)), function(x) sd(as.numeric(post[baseline,x])))
postmeans = sapply(seq(1, ncol(post)), function(x) mean(as.numeric(post[baseline,x])))

#calculates a z-score for each bin for each neuron
postzresp = as.matrix(sapply(seq(1, ncol(post)), function(x)
  (as.numeric(post[,x])-postmeans[x])/postsds[x]))




meanprefiring = rowMeans(prezresp)
meanpostfiring = rowMeans(postzresp)


preerror = rowSds(prezresp)/sqrt(ncol(prezresp))
posterror = rowSds(postzresp)/sqrt(ncol(postzresp))



xcoords = seq(-psthmin+(binw/1000)/2, psthmax-(binw/1000)/2, binw/1000)


par(pty = "s")
plot.new()
plot.window(xlim = c(-5,5), ylim = c(0, 5))
#rect(.1, -5, .3, 25, lwd =0, col = "gray84", border = "transparent")
lines(xcoords, meanprefiring, col = "blue", lwd = 3)
lines(xcoords, meanpostfiring, col= "red", lwd =3)

axis(1, at = c(seq(-5, 5, 5)), cex.axis=2, tcl = -.8)
axis(2, at = c(seq(0, 5, 5)),cex.axis=2, las=2, tcl = -.8)
abline(v=0, col = "gray", lwd = 2)


#error cloud for pre firing
testcol=c(as.vector (col2rgb("blue",alpha=F)[,1]))
polcol=rgb(testcol[1],testcol[2],testcol[3],100, names = NULL, maxColorValue = 255)
polygon(c(xcoords,rev(xcoords)), c(meanprefiring+preerror, rev(meanprefiring-preerror)), density=-20, col= polcol,border=NA)

#error cloud for post firing
testcol=c(as.vector (col2rgb("red",alpha=F)[,1]))
polcol=rgb(testcol[1],testcol[2],testcol[3],100, names = NULL, maxColorValue = 255)
polygon(c(xcoords,rev(xcoords)), c(meanpostfiring+posterror, rev(meanpostfiring-posterror)), density=-20, col= polcol,,border=NA)

mtext("Time (s)", side = 1, line = 3, cex= 2)
mtext("Z-score", side = 2, line = 3, cex=2)
mtext("Sated pre", side = 1, line = -20, at = -.3, cex=1.75, col = "blue")
mtext("Sated post", side = 1, line = -18, at = -.3, cex=1.75, col="firebrick1")


##stats
pvals = sapply(which(xcoords >= 0 & xcoords <=5), function(x) wilcox.test(x=as.numeric(prezresp[x,]), y=as.numeric(postzresp[x,]), paired = T, correct = F)$p.value)
sigps = pvals[which(pvals <= .05)]
text(xcoords[which(pvals <= .05)+length(which(xcoords<.1)) ], rep(30, length(sigps)), labels = "_", cex = 1.5)


cpvals = p.adjust(pvals, method = "bonf")

sigbarfrom = seq(-psthmin,psthmax-(binw/1000),binw/1000)[which(xcoords >= 0 & xcoords <=.3)]
sigbarto = seq(-psthmin+(binw/1000),psthmax,binw/1000)[which(xcoords >= 0 & xcoords <=.3)]  
 
segments(sigbarfrom[which(cpvals <= .05) ], rep(5, length(which(cpvals <=.05))), sigbarto[which(cpvals <= .05) ], rep(5, length(which(cpvals <=.05))), lwd =2,  cex = 1.5)



#sum(diff(xcoords[501:550]) * (head(meansatedfiring[501:550],-1)+tail(meansatedfiring[501:550],-1)))/2
#sum(diff(xcoords[501:550]) * (head(meanrestrfiring[501:550],-1)+tail(meanrestrfiring[501:550],-1)))/2





