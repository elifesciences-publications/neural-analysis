load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/KC.sigbins_cons.Rfunc")
load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/KC.sigbins_inhib_cons.Rfunc")

#load the vector of indeces of non-injected sated neurons that have no trials in the 'post' epoch
load("C:/Users/Kevin Caref/Google Drive/Opioid Pilot/rdat/sated_torem.rdat")

filepath = "C:/Users/Kevin Caref/Google Drive/Opioid Pilot/Neural Data/sorted/"

#event = 6
startt = 0
endt = 2000
binw = 400
psthmax=4


#find all excited bins
expresigbins = KC.sigbins_cons.Rfunc(path = paste(filepath, "sated response analysis/", sep = ""), startt, endt)[,-torem]
 
expostsigbins = KC.sigbins_cons.Rfunc(path = paste(filepath, "sated response analysis/", sep = ""), startt=2720, endt=4720)[,-torem]


excueexneuronspre = which(unlist(lapply(seq(1:ncol(expresigbins)), function(x) length(which(expresigbins[,x] == T)))) != 0)
excueexneuronspost = which(unlist(lapply(seq(1:ncol(expostsigbins)), function(x) length(which(expostsigbins[,x] == T)))) != 0)


#include only significantly excited neurons in the analysis
exsigpre = expresigbins[,which(unlist(lapply(seq(1:ncol(expresigbins)), function(x) length(which(expresigbins[,x] == T)))) != 0)]
exsigpost = expostsigbins[,which(unlist(lapply(seq(1:ncol(expresigbins)), function(x) length(which(expresigbins[,x] == T)))) != 0)]  #must look at same neurons
#sigpost =  postsigbins[,which(unlist(lapply(seq(1:ncol(postsigbins)), function(x) length(which(postsigbins[,x] == T)))) != 0)]


exprepct = sapply(seq(1,nrow(exsigpre)), function(x) {
  length(which(exsigpre[x,] == T))/ncol(exsigpre)
  })

#needs to be divided by the same number of neurons as pre injection  
expostpct = sapply(seq(1,nrow(exsigpost)), function(x) {
  length(which(exsigpost[x,] == T))/ncol(exsigpre)
  }) 


kcsmooth = function(x, window) {
    unlist(lapply(seq(1, length(x)), function(y) {
      mean(x[seq(y, (y+window)-1)], na.rm = T)       
      }))
      }

expresmooth = kcsmooth(exprepct, window = 4)
expostsmooth = kcsmooth(expostpct, window = 4)  

#find all inhibited bins
inpresigbins = KC.sigbins_inhib_cons.Rfunc(path = paste(filepath, "sated response analysis/", sep = ""), startt, endt)[,-torem]
 
inpostsigbins = KC.sigbins_inhib_cons.Rfunc(path = paste(filepath, "sated response analysis/", sep = ""), startt=2720, endt=4720)[,-torem]


incueexneuronspre = which(unlist(lapply(seq(1:ncol(inpresigbins)), function(x) length(which(inpresigbins[,x] == T)))) != 0)
incueexneuronspost = which(unlist(lapply(seq(1:ncol(inpostsigbins)), function(x) length(which(inpostsigbins[,x] == T)))) != 0)


#include only significantly excited neurons in the analysis
insigpre = inpresigbins[,which(unlist(lapply(seq(1:ncol(inpresigbins)), function(x) length(which(inpresigbins[,x] == T)))) != 0)]
insigpost =  inpostsigbins[,which(unlist(lapply(seq(1:ncol(inpresigbins)), function(x) length(which(inpresigbins[,x] == T)))) != 0)]  #must look at same neurons
#sigpost =  postsigbins[,which(unlist(lapply(seq(1:ncol(postsigbins)), function(x) length(which(postsigbins[,x] == T)))) != 0)]


inprepct = sapply(seq(1,nrow(insigpre)), function(x) {
  length(which(insigpre[x,] == T))/ncol(insigpre)
  })

#needs to be divided by the same number of neurons as pre injection  
inpostpct = sapply(seq(1,nrow(insigpost)), function(x) {
  length(which(insigpost[x,] == T))/ncol(insigpre)
  }) 


#xcoords = seq(.025,4.475,.05)



kcsmooth = function(x, window) {
    unlist(lapply(seq(1, length(x)), function(y) {
      mean(x[seq(y, (y+window)-1)], na.rm = T)       
      }))
      }

inpresmooth = kcsmooth(inprepct, window = 4)
inpostsmooth = kcsmooth(inpostpct, window = 4) 


xcoords = seq(0, psthmax-(binw/1000)/2, binw/1000)     

par(pty = "s")  
plot.new()
plot.window(xlim = c(0,3), ylim = c(-50, 50))


#lines(xcoords, prepct*100, col = "blue", lwd = 4)
#lines(xcoords, postpct*100, col = "firebrick1", lwd = 4)

lines(xcoords, expresmooth*100, col = "blue", lwd = 4)
lines(xcoords, expostsmooth*100, col = "firebrick1", lwd = 4)

lines(xcoords, -inpresmooth*100, col = "blue", lwd = 4)
lines(xcoords, -inpostsmooth*100, col = "firebrick1", lwd = 4)

axis(1, at= seq(0,3,1), cex.axis = 2, pos = 0)
axis(2, at = seq(-50,50,25), labels = c(seq(50,0,-25), seq(25,50,25)), cex.axis = 2, las = 2, tcl = -.8)

mtext("Time (s) since rewarded entry", side = 1, line = 3, cex = 2)
mtext("% Excited", side = 2, line = 3, cex = 2, at = 25)
mtext("% Inhibited", side = 2, line = 3, cex = 2, at = -25)
mtext(paste("pre", sep = "-"), side = 1, line = -20, at = .4, cex=1.75, col = "blue")
mtext(paste("post", sep = "-"), side = 1, line = -18, at = .4, cex=1.75, col="firebrick1")


####stats
#strues = sapply(seq(1, nrow(sigpre)), function(x) length(which(sigpre[x,] == T)))
#sfalses = sapply(seq(1, nrow(sigpre)), function(x) length(which(sigpre[x,] == F)))
#rtrues =  sapply(seq(1, nrow(sigpost)), function(x) length(which(sigpost[x,] == T)))
#
##falses must be computed differently; trues must be subracted from the number of pre-injected excitations
#rfalses = ncol(sigpre) - rtrues
#
#
##rfalses = sapply(seq(1, nrow(sigrest)), function(x) length(which(sigrest[x,] == F)))
#
#chidf = lapply(seq(1, length(strues)), function(x) {
#   data.frame(sated = c(strues[x], sfalses[x]), restricted = c(rtrues[x], rfalses[x]))
#   })
#
#pvals = unlist(lapply(chidf, function(x) {
#  chisq.test(x, correct = F)$p.value 
#  }))
#
#cpvals = p.adjust(pvals[1:10], method = "holm")  
#
#sigbarfrom = seq(0,.95,.05)
#sigbarto = seq(.05,1,.05)  
# 
#segments(sigbarfrom[which(cpvals <= .05) ], rep(90, length(which(cpvals <=.05))), sigbarto[which(cpvals <= .05) ], rep(90, length(which(cpvals <=.05))), lwd =2,  cex = 1.5)


####################################################################################################
#instead of all that, just compute, for each cue-excited neuron, the percentage of post-cue bins that show siginifant excitation
#and run a wilcoxon. Then plot the medians and IQRs of each population.
########################################################################################################
prepctexbins = sapply(seq(1:ncol(exsigpre)), function(x) length(which(exsigpre[1:8,x] == T))/8)   #divide here by number of bins in 3 s
postpctexbins = sapply(seq(1:ncol(exsigpost)), function(x) length(which(exsigpost[1:8,x] == T))/8)   #divide here by number of bins in 3 s

prepctinbins = sapply(seq(1:ncol(insigpre)), function(x) length(which(insigpre[1:8,x] == T))/8)   #divide here by number of bins in 3 s
postpctinbins = sapply(seq(1:ncol(insigpost)), function(x) length(which(insigpost[1:8,x] == T))/8)   #divide here by number of bins in 3 s

expreiqr = boxplot(prepctexbins, plot = F)$stats[c(2,4)]
expostiqr = boxplot(postpctexbins, plot = F)$stats[c(2,4)]

inpreiqr = boxplot(prepctinbins, plot = F)$stats[c(2,4)]
inpostiqr = boxplot(postpctinbins, plot = F)$stats[c(2,4)]

par(pty = "s")
plot.new()
plot.window(xlim = c(0,2), ylim = c(-50,50))

abline(h=0, lwd = 2, col = "gray")

segments(.5, expreiqr[2]*100, .5, expreiqr[1]*100, lwd = 2)
segments(1, expostiqr[2]*100, 1, expostiqr[1]*100, lwd = 2)

segments(.5, -inpreiqr[2]*100, .5, -inpreiqr[1]*100, lwd = 2)
segments(1, -inpostiqr[2]*100, 1, -inpostiqr[1]*100, lwd = 2)

points(.5, median(prepctexbins)*100, col = "blue", pch = 19, cex = 3)
points(1, median(postpctexbins)*100, col = "firebrick1", pch = 19, cex = 3)

points(.5, -median(prepctinbins)*100, col = "blue", pch = 19, cex = 3)
points(1, -median(postpctinbins)*100, col = "firebrick1", pch = 19, cex = 3)

#axis(1, at = c(.5,1), labels = c("Sated", "Restr."), cex.axis = 1.75)

axis(2, at = c(seq(-50, 50,25)), las = 2, labels = c(seq(50,0,-25), seq(25,50,25)), cex.axis = 1.75, tcl= -.8)

mtext("% Bins Excited", side = 2, line = 3.5, cex = 1.75, at = 25)
mtext("% Bins Inhibited", side = 2, line = 3.5, cex = 1.75, at = -25)

pex = wilcox.test(prepctexbins, postpctexbins, paired = T)$p.value
pin = wilcox.test(prepctinbins, postpctinbins, paired = T)$p.value

text(paste("p=", pex, sep = ""), x=.5,y=25)
text(paste("p=", pin, sep = ""), x=.5,y=-25)


