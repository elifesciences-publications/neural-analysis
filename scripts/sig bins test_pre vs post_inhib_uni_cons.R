load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/KC.sigbins_inhib_uni_cons.Rfunc")

filepath = "C:/Users/Kevin Caref/Google Drive/Opioid Pilot/Neural Data/sorted/"

event = 6
startt = 0
endt = 2000
side = "contra"
binw = 400
psthmax=4

presigbins = KC.sigbins_inhib_uni_cons.Rfunc(path = paste(filepath, "unilateral ctap/", sep = ""), startt, endt, event, side)
 
postsigbins = KC.sigbins_inhib_uni_cons.Rfunc(path = paste(filepath, "unilateral ctap/", sep = ""), startt=2720, endt=4720, event, side)


cueexneuronspre = which(unlist(lapply(seq(1:ncol(presigbins)), function(x) length(which(presigbins[,x] == T)))) != 0)
cueexneuronspost = which(unlist(lapply(seq(1:ncol(postsigbins)), function(x) length(which(postsigbins[,x] == T)))) != 0)


#include only significantly excited neurons in the analysis
sigpre = presigbins[,which(unlist(lapply(seq(1:ncol(presigbins)), function(x) length(which(presigbins[,x] == T)))) != 0)]
sigpost =  postsigbins[,which(unlist(lapply(seq(1:ncol(presigbins)), function(x) length(which(presigbins[,x] == T)))) != 0)]  #must look at same neurons
#sigpost =  postsigbins[,which(unlist(lapply(seq(1:ncol(postsigbins)), function(x) length(which(postsigbins[,x] == T)))) != 0)]


prepct = sapply(seq(1,nrow(sigpre)), function(x) {
  length(which(sigpre[x,] == T))/ncol(sigpre)
  })

#needs to be divided by the same number of neurons as pre injection  
postpct = sapply(seq(1,nrow(sigpost)), function(x) {
  length(which(sigpost[x,] == T))/ncol(sigpre)
  }) 


#xcoords = seq(.025,4.475,.05)

xcoords = seq(0+(binw/1000)/2, psthmax-(binw/1000)/2, binw/1000)

kcsmooth = function(x, window) {
    unlist(lapply(seq(1, length(x)), function(y) {
      mean(x[seq(y, (y+window)-1)], na.rm = T)       
      }))
      }

presmooth = kcsmooth(prepct, window = 5)
postsmooth = kcsmooth(postpct, window = 5)      

par(pty = "s")  
plot.new()
plot.window(xlim = c(0,3), ylim = c(0, 50))


#lines(xcoords, prepct*100, col = "blue", lwd = 4)
#lines(xcoords, postpct*100, col = "firebrick1", lwd = 4)

lines(xcoords, presmooth*100, col = "blue", lwd = 4)
lines(xcoords, postsmooth*100, col = "firebrick1", lwd = 4)

axis(1, at= seq(0,3,1), cex.axis = 2)
axis(2, at = seq(0,50,25), cex.axis = 2, las = 2, tcl = -.8)

mtext("Time (s) since cue onset", side = 1, line = 3, cex = 2)
mtext("% Inhibited Neurons", side = 2, line = 3, cex = 2)
mtext(paste(side,"pre", sep = "-"), side = 1, line = -20, at = .4, cex=1.75, col = "blue")
mtext(paste(side,"post", sep = "-"), side = 1, line = -18, at = .4, cex=1.75, col="firebrick1")


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
prepctexbins = sapply(seq(1:ncol(sigpre)), function(x) length(which(sigpre[,x] == T))/nrow(sigpre))
postpctexbins = sapply(seq(1:ncol(sigpost)), function(x) length(which(sigpost[,x] == T))/nrow(sigpost))

preiqr = boxplot(prepctexbins, plot = F)$stats[c(2,4)]
postiqr = boxplot(postpctexbins, plot = F)$stats[c(2,4)]

par(pty = "s")
plot.new()
plot.window(xlim = c(0,2), ylim = c(0,50))

segments(.5, preiqr[2]*100, .5, preiqr[1]*100, lwd = 2)

segments(1, postiqr[2]*100, 1, postiqr[1]*100, lwd = 2)


points(.5, median(prepctexbins)*100, col = "blue", pch = 19, cex = 3)
points(1, median(postpctexbins)*100, col = "firebrick1", pch = 19, cex = 3)

#axis(1, at = c(.5,1), labels = c("Sated", "Restr."), cex.axis = 1.75)
axis(2, at = c(seq(0, 50,25)), las = 2, cex.axis = 1.75, tcl= -.8)

mtext("% Bins Inhibited", side = 2, line = 3.5, cex = 1.75)


p = wilcox.test(prepctexbins, postpctexbins, paired = T)$p.value

text(paste("p=", p, sep = ""), x=.5,y=25)


