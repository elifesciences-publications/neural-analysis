load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/KC.sigbins.Rfunc")
load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/KC.sigbins_inhib.Rfunc")

filepath = "C:/Users/Kevin Caref/Google Drive/Opioid Pilot/Neural Data/sorted/"

event = 4
startt = 0
endt = 7200

#excitations
exsatedsigbins = KC.sigbins.Rfunc(path = paste(filepath, "sated response analysis/", sep = ""), startt, endt, event)
 
exrestsigbins = KC.sigbins.Rfunc(path = paste(filepath, "restricted response analysis/", sep = ""), startt=0, endt=7200, event)





#include only significantly excited neurons in the analysis
exsigsated = exsatedsigbins[,which(unlist(lapply(seq(1:ncol(exsatedsigbins)), function(x) length(which(exsatedsigbins[,x] == T)))) != 0)]
exsigrest =  exrestsigbins[,which(unlist(lapply(seq(1:ncol(exrestsigbins)), function(x) length(which(exrestsigbins[,x] == T)))) != 0)]


exsatedpct = sapply(seq(1,nrow(exsigsated)), function(x) {
  length(which(exsigsated[x,] == T))/ncol(exsatedsigbins)
  })
  
exrestpct = sapply(seq(1,nrow(exsigrest)), function(x) {
  length(which(exsigrest[x,] == T))/ncol(exrestsigbins)
  }) 

#inhibitions
insatedsigbins = KC.sigbins_inhib.Rfunc(path = paste(filepath, "sated response analysis/", sep = ""), startt, endt, event)
 
inrestsigbins = KC.sigbins_inhib.Rfunc(path = paste(filepath, "restricted response analysis/", sep = ""), startt=0, endt=7200, event)





#include only significantly excited neurons in the analysis
insigsated = insatedsigbins[,which(unlist(lapply(seq(1:ncol(insatedsigbins)), function(x) length(which(insatedsigbins[,x] == T)))) != 0)]
insigrest =  inrestsigbins[,which(unlist(lapply(seq(1:ncol(inrestsigbins)), function(x) length(which(inrestsigbins[,x] == T)))) != 0)]


insatedpct = sapply(seq(1,nrow(insigsated)), function(x) {
  length(which(insigsated[x,] == T))/ncol(insatedsigbins)
  })
  
inrestpct = sapply(seq(1,nrow(insigrest)), function(x) {
  length(which(insigrest[x,] == T))/ncol(inrestsigbins)
  }) 



xcoords = seq(.025,.475,.05)

par(pty = "s")  
plot.new()
plot.window(xlim = c(0,.5), ylim = c(-50, 50))


lines(xcoords, exsatedpct*100, col = "blue", lwd = 4)
lines(xcoords, exrestpct*100, col = "firebrick1", lwd = 4)

lines(xcoords, -insatedpct*100, col = "blue", lwd = 4)
lines(xcoords, -inrestpct*100, col = "firebrick1", lwd = 4)

axis(1, at= seq(0,.5,.25), cex.axis = 2, pos = 0)
axis(2, at = seq(-50,50,25), labels = c(seq(50,0,-25), seq(25,50,25)), cex.axis = 2, las = 2, tcl = -.8)

mtext("Time (s) since cue onset", side = 1, line = 3, cex = 2)
mtext("% Excited", side = 2, line = 3, cex = 2, at = 25)
mtext("% Inhibited", side = 2, line = 3, cex = 2, at = -25)
mtext("Sated", side = 1, line = -20, at = .4, cex=1.75, col = "blue")
mtext("Restricted", side = 1, line = -18, at = .4, cex=1.75, col="firebrick1")


###stats
strues = sapply(seq(1, nrow(sigsated)), function(x) length(which(sigsated[x,] == T)))
sfalses = sapply(seq(1, nrow(sigsated)), function(x) length(which(sigsated[x,] == F)))
rtrues =  sapply(seq(1, nrow(sigrest)), function(x) length(which(sigrest[x,] == T)))
rfalses = sapply(seq(1, nrow(sigrest)), function(x) length(which(sigrest[x,] == F)))

chidf = lapply(seq(1, length(strues)), function(x) {
   data.frame(sated = c(strues[x], sfalses[x]), restricted = c(rtrues[x], rfalses[x]))
   })

pvals = unlist(lapply(chidf, function(x) {
  chisq.test(x, correct = F)$p.value 
  }))

cpvals = p.adjust(pvals[1:10], method = "bonf")  

sigbarfrom = seq(0,.95,.05)
sigbarto = seq(.05,1,.05)  
 
segments(sigbarfrom[which(cpvals <= .05) ], rep(90, length(which(cpvals <=.05))), sigbarto[which(cpvals <= .05) ], rep(90, length(which(cpvals <=.05))), lwd =2,  cex = 1.5)

#abline(v = xcoords-.025)

####################################################################################################
#instead of all that, just compute, for each cue-excited neuron, the percentage of post-cue bins that show siginifant excitation
#and run a wilcoxon. Then plot the medians and IQRs of each population.
########################################################################################################
satpctexbins = sapply(seq(1:ncol(exsigsated)), function(x) length(which(exsigsated[,x] == T))/nrow(exsigsated))
restpctexbins = sapply(seq(1:ncol(exsigrest)), function(x) length(which(exsigrest[,x] == T))/nrow(exsigrest))

exsatediqr = boxplot(satpctexbins, plot = F)$stats[c(2,4)]
exrestiqr = boxplot(restpctexbins, plot = F)$stats[c(2,4)]

satpctinbins = sapply(seq(1:ncol(insigsated)), function(x) length(which(insigsated[,x] == T))/nrow(insigsated))
restpctinbins = sapply(seq(1:ncol(insigrest)), function(x) length(which(insigrest[,x] == T))/nrow(insigrest))

insatediqr = boxplot(satpctinbins, plot = F)$stats[c(2,4)]
inrestiqr = boxplot(restpctinbins, plot = F)$stats[c(2,4)]

par(pty = "s")
plot.new()
plot.window(xlim = c(0,2), ylim = c(-50,50))
abline(h=0, lwd = 2, col = "gray")

segments(.5, exsatediqr[2]*100, .5, exsatediqr[1]*100, lwd = 2)
segments(1, exrestiqr[2]*100, 1, exrestiqr[1]*100, lwd = 2)

segments(.5, -insatediqr[2]*100, .5, -insatediqr[1]*100, lwd = 2)
segments(1, -inrestiqr[2]*100, 1, -inrestiqr[1]*100, lwd = 2)


points(.5, median(satpctexbins)*100, col = "blue", pch = 19, cex = 3)
points(1, median(restpctexbins)*100, col = "firebrick1", pch = 19, cex = 3)


points(.5, -median(satpctinbins)*100, col = "blue", pch = 19, cex = 3)
points(1, -median(restpctinbins)*100, col = "firebrick1", pch = 19, cex = 3)

#axis(1, at = c(.5,1), labels = c("Sated", "Restr."), cex.axis = 1.75)
axis(2, at = seq(-50,50,25), labels = c(seq(50,0,-25), seq(25,50,25)), cex.axis = 2, las = 2, tcl = -.8)

mtext("% Bins Excited", side = 2, line = 3.5, cex = 1.75, at = 25)
mtext("% Bins Inhibited", side = 2, line = 3.5, cex = 1.75, at = -25)

pex = wilcox.test(satpctexbins, restpctexbins, paired = F)$p.value
pin = wilcox.test(satpctinbins, restpctinbins, paired = F)$p.value

text(paste("p=", pex, sep = ""), x=.5,y=25)
text(paste("p=", pin, sep = ""), x=.5,y=-25)












