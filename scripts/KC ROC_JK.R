wd="C:/Users/Kevin Caref/Google Drive/Opioid Pilot/kpipeline/ROC/"
datdirect=paste (wd,"/",sep="")
funcdirect=paste (wd,"/functions/",sep="")

#load(paste(datdirect,"spikelist.rdat",sep=""))
#load(paste(datdirect,"eventslist.rdat",sep=""))

load(paste(funcdirect,"Rocparse.Rfunc",sep=""))

#########################################################
cuelength=5 
lower=2720  
upper=4720

intervalwindow=2    #this sets the length of baseline available for analysis; original value was 1
binsize = 10         #original value was 10
slidingbinsize=200   #original value was 200
#plotwindow=1000 #was 2000-determines how much time (perievent) is plotted
howmuchbaseline=500 #was 500-determines how much baseline is used in the analysis.units are ms i.e. 500=500ms      KC original value is 1000

#set plot dimensions here
plotwinmin = .25
plotwinmax = 1


##############################
library(matlab)
library(pROC)
library(fields)
library(Hmisc)
library(mixOmics)
############################################
#k=1

ROCdf = vector()
for(k in 1:length(spikelist)) ({
    
    #LIST DIVE
    NeuralFiring=spikelist[[k]]
    currentbehavior=eventslist[[k]]
    
    #Apply time condition
    timecondition = ((currentbehavior >lower) & (currentbehavior <upper))     
    event0time = currentbehavior [timecondition]   
    
    NeuralFiringINT = findInterval(NeuralFiring, event0time - intervalwindow)
    NeuralFiringABS = NeuralFiring - c(rep(0, sum(NeuralFiringINT == 0)), event0time[NeuralFiringINT])
    eventFiring = data.frame(times = NeuralFiring, trialnum = NeuralFiringINT, abstimes = NeuralFiringABS)
    eventFiring = eventFiring[-which((eventFiring$trialnum == 0) | (eventFiring$abstimes > intervalwindow)), ]
    binnames = c(seq(-intervalwindow, intervalwindow, binsize/1000))   
    eventFiring$binnum = findInterval(eventFiring$abstimes, binnames)

   
    alltrials=matrix(0,nrow=length(binnames),ncol=length(event0time))
    binrle = rle(paste(eventFiring$trialnum,eventFiring$binnum)) 
    binrleidx = matrix(as.numeric(unlist(strsplit(binrle$values, " "))),ncol =2,byrow = T)
    binrleidx = ((binrleidx[,1]-1)*length(binnames))+binrleidx[,2]
    alltrials[binrleidx]= binrle$lengths/(binsize/1000)            #Number of spikes per bin / number of seconds per bin. Gives spikes/s
    
    
    baselineindx = which(binnames>=(-howmuchbaseline/1000) & (binnames<0))
    
    roc1=colMeans (alltrials[baselineindx,])
    
      ###calculateROC
    
    slidingwindow = slidingbinsize/binsize
  
    seed=matrix(seq(1,slidingwindow,1), nrow = length(binnames), ncol = slidingwindow, byrow = T) + seq(0,length(binnames)-1,1)
    seed=seed[-unique(row(seed)[(which(seed>length(binnames)))]),]
    
    
    roc2=apply(seed,1, function(x) colMeans(alltrials[x,],na.rm = T))
      
          
      ##for inhibitions, choose >; for excitations, choose <
    toplotindx = apply(roc2,2,function (y) auc(controls = roc1,cases = y ,direction="<",partial.auc.correct=FALSE))
    
   
    ROCdf = cbind(ROCdf, toplotindx)

})





#calculates location of the event within the specified plot window
eventbin = (nrow(ROCdf)-(intervalwindow/(binsize/1000)))
plotwin = seq(eventbin- (plotwinmin/(binsize/1000)), eventbin + (plotwinmax/(binsize/1000))-1)
timezero = (which(plotwin == eventbin)-1)/length(plotwin)

#############################################################################################
#sorts neurons by response magnitiude by averaging the 200 ms window from 50-250 ms after event onset
peaksort = sort(colMeans(ROCdf[seq(eventbin+15,eventbin+30),]), decreasing = F, index.return = T)$ix
##############################################################################################


ROCtoplot = ROCdf[,peaksort]

allbrks = seq(0,1,.01)


#generates colors
jetcol = color.jet(length(allbrks) -1) #need one more break than color


#plots heat map
par(pty = "s")
image(ROCtoplot[plotwin,], col = jetcol, xaxt = "n", yaxt = "n", breaks = allbrks)


abline(v=timezero, col = "black", lwd = 2)


axis(1, at = seq(0,1,timezero), labels = seq(-plotwinmin,plotwinmax,plotwinmin), cex.axis = 1.75, tcl = -.8)    
axis(2, at = (c(0,1)), labels = c(1,ncol(ROCtoplot)), cex.axis = 1.75, las = 2, tcl = -.8)
mtext("Time (s)", side= 1, cex = 2,line = 3)
mtext("Neuron #", side= 2, cex = 2,line = 1)

 
 
#plots heat map legend
#image.plot(ROCtoplot, nlevel = 256, col = jetcol, legend.lab = "auROC", xaxt = "n", yaxt = "n", breaks = allbrks, legend.only = T, cex.axis = 1.5)







