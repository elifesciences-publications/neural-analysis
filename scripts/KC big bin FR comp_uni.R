bigbinFR_uni.rFunc = function(path, startt, endt, binw, psthmin, psthmax, event, side, cueexonly) { 

load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/unineuraldataextract.rFunc")
load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/eventfinder.rFunc")
load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/KC.pois.detect.Rfunc")

#specify path of nex files
neuralpath = path
nexfiles = list.files(neuralpath)

#extracts all NeuroExplorer data
nexdata = lapply(nexfiles, function(x) {
  unineuraldataextract.rFunc(paste(neuralpath, x, sep = ""))})
  
unitype = vector() 
unitype = sapply(seq(1, length(nexdata)), function(x) {
  if(grepl("left", nexdata[[x]]$expt) == T) (unitype[x] = "L")
  else(unitype[x] = "R")
  })

  

#these two functions split up the neurons and the behavioral events for each experiment
neurons = lapply(seq(1, length(nexdata)), function(x) {
  neuronidx = grep("sig", names(nexdata[[x]]))
  nexdata[[x]][neuronidx] })

neuronnames = lapply(seq(1, length(neurons)), function(x) {
  names(neurons[[x]])})

left = c("001", "002","003","004","005","006","007","008")
right = c("009", "010","011","012","013","014","015","016")

leftflag = lapply(seq(1, length(neurons)), function(x) {
  unlist(lapply(left, function(y) {
    grep(y, neuronnames[[x]])
    }))
  })
  
rightflag = lapply(seq(1, length(neurons)), function(x) {
  unlist(lapply(right, function(y) {
    grep(y, neuronnames[[x]])
    }))
  })     

ipsi = list()  
ipsiidx = lapply(seq(1, length(neurons)), function(x) {
  if(unitype[x] == "L") (ipsi = leftflag[[x]])
  else(ipsi = rightflag[[x]])
  })

contra = list()
contraidx = lapply(seq(1, length(neurons)), function(x) {
  if(unitype[x] == "L") (contra = rightflag[[x]])
  else(contra = leftflag[[x]])
  })   
  
events = eventfinder.rFunc(nexdata, event)



 


#specify the interval of timestamps you want to examine  
sstart = startt
send = endt

#specify whether to only return cue excited neurons
    

#########################################################
#these two functions simply restrict the timestamp values
#to the window specified above
#######################################################
neurons = lapply(seq(1, length(neurons)), function(x) {
  lapply(seq(1, length(neurons[[x]])), function(y) {
    neurons[[x]][[y]][which(neurons[[x]][[y]] >= sstart & neurons[[x]][[y]] <= send)]
   })
  })

events = lapply(seq(1, length(events)), function(x) {
   events[[x]][which(events[[x]] >= sstart & events[[x]] <= send)] })
   
   
 

#####################################################################################
#These functions pull out only the neurons of interest: ipsilateral or contralateral
######################################################################################
if(side == "ipsi") ({
  neurons = lapply(1:length(neurons), function(x) {
    neurons[[x]][ipsiidx[[x]]]
    })
    })

if(side == "contra") ({    
  neurons = lapply(1:length(neurons), function(x) {
    neurons[[x]][contraidx[[x]]]
    })
    })
    

  
spikelist = list()  
for(i in 1:(length(neurons))) ({
  spikelist = c(spikelist, neurons[[i]])
  })
  
allevents = lapply(seq(1, length(events)), function(x) {
  rep(events[x], length(neurons[[x]]))
  })
  
eventslist = list()  
for(i in 1:(length(allevents))) ({
  eventslist = c(eventslist, allevents[[i]])
  })  


#winminbase = .2
#winmaxbase = 0

#min here is additive; e.g. need to find spike counts between 50 and 250 ms after cue onset
winmintest = .1 #in s
winmaxtest = .3 #in s

binsize = winmaxtest-winmintest

#####################################################################
#This function returns a list of lists of lists:
#Level 1 is the experiment; Level 2 is each neuron in the experiment;
#Level 3 is the time relative to the specified event for each trial
#The object masterlist can then be used to construct PSTHs or rasters
########################################################################
masterlisttest = lapply(seq(1, length(spikelist)), function(x) {
 lapply(seq(1, length(eventslist[[x]])), function(y) {
  stampsidx = which(spikelist[[x]] >= eventslist[[x]][y] + winmintest & spikelist[[x]] <= eventslist[[x]][y] + winmaxtest)
  relstamps = spikelist[[x]][stampsidx] - eventslist[[x]][y]
   })
  }) 

#masterlistbase = lapply(seq(1, length(spikes)), function(x) {
# lapply(seq(1, length(events[[x]])), function(y) {
#  stampsidx = which(spikes[[x]] >= events[[x]][y] - winminbase & spikes[[x]] <= events[[x]][y] + winmaxbase)
#  relstamps = spikes[[x]][stampsidx] - events[[x]][y]
#   })
#  })    
#
                                                                        
#######################################################################
#This function returns a list of lists:
#Level 1 is the experiment
#Level 2 contains the the frequency histogram (PSTH)
#for each neuron for the event specified above.
#
#The object neurohist can then be used to plot histograms individually or
#averaged together as in the next function.
#########################################################################
FRtest = lapply(seq(1, length(masterlisttest)), function(x) {
   allvals = unlist(masterlisttest[[x]])
   #hcounts = hist(allvals, breaks = seq(winmintest, winmaxtest, binsize), plot = F)$counts 
   freq = (length(allvals)/binsize)/length(masterlisttest[[x]])
   })
   
   
#FRbase = lapply(seq(1, length(masterlistbase)), function(x) {
#  lapply(seq(1:length(masterlistbase[[x]])), function(y) {
#   length(masterlistbase[[x]][[y]])
#   allvals = unlist(masterlistbase[[x]])
#   hcounts = hist(allvals, breaks = seq(-winminbase, winmaxbase, binsize/1000), plot = F)$counts 
#   freq = (hcounts/(binsize/1000))/length(masterlistbase[[x]])
#   })
#   })   


return(unlist(FRtest))
}

save(bigbinFR_uni.rFunc, file = "C:/Users/Kevin Caref/Google Drive/RScripts/Functions/bigbinFR_uni.rFunc") 


    
    
    
    
