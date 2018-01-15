getspikes.rFunc = function(path) { 

#path = "C:/Users/Kevin Caref/Google Drive/Opioid Pilot/Neural Data/sorted/restricted ctap/"

load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/neuraldataextract.rFunc")
load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/eventfinder.rFunc")
#load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/KC.pois.detect.Rfunc")

#specify path of nex files
neuralpath = path
nexfiles = list.files(neuralpath)

#extracts all NeuroExplorer data
nexdata = lapply(nexfiles, function(x) {
  neuraldataextract.rFunc(paste(neuralpath, x, sep = ""))})
  
#for debugging purposes
#nexdata = list()
#for(i in (1:length(nexfiles))) (
#   nexdata[[i]] = neuraldataextract.rFunc(paste(neuralpath, nexfiles[i], sep = ""))) 

#these two functions split up the neurons and the behavioral events for each experiment
neurons = lapply(seq(1, length(nexdata)), function(x) {
  neuronidx = grep("sig", names(nexdata[[x]]))
  nexdata[[x]][neuronidx] })
  
#events = eventfinder.rFunc(nexdata, event)

#if some nex files have no neurons, remove them here
torem = which(unlist(lapply(neurons, function(x) length(x))) == 0)
if(length(torem) != 0) ({
neurons = neurons[-torem]
#events =  events[-torem]
nexdata = nexdata[-torem]
})

#specify the interval of timestamps you want to examine  
#sstart = startt
#send = endt

#specify whether to only return cue excited neurons
    

#########################################################
#these two functions simply restrict the timestamp values
#to the window specified above
#######################################################
#neurons = lapply(seq(1, length(neurons)), function(x) {
#  lapply(seq(1, length(neurons[[x]])), function(y) {
#    neurons[[x]][[y]][which(neurons[[x]][[y]] >= sstart & neurons[[x]][[y]] <= send)]
#   })
#  })

#events = lapply(seq(1, length(events)), function(x) {
#   events[[x]][which(events[[x]] >= sstart & events[[x]] <= send)] })
   
spikelist = list()  
for(i in 1:(length(neurons))) ({
  spikelist = c(spikelist, neurons[[i]])
  })  



#allevents = lapply(seq(1, length(events)), function(x) {
#  rep(events[x], length(neurons[[x]]))
#  })
#  
#eventslist = list()  
#for(i in 1:(length(allevents))) ({
#  eventslist = c(eventslist, allevents[[i]])
#  })     
 
return(spikelist)
}

save(getspikes.rFunc, file = "C:/Users/Kevin Caref/Google Drive/RScripts/Functions/getspikes.rFunc") 
 
 
  
   
  
