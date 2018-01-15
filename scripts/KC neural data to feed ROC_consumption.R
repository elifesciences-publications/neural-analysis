

path = "C:/Users/Kevin Caref/Google Drive/Opioid Pilot/Neural Data/sorted/sated response analysis/"

load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/neuraldataextract.rFunc")
load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/eventfinder.rFunc")

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

spikelist = list()  
for(i in 1:(length(neurons))) ({
  spikelist = c(spikelist, neurons[[i]])
  })

  
events = eventfinder.rFunc(nexdata, event=6)

allevents = lapply(seq(1, length(events)), function(x) {
  rep(events[x], length(neurons[[x]]))
  })
  
eventslist = list()  
for(i in 1:(length(allevents))) ({
  eventslist = c(eventslist, allevents[[i]])
  })
  
events = eventfinder.rFunc(nexdata, event=4)

allevents = lapply(seq(1, length(events)), function(x) {
  rep(events[x], length(neurons[[x]]))
  })
  
baselist = list()  
for(i in 1:(length(allevents))) ({
  baselist = c(baselist, allevents[[i]])
  })  
 



    
