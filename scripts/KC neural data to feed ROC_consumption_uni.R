event = 6
side = "contra"

path = "C:/Users/Kevin Caref/Google Drive/Opioid Pilot/Neural Data/sorted/unilateral ctap/"

load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/unineuraldataextract.rFunc")
load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/eventfinder.rFunc")

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

ipsilist = lapply(seq(1, length(neurons)), function(x) {
  neurons[[x]][ipsiidx[[x]]]
  })
  
contralist = lapply(seq(1, length(neurons)), function(x) {
  neurons[[x]][contraidx[[x]]]
  })  

#####for ipsi
if(side == "ipsi") ({
spikelist = list()  
for(i in 1:(length(ipsilist))) ({
  spikelist = c(spikelist, ipsilist[[i]])
  })
  })
  
####for contra
if(side == "contra") ({
spikelist = list()  
for(i in 1:(length(contralist))) ({
  spikelist = c(spikelist, contralist[[i]])
  })  
  })
  
events = eventfinder.rFunc(nexdata, event)

#match events to number of neurons
if(side == "ipsi") ({
allevents = lapply(seq(1, length(events)), function(x) {
  rep(events[x], length(ipsilist[[x]]))
  })
  })
  
if(side == "contra") ({
allevents = lapply(seq(1, length(events)), function(x) {
  rep(events[x], length(contralist[[x]]))
  })
  })  
  
eventslist = list()  
for(i in 1:(length(allevents))) ({
  eventslist = c(eventslist, allevents[[i]])
  })
  
events = eventfinder.rFunc(nexdata, event=4)

#match events to number of neurons
if(side == "ipsi") ({
allevents = lapply(seq(1, length(events)), function(x) {
  rep(events[x], length(ipsilist[[x]]))
  })
  })
  
if(side == "contra") ({
allevents = lapply(seq(1, length(events)), function(x) {
  rep(events[x], length(contralist[[x]]))
  })
  })  
  
baselist = list()  
for(i in 1:(length(allevents))) ({
  baselist = c(baselist, allevents[[i]])
  })    
  
  
  
  
  
  
  
  
  
  
  
  