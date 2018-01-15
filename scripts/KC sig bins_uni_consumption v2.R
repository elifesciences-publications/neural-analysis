KC.sigbins_uni_cons.Rfunc = function(path, startt, endt, side) {

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
  
events = eventfinder.rFunc(nexdata, event=6)



 


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
  
events = eventfinder.rFunc(nexdata, event=4)

allevents = lapply(seq(1, length(events)), function(x) {
  rep(events[x], length(neurons[[x]]))
  })
  
baselist = list()  
for(i in 1:(length(allevents))) ({
  baselist = c(baselist, allevents[[i]])
  })
  
baselist = lapply(1:length(baselist), function(x) {
  baselist[[x]][which(baselist[[x]]>= sstart & baselist[[x]] <= send)] })           
      

#########################################################
#Now that the relevant data is extracted, we can begin to 
#analyze it by constructing rasters, histograms, etc.
#########################################################
#binsize = binw #in ms
winmin = 10 #in s
winmax = 10 #in s

#####################################################################
#This function returns a list of lists:
#Level 1 is each neuron;
#Level 2 is the time relative to the specified event for each trial
#The object masterlist can then be used to construct PSTHs or rasters
########################################################################
masterlist = lapply(seq(1, length(spikelist)), function(x) {
 lapply(seq(1, length(eventslist[[x]])), function(y) {
  evokedspikesidx = which(spikelist[[x]] >= eventslist[[x]][y] & spikelist[[x]] <= eventslist[[x]][y] + winmax)
  basespikesidx = which(spikelist[[x]] >= baselist[[x]][y] - winmin & spikelist[[x]] <= baselist[[x]][y]) 
  evstamps = spikelist[[x]][evokedspikesidx] - eventslist[[x]][y]
  basestamps = spikelist[[x]][basespikesidx] - baselist[[x]][y]
  relstamps = c(basestamps, evstamps)
  return(relstamps)
   })
  }) 
   
     





sigbins = lapply(seq(1, length(masterlist)), function(x) {    
    
    intmin = -8
    intmax = 4
    pbin = .4
    
    allvals = unlist(masterlist[[x]])
    hcounts = hist(allvals[which(allvals >= intmin & allvals <= intmax)], breaks = seq(intmin, intmax, pbin), plot = F)$counts
    
    baseline = hcounts[1:abs(intmin/pbin)]
    

#Applying Vince's code to my data
    premean = mean( (baseline / length(masterlist[[x]])) )   # find the mean fr per bin within the pre period    
    probvect = ppois(hcounts, premean*length(masterlist[[x]])) # the lambda argument is the mean fr per bin times the number of trials

    critwin = probvect[(abs(intmin/pbin)+1):(abs(intmin/pbin)+intmax/pbin)]


    sigbins = rep(F, length(critwin))
    sigbins[which(critwin >= .99)] = T
    

    
return(sigbins)

 })
 
sigbindf = t(do.call("rbind", sigbins))
 
return(sigbindf)

}

save(KC.sigbins_uni_cons.Rfunc, file = "C:/Users/Kevin Caref/Google Drive/RScripts/Functions/KC.sigbins_uni_cons.Rfunc")


 