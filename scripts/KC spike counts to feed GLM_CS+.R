spikecounts.rFunc = function(spikes, events) { 

#min here is additive; e.g. need to find spike counts between 50 and 200 ms after cue onset
winmin = .05 #in s
winmax = .5 #in s


#####################################################################
#This function returns a list of lists of lists:
#Level 1 is the experiment; Level 2 is each neuron in the experiment;
#Level 3 is the time relative to the specified event for each trial
#The object masterlist can then be used to construct PSTHs or rasters
########################################################################
masterlist = lapply(seq(1, length(spikes)), function(x) {
 lapply(seq(1, length(events[[x]])), function(y) {
  stampsidx = which(spikes[[x]] >= events[[x]][y] + winmin & spikes[[x]] <= events[[x]][y] + winmax)
  relstamps = spikes[[x]][stampsidx] - events[[x]][y]
   })
  }) 
   

                                                                        
#######################################################################
#This function returns a list of lists:
#Level 1 is the experiment
#Level 2 contains the the frequency histogram (PSTH)
#for each neuron for the event specified above.
#
#The object neurohist can then be used to plot histograms individually or
#averaged together as in the next function.
#########################################################################
counts = lapply(seq(1, length(masterlist)), function(x) {
  lapply(seq(1:length(masterlist[[x]])), function(y) {
   length(masterlist[[x]][[y]])
   #allvals = unlist(masterlist[[x]])
   #hcounts = hist(allvals, breaks = seq(-winmin, winmax, binsize/1000), plot = F)$counts 
   #freq = (hcounts/(binsize/1000))/length(masterlist[[x]])
   })
   })

return(counts)
}

save(spikecounts.rFunc, file = "C:/Users/Kevin Caref/Google Drive/RScripts/Functions/spikecounts.rFunc") 


    
    
    
    
