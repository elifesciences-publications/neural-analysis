KC.pois.detect.uni.inhib.Rfunc = function(spikelist, eventslist, startt, endt) {

load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/eventfinder.rFunc")



#########################################################
#Now that the relevant data is extracted, we can begin to 
#analyze it by constructing rasters, histograms, etc.
#########################################################
#binsize = binw #in ms
winmin = 10 #in s
winmax = .5 #in s



#####################################################################
#This function returns a list of lists of lists:
#Level 1 is the experiment; Level 2 is each neuron in the experiment;
#Level 3 is the time relative to the specified event for each trial
#The object masterlist can then be used to construct PSTHs or rasters
########################################################################
masterlist = lapply(seq(1, length(spikelist)), function(x) {
 lapply(seq(1, length(eventslist[[x]])), function(y) {
  stampsidx = which(spikelist[[x]] >= eventslist[[x]][y] - winmin & spikelist[[x]] <= eventslist[[x]][y] + winmax)
  relstamps = spikelist[[x]][stampsidx] - eventslist[[x]][y]
   })
  }) 


cueex = lapply(seq(1, length(masterlist)), function(x) {
        
    
    intmin = -10
    intmax = .5
    pbin = .05
    threshold = 1
    
    allvals = unlist(masterlist[[x]])
    hcounts = hist(allvals[which(allvals >= intmin & allvals <= intmax)], breaks = seq(intmin, intmax, pbin), plot = F)$counts
    
    baseline = hcounts[1:abs(intmin/pbin)]
    

#Applying Vince's code to my data
    premean = mean( (baseline / length(masterlist[[x]])) )   # find the mean fr per bin within the pre period    
    probvect = ppois(hcounts, premean*length(masterlist[[x]])) # the lambda argument is the mean fr per bin times the number of trials

    critwin = probvect[(abs(intmin/pbin)+1):(abs(intmin/pbin)+intmax/pbin)]



    #diffs = diff(which(critwin >= .999))       #computes the differences in indices for bins exceeding the critical value
    cueex = F
    #if(length(which(rle(diffs)$values == 1 & rle(diffs)$lengths >= threshold))>0) (cueex = T)   #looks for consecutive bins (diff equal to 1) that are at least 3 bins long (rle length of at least 3)
    if(length(which(critwin <= .01)) > 0) (cueex = T)
    
return(cueex)

 })
 
return(cueex)

}

save(KC.pois.detect.uni.inhib.Rfunc, file = "C:/Users/Kevin Caref/Google Drive/RScripts/Functions/KC.pois.detect.uni.inhib.Rfunc")


 