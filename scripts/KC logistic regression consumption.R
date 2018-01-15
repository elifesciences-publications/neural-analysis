load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/cons_model.rFunc")

path = "C:/Users/Kevin Caref/Google Drive/Opioid Pilot/Neural Data/sorted/sated response analysis/"

#allspikes = getspikes.rFunc(path) 
trialsdf = cons_model.rFunc(path)




for(i in 1:length(trialsdf)) ({
  trialsdf[[i]]$neuronid = rep(i, nrow(trialsdf[[i]]))
  })

consglm = lapply(1:length(trialsdf), function(x) {
  glm(nextresp~spikes, family = binomial(link='logit'), data = trialsdf[[x]])
  })

summcoeffs = lapply(consglm, function(x) summary(x))
pvals = as.numeric(unlist(lapply(summcoeffs, function(x) x$coefficients[2,][4])  ))
  
coeffs = as.numeric(unlist(lapply(summcoeffs, function(x) x$coefficients[2,][1])  ))


binex = KC.sigbins_cons.Rfunc(path, startt = 0, endt = 7200, event = 6)
binin = KC.sigbins_inhib_cons.Rfunc(path, startt = 0, endt = 7200, event = 6)

exneurons = which(sapply(1:ncol(binex), function(x) length(which(binex[,x] == T ))) >0)
inneurons = which(sapply(1:ncol(binin), function(x) length(which(binin[,x] == T ))) >0)

wilcox.test(coeffs[exneurons])

###########################################
#run another glm using all trials and neuron id and z-scores/spikes as dvs
############################################

extrials = do.call("rbind", trialsdf[exneurons])
intrials = do.call("rbind", trialsdf[inneurons])

extotglm = glm(nextresp~spikes+as.factor(neuronid), family = binomial, data = extrials)


extotglm2 = glm(nextresp~zscore+as.factor(neuronid), family = binomial, data = extrials)


intotglm = glm(nextresp~spikes+as.factor(neuronid), family = binomial, data = intrials)


intotglm2 = glm(nextresp~zscore+as.factor(neuronid), family = binomial, data = intrials)