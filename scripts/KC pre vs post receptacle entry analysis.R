load("C:/Users/Kevin Caref/Google Drive/RScripts/Functions/entries_uni.rFunc")

entdf = entries_uni.rFunc(path = "C:/Users/Kevin Caref/Google Drive/Opioid Pilot/Neural Data/sorted/unilateral ctap/")
rewents = rew_entries_uni.rFunc(path = "C:/Users/Kevin Caref/Google Drive/Opioid Pilot/Neural Data/sorted/unilateral ctap/")

entries = lapply(entdf, function(x) x$entries)
exits = lapply(entdf, function(x) x$exits) 


pre.rewents = lapply(rewents, function(x) which(x < 2000))
post.rewents = lapply(rewents, function(x) which(x >= 2720 & x <= 4720))

preents = lapply(entdf, function(x) x[which(x$entries < 2000),] )
postents = lapply(entdf, function(x) x[which(x$entries >= 2720 & x$entries <= 4720),] )

rewints = lapply(1:length(rewents), function(x) findInterval(rewents[[x]], entdf[[x]]$entries))

    
    
rewentslist = lapply(seq(1, length(entries)), function(x) {
 lapply(seq(1, length(rewents[[x]])), function(y) {
  stampsidx = which(entries[[x]] >= rewents[[x]][y] & entries[[x]] <= rewents[[x]][y] + 5)
  entrydur = exits[[x]][stampsidx] - entries[[x]][stampsidx]
  #relstamps = entries[[x]][stampsidx] - rewents[[x]][y]
   })
  }) 
  
prerewlist = lapply(1:length(rewentslist), function(x) rewentslist[[x]][pre.rewents[[x]]])
postrewlist = lapply(1:length(rewentslist), function(x) rewentslist[[x]][post.rewents[[x]]])

pretotal = lapply(1:length(prerewlist), function(x) {
  unlist(lapply(1:length(prerewlist[[x]]), function(y) {
    sum(prerewlist[[x]][[y]])
    }))
    })
    
posttotal = lapply(1:length(postrewlist), function(x) {
  unlist(lapply(1:length(postrewlist[[x]]), function(y) {
    sum(postrewlist[[x]][[y]])
    }))
    })    

    
hist(unlist(pretotal), xaxt = "n", yaxt = "n", xlim = c(0,12), ylim = c(0,80), xlab = "", ylab= "", main= "")
hist(unlist(posttotal), add = T, col = "black")
axis(1, at = seq(0, 12, 2), cex.axis = 1.5)
axis(2, at = seq(0,80,20), las = 2, tcl = -.8, cex.axis = 1.5)

mtext("Count", side = 2, line = 3, cex = 1.5)
mtext("Total time spent in receptacle after rewarded entry", side = 1, line = 3, cex = 1.5)


wilcox.test(unlist(pretotal), unlist(posttotal))


hist(unlist(prerewlist), xaxt = "n", yaxt = "n", xlim = c(0,12), ylim = c(0,80), xlab = "", ylab= "", main= "")
hist(unlist(postrewlist), col = "black", add = T)
axis(1, at = seq(0, 12, 2), cex.axis = 1.5)
axis(2, at = seq(0,80,20), las = 2, tcl = -.8, cex.axis = 1.5)

mtext("Count", side = 2, line = 3, cex = 1.5)
mtext("Receptacle entry duration after rewarded entry", side = 1, line = 3, cex = 1.5)

wilcox.test(unlist(prerewlist), unlist(postrewlist))



########################################################
rewentstamps = lapply(seq(1, length(entries)), function(x) {
 lapply(seq(1, length(rewents[[x]])), function(y) {
  entstamps = entries[[x]][which(entries[[x]] >= rewents[[x]][y] & entries[[x]] <= rewents[[x]][y] + 5)]
  #entrydur = exits[[x]][stampsidx] - entries[[x]][stampsidx]
  #relstamps = entries[[x]][stampsidx] - rewents[[x]][y]
   })
  })
  
rewexstamps = lapply(seq(1, length(exits)), function(x) {
 lapply(seq(1, length(rewents[[x]])), function(y) {
  exstamps = exits[[x]][which(exits[[x]] >= rewents[[x]][y] & exits[[x]] <= rewents[[x]][y] + 5)]
  #entrydur = exits[[x]][stampsidx] - entries[[x]][stampsidx]
  #relstamps = entries[[x]][stampsidx] - rewents[[x]][y]
   })
  })
  
exlat = rewentstamps
for(i in 1:length(rewentstamps)) ({
  for(j in 1:length(rewentstamps[[i]])) ({
    exlat[[i]][[j]] = rep(NA, length(rewentstamps[[i]][[j]]))
    if(length(rewentstamps[[i]][[j]]) > 1) (exlat[[i]][[j]] = rewexstamps[[i]][[j]][2] - rewentstamps[[i]][[j]][1])
    })
    }) 
     
hist(unlist(exlat))



































      