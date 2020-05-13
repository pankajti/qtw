library(tidyverse)
library(magrittr)
library(fields)
library(codetools)
library(lattice)


offline_file_path = "/Users/pankaj/dev/git/smu/qtw/data/offline.final.trace.txt"
online_file_path = "/Users/pankaj/dev/git/smu/qtw/data/online.final.trace.txt"


roundOrientation = function(angles) {
  refs = seq(0, by = 45, length = 9)
  q = sapply(angles, function(o) which.min(abs(o - refs)))
  c(refs[1:8], 0)[q]
}

processLine = function(x)
{
  tokens = strsplit(x, "[;=,]")[[1]]
  if (length(tokens) == 10)
    return(NULL)
  tmp = matrix(tokens[ - (1:10) ], , 4, byrow = TRUE)
  cbind(matrix(tokens[c(2, 4, 6:8, 10)], nrow(tmp), 6,
               byrow = TRUE), tmp)
}



readData = function(file_path , subMacs=NULL){
  txt = readLines(file_path)
  unlist(lapply(strsplit(txt[4], ";")[[1]],
                function(x)
                  sapply(strsplit(x, "=")[[1]], strsplit, ",")))
  tokens = strsplit(txt[4], "[;=,]")[[1]]
  tmp = matrix(tokens[ - (1:10) ], ncol = 4, byrow = TRUE)
  lines = txt[ substr(txt, 1, 1) != "#" ]
  tmp = lapply(lines, processLine)
  records = as.data.frame(do.call("rbind", tmp),stringsAsFactors = FALSE)
  names(records) = c("time", "scanMac", "posX", "posY", "posZ",
                     "orientation", "mac", "signal",
                     "channel", "type")
  numVars = c("time", "posX", "posY", "posZ",
              "orientation", "signal")
  records[ numVars ] = lapply(records[ numVars ], as.numeric)
  records = records[ records$type == "3", ]
  records = records[ , "type" != names(records) ]
  records$rawTime = records$time
  records$time = records$time/1000
  class(records$time) = c("POSIXt", "POSIXct")
  records = records[ , !(names(records) %in% c("scanMac", "posZ"))]
  records$angle = roundOrientation(records$orientation)
  if (is.null(subMacs)){
    subMacs = names(sort(table(records$mac), decreasing = TRUE))[1:7]
  }
  records = records[ records$mac %in% subMacs, ]
  records = records[ , "channel" != names(records)]
  records
} 

surfaceSS = function(data, mac, angle){
  
  oneAPAngle = subset(data,
                      mac == mac & angle == angle)
  
  smoothSS = Tps(oneAPAngle[, c("posX","posY")],
                 oneAPAngle$avgSignal)
  
  
  vizSmooth = predictSurface(smoothSS)
  
  plot.surface(vizSmooth, type = "C")
  
  
  points(oneAPAngle$posX, oneAPAngle$posY, pch=19, cex = 0.5)
  
}

reshapeSS = function(data, varSignal = "signal",
                     keepVars = c("posXY", "posX","posY"), sampleAngle= FALSE) {
  byLocation =
    with(data, by(data, list(posXY),
                  function(x) {
                    if (sampleAngle) x = x[x$angle == sample(refs, size = 1), ]
                    
                    ans = x[1, keepVars]
                    avgSS = tapply(x[ , varSignal ], x$mac, mean)
                    y = matrix(avgSS, nrow = 1, ncol = 6,
                               dimnames = list(ans$posXY,
                                               names(avgSS)))
                    cbind(ans, y)
                  }))
  newDataSS = do.call("rbind", byLocation)
  
  return(newDataSS)
}


selectTrain = function(angleNewObs, offlineSummary, m){
  
  ## todo check below
  nearestAngle = roundOrientation(angleNewObs)
  
  if (m %% 2 == 1) {
    angles = seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
  } else {
    m = m + 1
    angles = seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
    if (sign(angleNewObs - nearestAngle) > -1)
      angles = angles[ -1 ]
    else
      angles = angles[ -m ]
  }
  
  
  
  angles = angles + nearestAngle
  angles[angles < 0] = angles[ angles < 0 ] + 360
  angles[angles > 360] = angles[ angles > 360 ] - 360
  
  
  offlineSubset =
    offlineSummary[ offlineSummary$angle %in% angles, ]
  
  trainSS = reshapeSS(offlineSubset, varSignal = "avgSignal")
  trainSS
  
}

findNN = function(newSignal, trainSubset) {
  diffs = apply(trainSubset[ , 4:9], 1,
                function(x) x - newSignal)
  dists = apply(diffs, 2, function(x) sqrt(sum(x^2)) )
  closest = order(dists)
  return(trainSubset[closest, 1:3 ])
}

predXY = function(newSignals, newAngles, trainData,
                  numAngles = 1, k = 3){
  closeXY = list(length = nrow(newSignals))
  for (i in 1:nrow(newSignals)) {
    trainSS = selectTrain(newAngles[i], trainData, m = numAngles)
    closeXY[[i]] =
      findNN(newSignal = as.numeric(newSignals[i, ]), trainSS)
  }
  estXY = lapply(closeXY, function(x) sapply(x[ , 2:3],
                                             function(x) mean(x[1:k])))
  estXY = do.call("rbind", estXY)
  return(estXY)
}

 

 