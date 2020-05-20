library(tidyverse)
library(magrittr)
library(fields)
library(codetools)
library(lattice)


offline_file_path = "data/offline.final.trace.txt"
online_file_path = "data/online.final.trace.txt"

# Rounds an angle to the nearest 45 degree angle (setting 360 to 0)
roundOrientation = function(angles) {
  refs = seq(0, by = 45, length = 9)
  q = sapply(angles, function(o) which.min(abs(o - refs)))
  c(refs[1:8], 0)[q]
}

# Splits a line in the data file at these symbols ; = ,
# Turns result into a matrix with handheld device information and
# MAC address and signal strength information
processLine = function(x)
{
  tokens = strsplit(x, "[;=,]")[[1]]
  if (length(tokens) == 10)
    return(NULL)
  tmp = matrix(tokens[ - (1:10) ], ncol = 4, byrow = TRUE)
  cbind(matrix(tokens[c(2, 4, 6:8, 10)], nrow(tmp), 6,
               byrow = TRUE), tmp)
}


# Reads data from a file, parses each line, compresses into a data.frame
# with appropriate column names and classes. Discards measurements of
# type 1 (adhoc) and deletes type column. Converts time from ms to s.
# Deletes scanMac, posZ, and channel columns. Rounds angle to nearest 45.
# Deletes MAC addresses not in the top 7 most frequent, or not in subMacs
# if specified.
readData = function(file_path , subMacs=NULL){
  txt = readLines(file_path)
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
  return(records)
} 

# Creates contour plot of signal strength
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
  refs = seq(0, by = 45, length = 8)
  byLocation =
    with(data, by(data, list(posXY),
                  function(x) {
                    if (sampleAngle) x = x[x$angle == sample(refs, size = 1), ]
                    
                    ans = x[1, keepVars]
                    avgSS = tapply(x[ , varSignal ], x$mac, mean)
                    avss= avgSS
                    y = matrix(avgSS, nrow = 1, ncol = length(unique(data$mac)),
                               dimnames = list(ans$posXY,
                                               names(avgSS)))
                    cbind(ans, y)
                  }))
  newDataSS = do.call("rbind", byLocation)
  return(newDataSS)
}

# Creates summary statistics for each location/angle/MAC combination
createSummary = function(data) {
  data$posXY = paste(data$posX, data$posY, sep = "-")
  # Separates data frame into several data frames, each with the same
  # (posXY, angle, mac) combination
  byLocAngleAP = with(data,
                      by(data, list(posXY, angle, mac),
                         function(x) x))
  # Then we can calculate summary statistics on each of these data frames with
  signalSummary =
    lapply(byLocAngleAP,
           function(oneLoc) {
             ans = oneLoc[1, ]
             ans$medSignal = median(oneLoc$signal)
             ans$avgSignal = mean(oneLoc$signal)
             ans$num = length(oneLoc$signal)
             ans$sdSignal = sd(oneLoc$signal)
             ans$iqrSignal = IQR(oneLoc$signal)
             ans
           })
  # Combine data frames
  dataSummary = do.call("rbind", signalSummary)
  return(dataSummary)
}

# Subsets training data for m closest orientations
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
  return(trainSS)
}

# Sorts training subset by Euclidean distance to input signal newSignal
findNN = function(newSignal, trainSubset) {
  diffs = apply(trainSubset[ , 4:9], 1,
                function(x) x - newSignal)
  dists = apply(diffs, 2, function(x) sqrt(sum(x^2)) )
  closest = order(dists)
  return(trainSubset[closest, 1:3 ])
}

# Sorts training subset by Euclidean distance to input signal newSignal,
# then assigns weights by (1/d_i)/(sum[i=1:k](1/d_i))
findNNWeight = function(newSignal, trainSubset, k ) {
  diffs = apply(trainSubset[ , 4:9], 1,
                function(x) x - newSignal)
  dists = apply(diffs, 2, function(x) sqrt(sum(x^2)) )
  closest = order(dists)
  sorted_dist = sort(dists)
  inv_sorted_dist = 1/sorted_dist
  weights = inv_sorted_dist/(sum(inv_sorted_dist[1:k]))
  closest_weight = trainSubset[closest, 1:3 ]
  closest_weight$weights = weights
  return(closest_weight)
}

# For each new row of signals and angles, finds nearest k neighbors in
# training set using numAngles closest angles and predicts position
# by averaging positions of the neighbors
predXY = function(newSignals, newAngles, trainData,
                  numAngles = 1, k = 3){
  closeXY = vector("list", length = nrow(newSignals))
  for (i in 1:nrow(newSignals)) {
    trainSS = selectTrain(newAngles[i], trainData, m = numAngles)
    closeXY[[i]] =
      findNN(newSignal = as.numeric(newSignals[i, ]), trainSS)
  }
  avgPosition = function(x){
    sapply(x[,2:3], function(x) mean(x[1:k]))
  }
  estXY = lapply(closeXY, avgPosition)
  estXY = do.call("rbind", estXY)
  return(estXY)
}

# Same as predXY but uses weighted average
predXYWeighted = function(newSignals, newAngles, trainData,
                  numAngles = 1, k = 3){
  closeXY = vector("list", length = nrow(newSignals))
  for (i in 1:nrow(newSignals)) {
    trainSS = selectTrain(newAngles[i], trainData, m = numAngles)
    closeXY[[i]] =
      findNNWeight(newSignal = as.numeric(newSignals[i, ]), trainSS, k)
  }
  avgWeightedPosition = function(x){
    weights = x[1:k,4]
    sapply(x[,2:3], function(y) sum(y[1:k]*weights)/sum(weights))
  }
  estXY = lapply(closeXY, avgWeightedPosition)
  estXY = do.call("rbind", estXY)
  return(estXY)
}

# Calculates mean squared error between predicted and actual positions
calcError =
  function(estXY, actualXY)
    sum( rowSums( (estXY - actualXY)^2) )


run_kkross_fold = function(offlineSummaryData  , v = 11,
                             K = 20)
{
  permuteLocs = sample(unique(offlineSummaryData$posXY))
  permuteLocs = matrix(permuteLocs, ncol = v,
                       nrow = floor(length(permuteLocs)/v))
  
  keepVars = c("posXY", "posX","posY", "orientation", "angle")
  onlineCVSummary = reshapeSS(offlineSummaryData, keepVars = keepVars,
                              sampleAngle = TRUE)
  
  
  err = rep(0, K)
  for (j in 1:v) {
    onlineFold = subset(onlineCVSummary,
                        posXY %in% permuteLocs[ , j]) 
    offlineFold = subset(offlineSummaryData,
                         posXY %in% permuteLocs[ , -j])
    actualFold = onlineFold[ , c("posX", "posY")]
    for (k in 1:K) {
      #print(paste("running for  j ", j ," k ",  k))
      estFold = predXY(newSignals = onlineFold[ , 6:length(onlineFold)],
                       newAngles = onlineFold[ , 4],
                       offlineFold, numAngles = 3, k = k)
      err[k] = err[k] + calcError(estFold, actualFold)
    }
  }
  err
}

createOnlineSummary =  function(data ){
  keepVars = c("posXY", "posX","posY", "orientation", "angle")
  byLoc = with(data,
               by(data, list(posXY),
                  function(x) {
                    ans = x[1, keepVars]
                    avgSS = tapply(x$signal, x$mac, mean)
                    y = matrix(avgSS, nrow = 1, ncol =length(unique(data$mac)),
                               dimnames = list(ans$posXY, names(avgSS)))
                    cbind(ans, y)
                  }))
  onlineSummary = do.call("rbind", byLoc)
  onlineSummary
}
 


predict_online_locations = function(offlineData,onlineSummary, k=4 ){
   
  
  
  estXYk = predXY(newSignals = onlineSummary[ , 6:length(onlineSummary)],
                   newAngles = onlineSummary[ , 4],
                   offlineData, numAngles = 3, k = k)
  return(estXYk)
}


predict_online_locations_weighted = function(offlineData, onlineSummary, k=4 ){
  
  
  
  estXYk = predXYWeighted(newSignals = onlineSummary[ , 6:length(onlineSummary)],
                           newAngles = onlineSummary[ , 4],
                           offlineData, numAngles = 3, k = k)
  
  return(estXYk)
}
