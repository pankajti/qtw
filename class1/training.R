offline_file_path = "/Users/pankaj/dev/git/smu/qtw/class1/data/offline.final.trace.txt"
online_file_path = "/Users/pankaj/dev/git/smu/qtw/class1/data/online.final.trace.txt"

offline = readData(offline_file_path)

offline$posXY = paste(offline$posX, offline$posY, sep = "-")
 
 offlineSummary = createSummary(offline)
 subMacs = names(sort(table(offline$mac), decreasing = TRUE))[1:7]
  
 offlineSummary_without_cd = subset(offlineSummary, mac != subMacs[2])
 
 offlineSummary_without_c0 = subset(offlineSummary, mac != subMacs[1])
 
  actualXY = onlineSummary[ , c("posX", "posY")]
  
  caclulate_errors = function(offlineData ){
    macs = unique(offlineData$mac)
    print(macs)
    online = readData(online_file_path, subMacs = macs)
    online$posXY = paste(online$posX, online$posY, sep = "-")
    onlineSummary= createOnlineSummary(online)
    
  estXYk3 = predXY(newSignals = onlineSummary[ , 6:11],
                   newAngles = onlineSummary[ , 4],
                   offlineData, numAngles = 3, k = 3)
  
  estXYk1 = predXY(newSignals = onlineSummary[ , 6:11],
                   newAngles = onlineSummary[ , 4],
                   offlineData, numAngles = 3, k = 1)
  

  
  estXYk5 = predXY(newSignals = onlineSummary[ , 6:11],
                   newAngles = onlineSummary[ , 4],
                   offlineData, numAngles = 3, k = 5)
  
  estXYk12 = predXY(newSignals = onlineSummary[ , 6:11],
                    newAngles = onlineSummary[ , 4],
                    offlineData, numAngles = 3, k = 12)
  
  sapply(list(estXYk1, estXYk3, estXYk5, estXYk12), calcError, actualXY)
  }
  
  
  caclulate_errors_weighted = function(offlineData ){
    macs = unique(offlineData$mac)
    print(macs)
    online = readData(online_file_path, subMacs = macs)
    online$posXY = paste(online$posX, online$posY, sep = "-")
    onlineSummary= createOnlineSummary(online)
    
    estXYk3 = predXYWeighted(newSignals = onlineSummary[ , 6:11],
                     newAngles = onlineSummary[ , 4],
                     offlineData, numAngles = 3, k = 3)
    
    estXYk1 = predXYWeighted(newSignals = onlineSummary[ , 6:11],
                     newAngles = onlineSummary[ , 4],
                     offlineData, numAngles = 3, k = 1)
    
    
    
    estXYk5 = predXYWeighted(newSignals = onlineSummary[ , 6:11],
                     newAngles = onlineSummary[ , 4],
                     offlineData, numAngles = 3, k = 5)
    
    estXYk12 = predXYWeighted(newSignals = onlineSummary[ , 6:11],
                      newAngles = onlineSummary[ , 4],
                      offlineData, numAngles = 3, k = 12)
    
    sapply(list(estXYk1, estXYk3, estXYk5, estXYk12), calcError, actualXY)
  }
  
  
  estXYk12w = predXY(newSignals = onlineSummary[ , 6:11],
                    newAngles = onlineSummary[ , 4],
                    offlineSummary_without_c0, numAngles = 3, k = 4)
  
  calcError(estXYk12w,actualXY )
  
  caclulate_errors(offlineSummary_without_cd)
  
  caclulate_errors(offlineSummary)
  
  caclulate_errors(offlineSummary_without_c0)
  
  caclulate_errors_weighted(offlineSummary_without_cd)
  
  caclulate_errors_weighted(offlineSummary_without_c0)
  
  caclulate_errors_weighted(offlineSummary)
  
  
  
  
  
  k_errors_cd = run_kkross_fold(offlineSummaryData = offlineSummary_without_cd )
  k_errors_c0 = run_kkross_fold(offlineSummaryData = offlineSummary_without_c0 )
  
  findGlobals(run_kkross_fold, merge = FALSE)$variables
  
    
    plot(estXYk1)
    plot(k_errors_cd, type = 'l')
    
    plot(k_errors_c0, type = 'l')
    
    
   
    