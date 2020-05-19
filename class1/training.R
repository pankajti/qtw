offline_file_path = "/Users/pankaj/dev/git/smu/qtw/class1/data/offline.final.trace.txt"
online_file_path = "/Users/pankaj/dev/git/smu/qtw/class1/data/online.final.trace.txt"

offline = readData(offline_file_path)

offline$posXY = paste(offline$posX, offline$posY, sep = "-")
 
 offlineSummary = createSummary(offline)
 subMacs = names(sort(table(offline$mac), decreasing = TRUE))[1:7]
  
 offlineSummary_without_cd = subset(offlineSummary, mac != subMacs[2])
 
 offlineSummary_without_c0 = subset(offlineSummary, mac != subMacs[1])
 
 macs = unique(offlineSummary$mac)
 
 online = readData(online_file_path, subMacs = macs)
 online$posXY = paste(online$posX, online$posY, sep = "-")
 onlineSummary = createOnlineSummary(online)
 
  actualXY = onlineSummary[ , c("posX", "posY")]
  
  
  
  estXYk12w = predXY(newSignals = onlineSummary[ , 6:length(onlineSummary)],
                    newAngles = onlineSummary[ , 4],
                    offlineSummary , numAngles = 3, k = 4)
  
  calcError(estXYk12w,actualXY )
  
  macs = unique(offlineSummary_without_cd$mac)
  online = readData(online_file_path, subMacs = macs)
  online$posXY = paste(online$posX, online$posY, sep = "-")
  onlineSummary = createOnlineSummary(online)
  
  actualXY = onlineSummary[ , c("posX", "posY")]
  predict_online_locations(offlineSummary_without_cd, onlineSummary)
  
  caclulate_errors(offlineSummary)
  
  caclulate_errors(offlineSummary_without_c0)
  
  caclulate_errors_weighted(offlineSummary_without_cd)
  
  caclulate_errors_weighted(offlineSummary_without_c0)
  
  caclulate_errors_weighted(offlineSummary)
  
  
  
  

  
  
  
  k_errors_cd = run_kkross_fold(offlineSummaryData = offlineSummary_without_cd )
  k_errors_c0 = run_kkross_fold(offlineSummaryData = offlineSummary_without_c0 )
  
  findGlobals(run_kkross_fold, merge = FALSE)$variables
  
  sapply(list(estXYk1, estXYk3, estXYk5, estXYk12), calcError, actualXY)
  
    
    plot(estXYk1)
    plot(k_errors_cd, type = 'l')
    
    plot(k_errors_c0, type = 'l')
    
    
   
    