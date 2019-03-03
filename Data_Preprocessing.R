source("alg_functions.R")
library("pracma")
library("e1071")
library("rwt")




high_pass_filter <- function(rawData)
{
  constant <- 0.99
  multiplicator <- (1 + constant) / 2
  
  for(i in 1:nrow(rawData)) {
    row <- rawData$Value[i]

    # do stuff with row
    rawData$Value[i] <- multiplicator*(1 - (1/row))/(1 - ((1/row)*constant))
  }
  return(rawData)
}

prepareDateForDWT <- function(rawData, n) {
  
  x<- rawData$Value[1:as.numeric(n)]

  return(x)
}

low_pass_filter_by_DWT <- function(rawData)
{
  h <- c(0.11154074335008017,
         0.4946238903983854,
         0.7511339080215775,
         0.3152503517092432,
         -0.22626469396516913,
         -0.12976686756709563,
         0.09750160558707936,
         0.02752286553001629,
         -0.031582039318031156,
         0.0005538422009938016,
         0.004777257511010651,
         -0.00107730108499558)
  


  
  denoised <<- denoise.dwt(rawData, h, option = default.dwt.option)
  return(denoised$xd)
}


preprocess_data <- function(rawData)
{
  
  rawData <- data.frame(rawData)
  colnames(rawData) <- c("Value")
  rawData$sign <- ifelse(rawData$Value < 0, 1, 0)
  rawData$Value <- ifelse(rawData$sign == 1, ((rawData$Value*rawData$Value)-(((rawData$Value*rawData$Value)*2))),
                        (rawData$Value*rawData$Value))
  
  return(rawData$Value)
}

peak_detection_time_calculation <- function(rawData, frequency_of_data)
{
  # 0.987875
  peaks <- as.data.frame(findpeaks(as.numeric(rawData), peakpat = "[+]{20,}[0]{0,}[-]{10,}", npeaks = 1000000, minpeakheight = (mean(rawData)*0.9), minpeakdistance = frequency_of_data/2, sortstr = FALSE))
  colnames(peaks) <- c("Value", "Index", "From", "To")
  # Counting time of peak
  peaks$Index <- sort(peaks$Index)
  vrcholy <<- peaks$Index
  peaks$Time <- peaks$Index * (1000/frequency_of_data)
  return(as.data.frame(peaks))
  
}
# RR intervals counting ----

intervals_calculation <- function(peaks)
{
  peaks$Interval <- 0
  for(rowN in 1:as.numeric(nrow(peaks))) {
    if(rowN > 1)
      peaks[rowN,6] <- peaks[rowN,5] - peaks[(rowN-1),5]
    else
      peaks[1,6] <- peaks[1,5]
  }
  return(peaks)
}
# NN intervals detection ----
# NN_intervals_correction <- function(peaks)
# {
#   peaks$Interval_raw <- peaks$Interval
#   
#   for(rowN in 1:(as.numeric(length(peaks$Interval)-1))){
#     if(rowN > 2) {
#       if(peaks$Interval[rowN] > 0.7*(peaks$Interval[rowN-1]+peaks$Interval[rowN+1])){
#         peaks$Interval[rowN]=(peaks$Interval[rowN-1]+peaks$Interval[rowN+1])/2
#       }
#       
#     }
#   }
#   
#   return(peaks)
# }

NN_intervals_correction <- function(peaks)
{
  peaks$Interval_raw <- peaks$Interval
  mean_interval <- as.numeric(median(peaks$Interval))
  for(rowN in 1:(as.numeric(length(peaks$Interval)-1))){
    if(rowN > 2) {
      if(peaks$Interval[rowN] > 1.3*mean_interval){
        peaks$Interval[rowN] = mean_interval
      }
      
    }
  }
  
  return(peaks)
}

NN50_forXsec <- function(peaks, X){
  sumOfIntervals <- 0
  countOfIntervalsIn30sec <- c()
  
  
  rowN30sec <- 1
  for(rowN in 1:as.numeric(length(peaks$Interval))){
    if(rowN > 1) {
      
      if(X < (sumOfIntervals + peaks$Interval[rowN])){
        countOfIntervalsIn30sec$count[rowN30sec] <- (rowN-1)
        countOfIntervalsIn30sec$time[rowN30sec] <- sumOfIntervals
        #append(countOfIntervalsIn30sec$count, (rowN-1))
        #append(countOfIntervalsIn30sec$time, sumOfIntervals)
        
        sumOfIntervals <- peaks$Interval[rowN]
        rowN30sec <- rowN30sec + 1
      }else{
        if(rowN == length(peaks$Interval)){
          countOfIntervalsIn30sec$count[rowN30sec] <- rowN
          countOfIntervalsIn30sec$time[rowN30sec] <- sumOfIntervals + peaks$Interval[rowN]
          
          #append(countOfIntervalsIn30sec$count, rowN)
          #append(countOfIntervalsIn30sec$time, sumOfIntervals + peaks$Interval[rowN])
        }else{
          sumOfIntervals <- sumOfIntervals + peaks$Interval[rowN]
        }
      }
    }
  }
  return(countOfIntervalsIn30sec)
}

total_time_calculation <- function(countOfIntervalsIn30sec){
  total <- 0
  countOfIntervalsIn30sec$total_time <- 0
  for(rowN in 1:as.numeric(length(countOfIntervalsIn30sec$time))){
    total = countOfIntervalsIn30sec$time[rowN] + total
    countOfIntervalsIn30sec$total_time[rowN] <- total
  }
  
  return(countOfIntervalsIn30sec)
}

NN50_intervals_calculation <- function(countOfIntervalsIn30sec, peaks){
  NN_intervals <- c()
  nn <- 0
  rowNOfIntervals <- 1
  countOfIntervalsIn30sec$NN50 <- 0

  for(rowN in 1:as.numeric(length(peaks$Interval))){
    
    if(rowN > 1) {

      if(countOfIntervalsIn30sec$count[rowNOfIntervals] == rowN){
        sumOfNN50succesive <- 0
        
        for(rowInNN50 in 1:as.numeric(length(NN_intervals))){
          sumOfNN50succesive <- sumOfNN50succesive + NN_intervals[rowInNN50]
        }
        if(length(NN_intervals) != 0){
          countOfIntervalsIn30sec$NN50[rowNOfIntervals] <- sumOfNN50succesive/length(NN_intervals)
         
        }else{
          countOfIntervalsIn30sec$NN50[rowNOfIntervals] <- 0
        }
        
        NN_intervals <- c()
        rowNOfIntervals <- rowNOfIntervals +1
      }else{
        if(50 < (peaks$Interval[rowN-1]-peaks$Interval[rowN]) | 50 < (peaks$Interval[rowN]-peaks$Interval[rowN-1])){
          nn <- nn +1
        }else{
          if(nn != 0){
            NN_intervals <- append(NN_intervals, nn)
          }
          nn <- 0
        }
      }
    }
  }
  return(countOfIntervalsIn30sec)
}



pNN50_intervals_calculation <- function(countOfIntervalsIn30sec){
  
  countOfIntervalsIn30sec$pNN50 <- 0
  
  for(rowN in 1:as.numeric(length(countOfIntervalsIn30sec$count))){
    if(rowN == 1){
      countOfIntervalsIn30sec$pNN50[rowN] <- ((countOfIntervalsIn30sec$NN50[rowN]/(countOfIntervalsIn30sec$count[rowN]-1))*100)
    }else{
      
      countOfIntervalsIn30sec$pNN50[rowN] <- ((countOfIntervalsIn30sec$NN50[rowN]/(countOfIntervalsIn30sec$count[rowN]
                                                                     -countOfIntervalsIn30sec$count[rowN-1]))*100)
    }
  }
  return(countOfIntervalsIn30sec)
}

SDNN_intervals_calculation <- function(countOfIntervalsIn30sec, peaks){
  NN_intervals <- c()
  nn <- 0
  rowNOfIntervals <- 1
  countOfIntervalsIn30sec$SDNN <- 0
  for(rowN in 1:as.numeric(length(peaks$Interval))){
    
    if(rowN > 1) {
      
      if(countOfIntervalsIn30sec$count[rowNOfIntervals] == rowN){
        
        if(length(NN_intervals) != 0){
          
          countOfIntervalsIn30sec$SDNN[rowNOfIntervals] <- sd(NN_intervals)
        }
        
        NN_intervals <- c()
        rowNOfIntervals <- rowNOfIntervals +1
      }else{
        
        NN_intervals <- append(NN_intervals, peaks$Interval[rowN])
        
      }
    }
  }
  return(countOfIntervalsIn30sec)
}

RMSSD_intervals_calculation <- function(countOfIntervalsIn30sec, peaks){
  NN_intervalsDifferences <- c()
  nn <- 0
  rowNOfIntervals <- 1
  countOfIntervalsIn30sec$RMSSD <- 0
  for(rowN in 1:as.numeric(length(peaks$Interval))){
    
    if(rowN > 1) {
      
      if(countOfIntervalsIn30sec$count[rowNOfIntervals] == rowN){
        sumOfSquaredDifferences <- 0
        for(rowNinNNIntervals in 1:as.numeric(length(NN_intervalsDifferences))){
          
          
          sumOfSquaredDifferences <- sumOfSquaredDifferences + NN_intervalsDifferences[rowNinNNIntervals] 
        }
        

       
        countOfIntervalsIn30sec$RMSSD[rowNOfIntervals] <- sqrt(sumOfSquaredDifferences /length(NN_intervalsDifferences))
        
        NN_intervalsDifferences <- c()
        rowNOfIntervals <- rowNOfIntervals +1
      }else{
        result <- as.numeric(abs((peaks$Interval[rowN] - peaks$Interval[rowN-1])))
        
        NN_intervalsDifferences <- append(NN_intervalsDifferences, ifelse(result > 0, result^2, 0))
        
      }
    }
  }
  
  return(countOfIntervalsIn30sec)
}

SDSD_intervals_calculation <- function(countOfIntervalsIn30sec, peaks){
  NN_intervals <- c()
  nn <- 0
  rowNOfIntervals <- 1
  countOfIntervalsIn30sec$SDSD <- 0
  
  for(rowN in 1:as.numeric(length(peaks$Interval))){
    
    if(rowN > 1) {
      
      if(countOfIntervalsIn30sec$count[rowNOfIntervals] == rowN){

        if(length(NN_intervals) != 0){
          countOfIntervalsIn30sec$SDSD[rowNOfIntervals] <- sd(NN_intervals)
          
        }else{
          countOfIntervalsIn30sec$SDSD[rowNOfIntervals] <- 0
        }
        
        NN_intervals <- c()
        rowNOfIntervals <- rowNOfIntervals +1
      }else{
         
     
            NN_intervals <- append(NN_intervals, abs(peaks$Interval[rowN-1]-peaks$Interval[rowN]))
     
      }
    }
  }
  return(countOfIntervalsIn30sec)
}

feature_normalization <- function(list_L){
  
  list_L <- (list_L - min(list_L)) / (max(list_L)-min(list_L))
  
  return(list_L)
  #return(svm_prediction)
}

preprocessPPG <- function(ppg_data){
  
    ppg_data <- data.frame(ppg_data)
    ppg_data <- high_pass_filter(ppg_data)
    ppg_dataDWT_rest <- ppg_data$Value
    ppg_dataDWT <- c()
    
    dataSize <- length(ppg_dataDWT_rest)
   while(dataSize > 1500){
     
    biggestPowerOf2 <- findClosestLowerPowerOf2(dataSize)
    
    #dataset split
    ppg_dataDWT_part <- ppg_dataDWT_rest[1:as.numeric(biggestPowerOf2)]
    ppg_dataDWT_rest <- ppg_dataDWT_rest[(as.numeric(biggestPowerOf2)+1):dataSize]
    
    ppg_dataDWT <- append(ppg_dataDWT, low_pass_filter_by_DWT(ppg_dataDWT_part))
  
    dataSize <- length(ppg_dataDWT_rest)
   }

  
  ppg_dataDWT <- as.data.frame(ppg_dataDWT)
  colnames(ppg_dataDWT) <- c("Value") 
  ppg_dataDWT <- preprocess_data(ppg_dataDWT)
  mojedata <<- ppg_dataDWT
  return(ppg_dataDWT)
}

extractPPGfeatures <- function(ppg_data, frequencyIN, timeFramesSize){
  peaks <- peak_detection_time_calculation(ppg_data, frequencyIN)
  #cat(peaks$Value ,"\n")
  cat(peaks$Index ,"\n")
  peaks <- intervals_calculation(peaks)
  cat(peaks$Interval,"\n")
  peaks <- NN_intervals_correction(peaks)
  cat(peaks$Interval,"\n")
  intervalsFeatures <- NN50_forXsec(peaks, timeFramesSize)
  #cat(intervalsFeatures$count ,"\n")
  #cat(intervalsFeatures$time ,"\n")
  intervalsFeatures <- total_time_calculation(intervalsFeatures)
  intervalsFeatures <- NN50_intervals_calculation(intervalsFeatures, peaks)
  intervalsFeatures <- SDSD_intervals_calculation(intervalsFeatures, peaks)
  intervalsFeatures <- pNN50_intervals_calculation(intervalsFeatures)
  intervalsFeatures <- SDNN_intervals_calculation(intervalsFeatures,peaks)
  intervalsFeatures <- as.data.frame(RMSSD_intervals_calculation(intervalsFeatures,peaks))


  return(intervalsFeatures)
  
}

extractECG <- function(ppg_data, frequencyIN, timeFramesSize){
  ppg_data <- data.frame(ppg_data)
  
  ppg_data <- preprocess_data(ppg_data)
  peaks <- peak_detection_time_calculation(ppg_data, frequencyIN)
  peaks <- intervals_calculation(peaks)
  peaks <- NN_intervals_correction(peaks)
  intervalsFeatures <- NN50_forXsec(peaks, timeFramesSize)
  
  intervalsFeatures <- total_time_calculation(intervalsFeatures)
  intervalsFeatures <- NN50_intervals_calculation(intervalsFeatures, peaks)
  intervalsFeatures <- SDSD_intervals_calculation(intervalsFeatures, peaks)
  intervalsFeatures <- pNN50_intervals_calculation(intervalsFeatures)
  intervalsFeatures <- SDNN_intervals_calculation(intervalsFeatures,peaks)
  intervalsFeatures <- as.data.frame(RMSSD_intervals_calculation(intervalsFeatures,peaks))
  
  
  return(intervalsFeatures)
  
}


extractACC <- function(acc_data, frequency, interval){
  acc_time_calculation(acc_data, frequency)
}

acc_time_calculation <- function(accData, frequency_of_data)
{

  accData$Time <- accData$x * (1000/frequency_of_data)
  return(as.data.frame(accData))
  
}



