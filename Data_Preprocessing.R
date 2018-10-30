library("pracma")

preprocess_data <- function(rawData)
{# increasing dynamic range of the data
  rawData$sign <- ifelse(rawData$Value < 0, 1, 0)
  rawData$Value <- ifelse(rawData$sign == 1, ((rawData$Value*rawData$Value)-(((rawData$Value*rawData$Value)*2))),
                        (rawData$Value*rawData$Value))
  
  return(rawData$Value)
}

peak_detection_time_calculation <- function(rawData, frequency_of_data)
{
  #  Signal peaks ----
  #cat(rawData$Value)
  peaks <- as.data.frame(findpeaks(as.numeric(rawData$Value), nups = 15, ndowns = 1,zero = "+", npeaks = 10000, minpeakheight = 3, sortstr = FALSE))

  colnames(peaks) <- c("Value", "Index", "From", "To")
  # Counting time of peak
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
NN_intervals_correction <- function(peaks)
{
  peaks$Interval_raw <- peaks$Interval
  
  for(rowN in 1:(as.numeric(length(peaks$Interval)-1))){
    if(rowN > 2) {
      if(peaks$Interval[rowN] > 0.7*(peaks$Interval[rowN-1]+peaks$Interval[rowN+1])){
        peaks$Interval[rowN]=(peaks$Interval[rowN-1]+peaks$Interval[rowN+1])/2
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

NN50_intervals_calculation <- function(countOfIntervalsIn30sec, peaks){
  NN_intervals <- c()
  nn <- 0
  rowNOfIntervals <- 1
  countOfIntervalsIn30sec$NN50 <- 0
  cat(countOfIntervalsIn30sec$NN50[1])
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