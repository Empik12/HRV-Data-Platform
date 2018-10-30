source("Data_Preprocessing.R")
source("ui.R")
library("ggplot2")


server <- (function(input,output) {
  
  ppg_data <- reactiveValues(df_data = NULL, peaks = NULL, intervalsFeatures = NULL)
  
  observeEvent(input$PPG_File, {
    ppg_data$df_data <- read.csv(input$PPG_File$datapath, header = input$header ,sep = ',', dec = '.', encoding = 'UTF-8')
    colnames(ppg_data$df_data) <- c("Time", "Value") 
  })
  
  observeEvent(input$features, {
    ppg_data$df_data["Value"] <- preprocess_data(ppg_data$df_data["Value"])
    ppg_data$peaks <- peak_detection_time_calculation(ppg_data$df_data["Value"], as.numeric(input$frequencyIN))
    ppg_data$peaks <- intervals_calculation(ppg_data$peaks)
    ppg_data$peaks <- NN_intervals_correction(ppg_data$peaks)
    ppg_data$intervalsFeatures <- NN50_forXsec(ppg_data$peaks, as.numeric(input$timeFramesSize))
    ppg_data$intervalsFeatures <- NN50_intervals_calculation(ppg_data$intervalsFeatures, ppg_data$peaks)
    ppg_data$intervalsFeatures <- pNN50_intervals_calculation(ppg_data$intervalsFeatures)
    ppg_data$intervalsFeatures <- SDNN_intervals_calculation(ppg_data$intervalsFeatures,ppg_data$peaks)
    ppg_data$intervalsFeatures <- RMSSD_intervals_calculation(ppg_data$intervalsFeatures,ppg_data$peaks)
    
  })
  
  ## Outputs setting
  output$contents <- renderTable(ppg_data$df_data)
  output$plot <- renderPlot({
  
     ggplot(data=ppg_data$df_data, aes(as.numeric(ppg_data$df_data$Time), ppg_data$df_data$Value)) +
       geom_line(color="red")
   })
  output$extract <- renderTable(ppg_data$df_data)
  output$peaks <- renderTable(ppg_data$intervalsFeatures)

 
})



