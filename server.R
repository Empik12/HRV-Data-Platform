source("Data_Preprocessing.R")
source("UI_Helper.R")
source("ui.R")
library("ggplot2")
library("devtools")
library(DT)
#install_github("timelyportfolio/sweetalertR")






server <- (function(input,output,session) {
  
  # initialization of reactive values
  ppg_data <- reactiveValues(df_data = NULL, peaks = NULL, intervalsFeatures = NULL, DFRows = 0, feedbackDF = NULL, intervalsFeaturesFeedback = NULL,
                             df_data_ECG = NULL)
  
  # observation on File input of PPG Data
  observeEvent(input$PPG_File, {
    # reading data from csv and setting collumn names
    ppg_data$df_data <- read.csv(input$PPG_File$datapath, header = input$headerPPG ,sep = ',', dec = '.', encoding = 'UTF-8')
    colnames(ppg_data$df_data) <- c("Time", "Value") 
  })
  
  observeEvent(input$ECG_File, {
    # reading data from csv and setting collumn names
    ppg_data$df_data_ECG <- read.csv(input$ECG_File$datapath, header = input$headerECG ,sep = ',', dec = '.', encoding = 'UTF-8')
    colnames(ppg_data$df_data_ECG) <- c("Time", "Value") 
  })
  

  
  # observation on features extraction from PPG data into features
  observeEvent(input$features, {
    
    ppg_data$df_data["Value"] <- preprocess_data(ppg_data$df_data["Value"])
    ppg_data$peaks <- peak_detection_time_calculation(ppg_data$df_data["Value"], as.numeric(input$frequencyIN))
    ppg_data$peaks <- intervals_calculation(ppg_data$peaks)
    ppg_data$peaks <- NN_intervals_correction(ppg_data$peaks)
    ppg_data$intervalsFeatures <- NN50_forXsec(ppg_data$peaks, as.numeric(input$timeFramesSize))
    ppg_data$intervalsFeatures <- total_time_calculation(ppg_data$intervalsFeatures)
    ppg_data$intervalsFeatures <- NN50_intervals_calculation(ppg_data$intervalsFeatures, ppg_data$peaks)
    ppg_data$intervalsFeatures <- SDSD_intervals_calculation(ppg_data$intervalsFeatures, ppg_data$peaks)
    ppg_data$intervalsFeatures <- pNN50_intervals_calculation(ppg_data$intervalsFeatures)
    ppg_data$intervalsFeatures <- SDNN_intervals_calculation(ppg_data$intervalsFeatures,ppg_data$peaks)
    ppg_data$intervalsFeatures <- as.data.frame(RMSSD_intervals_calculation(ppg_data$intervalsFeatures,ppg_data$peaks))
    
    ppg_data$DFRows <- as.character(1:length(ppg_data$intervalsFeatures$time))
    
    #normalization
    ppg_data$intervalsFeatures$SDNN <- feature_normalization(ppg_data$intervalsFeatures$SDNN)
    
    ppg_data$intervalsFeaturesFeedback <- merge_data_frames(ppg_data$intervalsFeatures, ppg_data$DFRows)
    
    
    
  })
  
  # observation on features for rendering table with features and feedback possibility
  observeEvent(input$features, {
    output$selection = DT::renderDataTable(
      ppg_data$intervalsFeaturesFeedback[,-(1:2)],
      escape = FALSE, selection = 'none', server = FALSE,
      options = list(dom = 't', paging = FALSE, ordering = FALSE),
      callback = JS("table.rows().every(function(i, tab, row) {
                      var $this = $(this.node());
                      $this.attr('id', this.data()[0]);
                      $this.addClass('shiny-input-radiogroup');
    });
                      Shiny.unbindAll(table.table().node());
                      Shiny.bindAll(table.table().node());")
    )
  }
  
  )
  
  # observe feedback confirmation 
  observeEvent(input$confirmFeedback, {
    cat(sapply(ppg_data$DFRows,function(i) input[[i]]))
    
    ppg_data$intervalsFeaturesFeedback$feedback <- sapply(ppg_data$DFRows,function(i) input[[i]])
  })
  
  
  #### Outputs setting ####
  
  output$contents <- renderTable(ppg_data$df_data)
  output$plotPPG <- renderPlot({
  
     ggplot(data=ppg_data$df_data, aes(as.numeric(ppg_data$df_data$Time), ppg_data$df_data$Value)) +
       geom_line(color="red")
   })
  
  output$plotECG <- renderPlot({

    ggplot(data=ppg_data$df_data_ECG, aes(as.numeric(ppg_data$df_data_ECG$Time), ppg_data$df_data_ECG$Value)) +
      geom_line(color="red")
  })
  
  output$extract <- renderTable(ppg_data$df_data)
 # output$features = DT::renderDT({as.data.frame(ppg_data$intervalsFeatures)})



 
})



