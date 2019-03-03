source("data_Preprocessing.R")
source("UI_Helper.R")
source("storage_operations.R")
source("ui.R")
library("ggplot2")
library("devtools")
library(DT)
library(aws.s3)
#install_github("timelyportfolio/sweetalertR")






server <- (function(input,output,session) {
  
  AWS_ACCESS_KEY_ID <- 'AWS_ACCESS_KEY_ID'
  AWS_SECRET_ACCESS_KEY <- 'AWS_SECRET_ACCESS_KEY'
  AWS_DEFAULT_REGION <- 'AWS_DEFAULT_REGION'
  options(shiny.maxRequestSize=30*1024^2) 
  
  # initialization of reactive values
  initialization_dataset_calm_state <- reactiveValues(raw_PPG = NULL, preprocessed_PPG = NULL,
                                           raw_ECG = NULL, preprocessed_ECG = NULL,
                                           raw_ACC = NULL, 
                                           peaks = NULL, intervalsFeaturesPPG = NULL, intervalsFeaturesECG = NULL,
                                            DFRows = 0, feedbackDF = NULL, intervalsFeaturesFeedback = NULL,
                                            raw_ECG = NULL, raw_ACC = NULL)
  

  
  observeEvent(input$saveStorageInfo, {
    Sys.setenv("AWS_ACCESS_KEY_ID" = input$AWS_ACCESS_KEY_ID,
               "AWS_SECRET_ACCESS_KEY" = input$AWS_SECRET_ACCESS_KEY,
               "AWS_DEFAULT_REGION" = input$AWS_DEFAULT_REGION
               )
  })
  
  # observation on File input of PPG Data
  observeEvent(input$PPG_File_calm, {
    # reading data from csv and setting collumn names
    initialization_dataset_calm_state$raw_PPG <- NULL
    initialization_dataset_calm_state$raw_PPG <- read.csv(input$PPG_File_calm$datapath, header = input$headerPPG_calm ,sep = ',', dec = '.', encoding = 'UTF-8')
    colnames(initialization_dataset_calm_state$raw_PPG) <- c("Time", "Value") 
  })
  
  observeEvent(input$ECG_File_calm, {
    # reading data from csv and setting collumn names
    initialization_dataset_calm_state$df_data_ECG <- NULL
    initialization_dataset_calm_state$df_data_ECG <- read.csv(input$ECG_File_calm$datapath, header = input$headerECG_calm ,sep = ',', dec = '.', encoding = 'UTF-8')
    colnames(initialization_dataset_calm_state$df_data_ECG) <- c("Time", "Value") 
  })
  
  observeEvent(input$ACC_File, {
    # reading data from csv and setting collumn names
    initialization_dataset_calm_state$df_data_ACC <- NULL
    initialization_dataset_calm_state$df_data_ACC <- read.csv(input$ACC_File$datapath, header = input$headerACC ,sep = ',', dec = '.', encoding = 'UTF-8')
    colnames(initialization_dataset_calm_state$df_data_ACC) <- c("Time", "Value") 
  })
  

  
  observeEvent(input$saveCalmDatasets, {
    
    if(!(is.null(input$PPG_File_calm))){
      saveData(initialization_dataset_calm_state$raw_PPG,"calmPPG")
    }
    if(!(is.null(input$ECG_File_calm))){
      saveData(initialization_dataset_calm_state$raw_ECG,"calmECG")
    }
    if(!(is.null(input$ACC_File))){
      saveData(initialization_dataset_calm_state$raw_ACC,"calmACC")
    }

  })

  
  # observation on features extraction from PPG data into features
  observeEvent(input$features, {
    
    if(input$extractPPG){
        initialization_dataset_calm_state$preprocessed_PPG <<- preprocessPPG(initialization_dataset_calm_state$raw_PPG["Value"])
        initialization_dataset_calm_state$intervalsFeaturesPPG <- extractPPGfeatures(initialization_dataset_calm_state$preprocessed_PPG, as.numeric(input$frequencyIN_PPG_calm), as.numeric(input$timeFramesSize))
        colnames(initialization_dataset_calm_state$intervalsFeaturesPPG) <- c("count_PPG", "time_PPG", "total_time_PPG", "NN50_PPG", "SDSD_PPG","pNN50_PPG", "SDNN_PPG", "RMSSD_PPG")
    }
    if(input$extractECG){
      initialization_dataset$intervalsFeaturesECG <- extractECG(initialization_dataset_calm_state$df_data_ECG["Value"], as.numeric(input$frequencyIN_ECG), as.numeric(input$timeFramesSize))
      colnames(initialization_dataset_calm_state$intervalsFeaturesECG) <- c("count_ECG", "time_ECG", "total_time_ECG", "NN50_ECG", "SDSD_ECG","pNN50_ECG", "SDNN_ECG", "RMSSD_ECG")
    }

    # initialization_dataset$df_data["Value"] <- preprocess_data(initialization_dataset$df_data["Value"])
    # initialization_dataset$peaks <- peak_detection_time_calculation(initialization_dataset$df_data["Value"], as.numeric(input$frequencyIN))
    # initialization_dataset$peaks <- intervals_calculation(initialization_dataset$peaks)
    # initialization_dataset$peaks <- NN_intervals_correction(initialization_dataset$peaks)
    # initialization_dataset$intervalsFeatures <- NN50_forXsec(initialization_dataset$peaks, as.numeric(input$timeFramesSize))
    # initialization_dataset$intervalsFeatures <- total_time_calculation(initialization_dataset$intervalsFeatures)
    # initialization_dataset$intervalsFeatures <- NN50_intervals_calculation(initialization_dataset$intervalsFeatures, initialization_dataset$peaks)
    # initialization_dataset$intervalsFeatures <- SDSD_intervals_calculation(initialization_dataset$intervalsFeatures, initialization_dataset$peaks)
    # initialization_dataset$intervalsFeatures <- pNN50_intervals_calculation(initialization_dataset$intervalsFeatures)
    # initialization_dataset$intervalsFeatures <- SDNN_intervals_calculation(initialization_dataset$intervalsFeatures,initialization_dataset$peaks)
    # initialization_dataset$intervalsFeatures <- as.data.frame(RMSSD_intervals_calculation(initialization_dataset$intervalsFeatures,initialization_dataset$peaks))
    # 
    # 
    # 
    # 
    # 
    # initialization_dataset$DFRows <- as.character(1:length(initialization_dataset$intervalsFeatures$time))
    # 
    # #normalization
    # initialization_dataset$intervalsFeatures$SDNN <- feature_normalization(initialization_dataset$intervalsFeatures$SDNN)
    # 
    # initialization_dataset$intervalsFeaturesFeedback <- merge_data_frames(initialization_dataset$intervalsFeatures, initialization_dataset$DFRows)
    initialization_dataset_calm_state$intervalsFeatures <- NULL
    
    if(input$extractPPG){
      initialization_dataset_calm_state$intervalsFeatures <- merge(initialization_dataset_calm_state$intervalsFeatures,initialization_dataset_calm_state$intervalsFeaturesPPG, by=0, all=TRUE)
    }
    if(input$extractECG){
      initialization_dataset_calm_state$intervalsFeatures <- merge(initialization_dataset_calm_state$intervalsFeatures,initialization_dataset_calm_state$intervalsFeaturesECG, by=0, all=TRUE)
    }
    
    
    
    #initialization_dataset$intervalsFeatures <- initialization_dataset$intervalsFeaturesECG
    
  })
  
  # observation on features for rendering table with features and feedback possibility
  observeEvent(input$features, {
    output$selection = DT::renderDataTable(
      initialization_dataset_calm_state$intervalsFeatures[,-(1:2)],
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
    cat(sapply(initialization_dataset_calm_state$DFRows,function(i) input[[i]]))
    
    initialization_dataset_calm_state$intervalsFeaturesFeedback$feedback <- sapply(initialization_dataset_calm_state$DFRows,function(i) input[[i]])
  })
  
  
  #### Outputs setting ####
  
  output$contents <- renderTable(initialization_dataset_calm_state$raw_PPG)
  output$plotPPG <- renderPlot({
  
     ggplot(data=initialization_dataset_calm_state$raw_PPG, aes(as.numeric(initialization_dataset_calm_state$raw_PPG$Time), initialization_dataset_calm_state$raw_PPG$Value)) +
       geom_line(color="red")
   })
  
  output$plotECG <- renderPlot({

    ggplot(data=initialization_dataset_calm_state$df_data_ECG, aes(as.numeric(initialization_dataset_calm_state$df_data_ECG$Time), initialization_dataset_calm_state$df_data_ECG$Value)) +
      geom_line(color="red")
  })
  
  output$plotACC <- renderPlot({
    
    ggplot(data=initialization_dataset_calm_state$df_data_ACC, aes(as.numeric(initialization_dataset_calm_state$df_data_ACC$Time), initialization_dataset_calm_state$df_data_ACC$Value)) +
      geom_line(color="red")
  })
  
  
  output$extract <- renderTable(initialization_dataset_calm_state$raw_PPG)
 # output$features = DT::renderDT({as.data.frame(initialization_dataset$intervalsFeatures)})



 
})



