library(DT)

ui <- fluidPage(
  navbarPage("HRV Analysis",
             tabPanel("Dataset",
                      sidebarLayout(
                        sidebarPanel(
                          #### PPG Data loading ####
                          fileInput("PPG_File", "Choose CSV File with PPG data",
                                    accept = c(
                                      "text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")
                          ),
                          checkboxInput("headerPPG", "Header", TRUE),
                          textInput(inputId = "frequencyIN",
                                    label = "Acquisition frequency (HZ)",
                                    value = "300"),
                          textInput(inputId = "timeFramesSize",
                                    label = "Time frames size (ms)",
                                    value = "15000"),
                          #### ECG Data loading ####
                          tags$hr(),
                          fileInput("ECG_File", "Choose CSV File with ECG data",
                                    accept = c(
                                      "text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")
                          ),
                          checkboxInput("headerECG", "Header", TRUE),
                          textInput(inputId = "frequencyIN_ECG",
                                    label = "Acquisition frequency (HZ)",
                                    value = "300"),
                          textInput(inputId = "timeFramesSize_ECG",
                                    label = "Time frames size (ms)",
                                    value = "15000")
                          #### PD Data loading ####
                        ),
                        mainPanel(
                          # plotOutput("plot"),
                          # tableOutput("contents")
                          navbarPage("Data",
                                     tabPanel("Plot",
                                              plotOutput("plot")
                                              
                                     ),
                                     tabPanel("Raw",
                                              tableOutput("contents")
                                     )
                          )
                          
                          
                          
                        )
                      )
             ),
             tabPanel("Features",
                      actionButton("features", "Extract features!"),
                      actionButton("confirmFeedback", "Confirm feedback"),
                      # tableOutput("extract"),
                      #DT::DTOutput('peaks'),
                        DT::DTOutput('selection')
 
                      
                      
                      
             ),
             tabPanel("Reasults"
                      
             )
  )
  
  
  
  
)