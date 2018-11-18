library(DT) 
#library(sweetalertR)

ui <- fluidPage(
  navbarPage("HRV Analysis",
             tabPanel("Dataset",
                      fluidRow(
                        column(3,
                               wellPanel(
                               fileInput("PPG_File", "Choose CSV File with PPG data",
                                         accept = c(
                                           "text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")
                               ),
                               checkboxInput("headerPPG", "Header in PPG data.", TRUE),
                               textInput(inputId = "frequencyIN",
                                         label = "Acquisition frequency (HZ)",
                                         value = "300")
                               ),
                               wellPanel(
                                 fileInput("ECG_File", "Choose CSV File with ECG data",
                                                         accept = c(
                                                           "text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv")
                                               ),
                                               checkboxInput("headerECG", "Header in ECG data.", TRUE),
                                               textInput(inputId = "frequencyIN_ECG",
                                                         label = "Acquisition frequency (HZ)",
                                                         value = "300")
                               ),
                               wellPanel(
                                     fileInput("ACC_File", "Choose CSV File with Accelerometer data",
                                               accept = c(
                                                 "text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")
                                     ),
                                     checkboxInput("headerACC", "Header in Accelerometer data", TRUE),
                                     textInput(inputId = "frequencyIN_ACC",
                                               label = "Acquisition frequency (HZ)",
                                               value = "300")
                               ),
                               wellPanel(
                                     fileInput("PD_File", "Choose CSV File with PD data",
                                               accept = c(
                                                 "text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv")
                                     ),
                                     checkboxInput("headerPD", "Header in PD data", TRUE),
                                     textInput(inputId = "frequencyIN_PD",
                                               label = "Acquisition frequency (HZ)",
                                               value = "300")
                               )
                              ),
                        column(9,
                                   # plotOutput("plot"),
                                   # tableOutput("contents")
                                   navbarPage("Data",
                                              tabPanel("Plot",
                                                       plotOutput("plotPPG"),
                                                       plotOutput("plotECG")

                                              ),
                                              tabPanel("Raw",
                                                       tableOutput("contents")
                                              )
                               )

             )
                      
                      )
             ),
             tabPanel("Features",
                    
                      helpText("Select data source to be included in features extraction:"),
                      checkboxInput("extractPPG", "PPG", TRUE),
                      checkboxInput("extractECG", "ECG", TRUE),
                      textInput(inputId = "timeFramesSize",
                                label = "Time frame sizes in (ms)",
                                value = "60000"),
                      actionButton("features", "Extract features!"),
                      actionButton("confirmFeedback", "Confirm feedback"),
                      #sweetalert(selector = "#confirmFeedback", event = "onclick", text = "Are you sure?",
                      #           title = "Feedback", showConfirmButton = TRUE, confirmButtonText = "OK"),
                      # tableOutput("extract"),
                      #DT::DTOutput('peaks'),
                        DT::DTOutput('selection')
 
                      
                      
                      
             ),
             tabPanel("Reasults"
                      
             )
  )
  
  
  
  
)