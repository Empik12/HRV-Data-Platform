library(DT) 
#library(sweetalertR)

ui <- fluidPage(
  navbarPage("HRV Analysis",
             tabPanel("Storage setup",
                      textInput(inputId = "AWS_ACCESS_KEY_ID",
                                label = "Amazon Web Services Access Key ID",
                                value = ""),
                      textInput(inputId = "AWS_SECRET_ACCESS_KEY",
                                label = "Amazon Web Services Secret Access Key",
                                value = ""),
                      textInput(inputId = "AWS_DEFAULT_REGION",
                                label = "Amazon Web Services Default Region",
                                value = ""),
                      actionButton("saveStorageInfo", "Save storage access informations")
             ),
                      
             tabPanel("Initialize datasets",
                      fluidRow(
                        column(6,
                               helpText("Data measured in calm state"),
                               wellPanel(
                               fileInput("PPG_File_calm", "Choose CSV File with PPG data",
                                         accept = c(
                                           "text/csv",
                                           "text/comma-separated-values,text/plain",
                                           ".csv")
                               ),
                               checkboxInput("headerPPG_calm", "Header in PPG data.", TRUE),
                               textInput(inputId = "frequencyIN_PPG_calm",
                                         label = "Acquisition frequency (HZ)",
                                         value = "400")
                               ),
                               wellPanel(
                                 fileInput("ECG_File_calm", "Choose CSV File with ECG data",
                                                         accept = c(
                                                           "text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv")
                                               ),
                                               checkboxInput("headerECG_calm", "Header in ECG data.", TRUE),
                                               textInput(inputId = "frequencyIN_ECG_calm",
                                                         label = "Acquisition frequency (HZ)",
                                                         value = "300")
                               ),
                               # wellPanel(
                               #       fileInput("ACC_File", "Choose CSV File with Accelerometer data",
                               #                 accept = c(
                               #                   "text/csv",
                               #                   "text/comma-separated-values,text/plain",
                               #                   ".csv")
                               #       ),
                               #       checkboxInput("headerACC", "Header in Accelerometer data", TRUE),
                               #       textInput(inputId = "frequencyIN_ACC",
                               #                 label = "Acquisition frequency (HZ)",
                               #                 value = "300")
                               # ),
                               actionButton("saveCalmDatasets", "Save datasets")
                              ),
                        column(6,
                               helpText("Data measured under cognitive load"),
                               wellPanel(
                                 fileInput("PPG_File_cogload", "Choose CSV File with PPG data",
                                           accept = c(
                                             "text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")
                                 ),
                                 checkboxInput("headerPPG_cogload", "Header in PPG data.", TRUE),
                                 textInput(inputId = "frequencyIN_PPG_cogload",
                                           label = "Acquisition frequency (HZ)",
                                           value = "400")
                               ),
                               wellPanel(
                                 fileInput("ECG_File_cogload", "Choose CSV File with ECG data",
                                           accept = c(
                                             "text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")
                                 ),
                                 checkboxInput("headerECG_cogload", "Header in ECG data.", TRUE),
                                 textInput(inputId = "frequencyIN_ECG_cogload",
                                           label = "Acquisition frequency (HZ)",
                                           value = "300")
                               ),
                               # wellPanel(
                               #   fileInput("ACC_File", "Choose CSV File with Accelerometer data",
                               #             accept = c(
                               #               "text/csv",
                               #               "text/comma-separated-values,text/plain",
                               #               ".csv")
                               #   ),
                               #   checkboxInput("headerACC", "Header in Accelerometer data", TRUE),
                               #   textInput(inputId = "frequencyIN_ACC",
                               #             label = "Acquisition frequency (HZ)",
                               #             value = "300")
                               # ),
                               actionButton("saveCogLoadDatasets", "Save datasets")

             )
                      
                      )
             ),
             tabPanel("Features",
                      column(6,
                        wellPanel(
                        helpText("Select data source to be included in features extraction:"),
                        column(6,
                               checkboxInput("extractPPG", "PPG", FALSE)
                        ),
                        column(6,
                               checkboxInput("extractECG", "ECG", FALSE)
                        ),
                        textInput(inputId = "timeFramesSize",
                                  label = "Time frame sizes in (ms)",
                                  value = "60000")
                        )
                      ),
                      column(6,
                             wellPanel(
                              actionButton("features", "Extract features!"),
                              actionButton("confirmFeedback", "Confirm feedback")
                             )
                      ),
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