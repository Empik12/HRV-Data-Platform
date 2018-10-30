library(DT)

ui <- fluidPage(
  navbarPage("HRV Analysis",
             tabPanel("Dataset",
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("PPG_File", "Choose CSV File with PPG data",
                                    accept = c(
                                      "text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")
                          ),
                          tags$hr(),
                          checkboxInput("header", "Header", TRUE),
                          textInput(inputId = "frequencyIN",
                                    label = "Acquisition frequency (HZ)",
                                    value = "300"),
                          textInput(inputId = "timeFramesSize",
                                    label = "Time frames size (ms)",
                                    value = "15000")
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
                      # tableOutput("extract"),
                      DT::dataTableOutput("peaks")
                      
                      
             ),
             tabPanel("Reasults"
                      
             )
  )
  
  
  
  
)