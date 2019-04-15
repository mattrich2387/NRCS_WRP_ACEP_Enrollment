ui<-fluidPage(
  title = "NRCS ACEP-WRP Enrollment",
  shinyjs::useShinyjs(),
  
  fluidRow(id = "title-row",
           column(12,
                  h1("NRCS ACEP-WRP Enrollment"),
                  h6("Created by Matthew Richardson"),
                  h6("Code on", a("GitHub", href = "https://github.com/mattrich2387/NRCS_WRP_ACEP_Enrollment")
                  )
           )
  ),
  br(),
#Plots the plot created by the server
fluidRow(column(4,
                #Creates a button to refresh the page
                submitButton("Update View",icon("refresh")),
                #Creates an input for the start year
                numericInput("x",h3("Start Year"),
                             value=min(Easements@data$Enroll_FY),min=min(Easements@data$Enroll_FY),max=max(Easements@data$Enroll_FY)),
                #Creates an input for the end year
                numericInput("y",h3("End Year"),
                             value=max(Easements@data$Enroll_FY),min=min(Easements@data$Enroll_FY),max=max(Easements@data$Enroll_FY)),
                #Creates an T/F input for the county level option
                checkboxInput("countylevel","County Level",value=F),
                #Creates an T/F input for the border option
                checkboxInput("borders","Border",value=F)),
         column(8,align = "center",
                withSpinner(plotOutput("map")))),
#Changes the style so that all of the State checkboxes line up
tags$head(
  tags$style(
    HTML(
      ".checkbox-inline { 
      margin-left: 0px;
      margin-right: 10px;
      }
      .checkbox-inline+.checkbox-inline {
      margin-left: 0px;
      margin-right: 10px;
      }
      "
    )
    ) 
    ),
#Creates a break between the plot and the options
  hr(),
#Creates the display for the input options
  fluidRow(
    #Splits the row into a column for the simple options
    column(4),
    #Splits the row into a column for the state options
    column(8,checkboxGroupInput("z",h3("States"),choices=z,selected=NULL,inline=T)
                                                          
  )
    )
)
