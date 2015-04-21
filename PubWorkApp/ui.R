# ui.R

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("PubWork Application"),
  
  sidebarLayout(position = "right",
                sidebarPanel(
                  helpText("Use this app to make a template for a NEON publication workbook"),
                  br(),
                  verbatimTextOutput("test")
                  ),
                
                mainPanel(
                  # data entry ########################
                  h4("Enter Data Product Information Below"),
                  textInput("dpName", label = h5("Data Product Name (Title Case)")),
                  textInput("dpCaps", label = h5("Data Product Initials (C[apital] L[etters])")),
                  textInput("dpId5", label = h5("Data Product ID Number (5 digits)")),
                  textInput("dpRev3", label = h5("Data Product Revision (3 digits)")),
                  textInput("code3", label = h5("Data Product 3-Letter Code (3 digits)")),
                  textInput("agile6", label = h5("Agile Number for Workbook (6 digits)")),
                  textInput("units", label = h5("Data Product Units (Controlled List)")),
                  textInput("pFieldName", label = h5("Primary Subproduct Name (inPAR)")),
                  textInput("pFieldDesc", label = h5("Primary Subproduct Description (incoming PAR)")),
                  textInput("sFieldName", label = h5("Secondary Subproduct Name (outPAR or NA)")),
                  textInput("sFieldDesc", label = h5("Secondary Subproduct Description (outgoing PAR or NA)")),
                  textInput("tFieldName", label = h5("Tertiary Subproduct Name (sidePAR or NA)")),
                  textInput("tFieldDesc", label = h5("Tertiary Subproduct Description (sideways PAR or NA)")),
                  textInput("fTimeA", label = h5("Smallest Averaging Number (1 for 1-minute period)")),
                  textInput("fDescA", label = h5("Smallest Averaging Unit (minute for 1-minute period)")),
                  textInput("fTimeB", label = h5("Intermediate Averaging Number (5 for 5-minute period or NA)")),  
                  textInput("fDescB", label = h5("Intermediate Averaging Unit (minute for 5-minute period or NA)")),
                  textInput("fTimeC", label = h5("Largest Averaging Number (30 for 30-minute period or NA)")),
                  textInput("fDescC", label = h5("Largest Averaging Unit (minute for 30-minute period or NA)")),
                  fileInput("dirName", label = h3("Choose Output Directory")),
                  actionButton("action", label = h4("Click Here to Generate Workbook"))
                )
  )
))
