# ui.R

library(shiny)

##################    define UI for application   #######################
shinyUI(fluidPage(
  
  
  
  # application title
  titlePanel("PubWork Application"),
  
  
  
  # page layout
  sidebarLayout(position = "right",
                
                
                
                # sidebar
                sidebarPanel(
                  # instructions
                  helpText(h4("Instructions")),
                  br(),
                  helpText("Use this app to make a NEON publication workbook template for 
                           sensor-based data products."),
                  br(),
                  helpText("Data product name is the official name from the catalog, written in title 
                            case, such as 'IR Biological Temperature.'"),
                  helpText("Data product initials are the capital letters corresponding with 
                           the name, such as 'IRBT'."),
                  helpText("Data product ID number is the 5 digit code from the catalog, such as '00054'."),
                  helpText("Data product revision number is the 3 digit revision number, such as '001'."),
                  helpText("The Agile number is the 6 digit number specific to the publication workbook, 
                           usually created in Agile in advance as a placeholder for eventual submission."),
                  helpText("Data product units come from a controlled list, accessed via a DPS shiny app 
                           by Claire Lunch."),
                  helpText("Primary subproduct name is a controlled term in camel case, such as 'inPAR'."),
                  helpText("Primary subproduct description is a brief description of the subproduct name, such as
                           'incoming PAR'."),
                  helpText("Secondary subproduct name is a controlled term in camel case, such as 'outPAR'. 
                           Use 'NA' if there is only one subproduct."),
                  helpText("Secondary subproduct description is a brief description of the subproduct name, such as
                           'outgoing PAR'. Use 'NA' if there is only one subproduct."),
                  helpText("Tertiary subproduct name is a controlled term in camel case, such as 'sidePAR'. 
                           Use 'NA' if there are only two subproducts."),
                  helpText("Tertiary subproduct description is a brief description of the subproduct name, such as
                           'sideways PAR'. Use 'NA' if there are only two subproducts"),
                  helpText("Smallest averaging number is, for example, '1' if the smallest averaging interval 
                           is 1 minute."),
                  helpText("Smallest averaging unit is, for example, 'minute' if the smallest averaging interval 
                           is 1 minute."),
                  helpText("Intermediate averaging number is, for example, '5' if the intermediate averaging interval 
                           is 5 minutes. Use 'NA' if there is only one averaging interval."),
                  helpText("Intermediate averaging unit is, for example, 'minute' if the intermediate averaging interval 
                           is 5 minutes. Use 'NA' if there is only one averaging interval."),
                  helpText("Largest averaging number is, for example, '30' if the largest averaging interval 
                           is 30 minutes. Use 'NA' if there are only two averaging intervals."),
                  helpText("Largest averaging unit is, for example, 'minute' if the largest averaging interval 
                           is 30 minutes. Use 'NA' if there are only two averaging intervals."),
                  #helpText("Output directory is the location where you want the publication workbook
                           #template to be saved. For example, 'C:\\\\Users\\\\username\\\\Desktop'."),
                  helpText("Click the button at the bottom to run application."),
                  br(),
                  br(),
                  textOutput("test")
                  ),
                
                
                
                # main panel
                mainPanel(
                  # data entry
                  br(),
                  h4("Data Entry"),
                  br(),
                  textInput("dpName", label = h5("Data Product Name")),
                  textInput("dpCaps", label = h5("Data Product Initials")),
                  textInput("dpId5", label = h5("Data Product ID Number")),
                  textInput("dpRev3", label = h5("Data Product Revision")),
                  textInput("code3", label = h5("Data Product 3-Letter Code")),
                  textInput("agile6", label = h5("Agile Number for Workbook")),
                  textInput("units", label = h5("Data Product Units")),
                  textInput("pFieldName", label = h5("Primary Subproduct Name")),
                  textInput("pFieldDesc", label = h5("Primary Subproduct Description")),
                  textInput("sFieldName", label = h5("Secondary Subproduct Name")),
                  textInput("sFieldDesc", label = h5("Secondary Subproduct Description")),
                  textInput("tFieldName", label = h5("Tertiary Subproduct Name")),
                  textInput("tFieldDesc", label = h5("Tertiary Subproduct Description")),
                  textInput("fTimeA", label = h5("Smallest Averaging Number")),
                  textInput("fDescA", label = h5("Smallest Averaging Unit")),
                  textInput("fTimeB", label = h5("Intermediate Averaging Number")),  
                  textInput("fDescB", label = h5("Intermediate Averaging Unit")),
                  textInput("fTimeC", label = h5("Largest Averaging Number")),
                  textInput("fDescC", label = h5("Largest Averaging Unit")),
                  #textInput("dirName", label = h5("Output Directory")),
                  br(),
                  br(),
                  actionButton("click", label = h4("CLICK TO GENERATE WORKBOOK"))
                )
  )
))
#########################################################################