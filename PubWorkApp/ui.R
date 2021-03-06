# ui.R

library(shiny)

##################    define UI for application   #######################
shinyUI(fluidPage(
  
  # application title
  titlePanel("PubWork Application"),
  
  # application layout
  tabsetPanel(type = "tabs",
              
      # first tab
      tabPanel("Publication Workbook",
                sidebarLayout(position = "right",
                              
                     # sidebar
                     sidebarPanel(
                       helpText(h4("Instructions")),
                       br(),
                       helpText("Use this app to make a NEON publication workbook template for 
                                sensor-based data products."),
                       helpText("Data product name is the official name from the catalog, written in title 
                                case, such as 'IR Biological Temperature.'"),
                       helpText("Data product initials are the capital letters corresponding with 
                                the name, such as 'IRBT'."),
                       helpText("Data product ID number is the 5 digit code from the catalog, such as '00054'."),
                       helpText("Data product revision number is the 3 digit revision number, such as '001'."),
                       helpText("The ATBD number is the 6 digit number, specific to the product, used in the 
                                publication workbook file name."),
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
                       textOutput("message1")
                      ),
                     
                     # main panel
                     mainPanel(
                       br(),
                       h4("Data Entry"),
                       br(),
                       textInput("dpName", label = h5("Data Product Name")),
                       textInput("dpCaps", label = h5("Data Product Initials")),
                       textInput("dpId5", label = h5("Data Product ID Number")),
                       textInput("dpRev3", label = h5("Data Product Revision")),
                       textInput("code3", label = h5("Data Product 3-Letter Code")),
                       textInput("atbd6", label = h5("ATBD Number for File Name")),
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
                       actionButton("click1", label = h4("CLICK TO GENERATE WORKBOOK"))
                      )
                    )
            ),
      
      # second tab
      tabPanel("Read Me and Variables Files",
                sidebarLayout(position = "right",
                              
                    # sidebar
                    sidebarPanel(
                      helpText(h4("Instructions")),
                      br(),
                      helpText("Use this app to make readme and variables files for a 
                               sensor-based data product."),
                      helpText("Enter the complete data product description, from NEON.DOC.002652."),
                      helpText("Enter the name of the publication workbook, such as 'abc_datapub_NEONDOC123456.txt'."),
                      helpText("Enter the name of the sensor manufacturer, such as 'LI-COR'."),
                      helpText("Enter the model name of the sensor, such as 'LI-191'."),
                      helpText("Change log is used to report changes in how data were 
                                collected or processed, such as '2017-11-20: Updated algorithm 
                                to include temperature dependence'."),
                      helpText("Click the button at the bottom to run application."),
                      br(),
                      br(),
                      textOutput("message2")
                    ),
                    
                    # main panel
                    mainPanel(
                      br(),
                      h4("Data Entry"),
                      br(),
                      textInput("DPDescription", 
                                label = h5("Data Product Description Text")),
                      textInput("DPWB_lowest_time_res", 
                                label = h5("Publication Workbook File Name")),
                      textInput("manufacturer", 
                                label = h5("Sensor Manufacturer")),
                      textInput("model", 
                                label = h5("Sensor Model")),
                      textInput("changeLog", 
                                label = h5("Change Log Text")),
                      br(),
                      br(),
                      actionButton("click2", label = h4("CLICK TO GENERATE YOUR FILES"))
                    )
                )
            )
    )
  )
)
#########################################################################