# server.R

library(shiny)

######################    DEFINE SERVER LOGIC    #######################
shinyServer(function(input, output){

  outVal1 <- eventReactive(
    input$click1, {
      
      # get variables ##################################################
      dirName <- getwd()
      # data product characteristics
      dpName <- input$dpName
      dpCaps <- input$dpCaps
      dpId5 <- input$dpId5
      dpRev3 <- input$dpRev3
      code3 <- input$code3
      atbd6 <- input$atbd6
      units <- input$units
      # sub products
      pFieldName <- input$pFieldName
      pFieldDesc <- input$pFieldDesc
      sFieldName <- input$sFieldName
      sFieldDesc <- input$sFieldDesc
      tFieldName <- input$tFieldName
      tFieldDesc <- input$tFieldDesc
      # time intervals
      fTimeA <- input$fTimeA
      fDescA <- input$fDescA
      fTimeB <- input$fTimeB
      fDescB <- input$fDescB
      fTimeC <- input$fTimeC
      fDescC <- input$fDescC
      ##################################################################
      
      ########  CREATE LONG LIST OF FIELDS AND DESCRIPTIONS  ###########
      longLists <- function(pFieldName, pFieldDesc) {
        pFieldName <- pFieldName
        pFieldDesc <- pFieldDesc
        # create a field name list for the lowest averageing period
        fieldNameL <- list("startDateTime","endDateTime",
                           paste(pFieldName,"Mean",sep=""),
                           paste(pFieldName,"Minimum",sep=""),
                           paste(pFieldName,"Maximum",sep=""),
                           paste(pFieldName,"Variance",sep=""),
                           paste(pFieldName,"NumPts",sep=""),
                           paste(pFieldName,"ExpUncert",sep=""),
                           paste(pFieldName,"StdErMean",sep=""),
                           "rangeQAQCRpt","persistenceQAQCRpt",
                           "stepQAQCRpt","nullQAQCRpt",
                           "gapQAQCRpt","consistencyQAQCRpt",
                           "spikeQAQCRpt",
                           "alphaQAQCRpt","betaQAQCRpt",
                           "rangeFailQM","rangePassQM","rangeNAQM",
                           "persistenceFailQM","persistencePassQM",
                           "persistenceNAQM",
                           "stepFailQM","stepPassQM","stepNAQM",
                           "nullFailQM","nullPassQM","nullNAQM",
                           "gapFailQM","gapPassQM","gapNAQM",
                           "spikeFailQM","spikePassQM","spikeNAQM",
                           "consistencyFailQM","consistencyPassQM",
                           "consistencyNAQM",
                           "alphaQM","betaQM","finalQF"
        )
        ## establish length of longFieldNameL
        wbLength <- length(fieldNameL)
        l1 <- length(fieldNameL)
        ## create a list of field descriptions
        descriptionL <- list("Date and time at which a sampling is initiated",
                             "Date and time at which a sampling is completed",
                             paste("Arithmetic mean of",pFieldDesc,sep=" "),
                             paste("Minimum",pFieldDesc,sep=" "),
                             paste("Maximum",pFieldDesc,sep=" "),
                             paste("Variance in",pFieldDesc,sep=" "),
                             paste("Number of points used to calculate the arithmetic mean of",pFieldDesc,sep=" "),
                             paste("Expanded uncertainty for",pFieldDesc,sep=" "),
                             paste("Standard error of the mean for",pFieldDesc,sep=" "),
                             "Quality assurance and quality control report for the range test, which indicates whether a datum exceeds a realisitc value, detailed in NEON.DOC.011081 (1=fail, 0=pass, -1=NA (i.e., couldn't be run))",
                             "Quality assurance and quality control report for the persistence test, which indicates whether there is a realistic fluctuation of values over a designated period of time, detailed in NEON.DOC.011081 (1=fail, 0=pass, -1=NA (i.e., couldn't be run))",
                             "Quality assurance and quality control report for the step test, which indicates whether unusual jumps in the data exist, detailed in NEON.DOC.011081 (1=fail, 0=pass, -1=NA (i.e., couldn't be run))",
                             "Quality assurance and quality control report for the null test, which indicates a missing datum, detailed in NEON.DOC.011081 (1=fail, 0=pass, -1=NA (i.e., couldn't be run))",
                             "Quality assurance and quality control report for the gap test, which indicates that the datum is missing and is apart of a prolonged period of missing data, detailed in NEON.DOC.011081 (1=fail, 0=pass)",
                             "Quality assurance and quality control report for the consistency test, which indicates whether or not measurements are consistent with co-located measurements, (1=fail, 0=pass, -1=NA (i.e., couldn't be run))",
                             "Quality assurance and quality control report for the spike test, which indicates whether or not a datum has been identified as a spike, detailed in NEON.DOC.000783 (1=fail, 0=pass, -1=NA (i.e., couldn't be run))",
                             "Quality assurance and quality control report for the alpha quality flag, which indicates if one or more quality analysis failed for a datum, detailed in NEON.DOC.001113 (1=fail, 0=pass, -1=NA (i.e., couldn't be run))",
                             "Quality assurance and quality control report for the beta quality flag, which indicates if one or more quality analysis could not be run for a datum, detailed in NEON.DOC.001113 (1=fail, 0=pass, -1=NA (i.e., couldn't be run))",
                             "Quality metric that summarizes the failed outcomes of the range test over the averaging period, as a percent",
                             "Quality metric that summarizes the passed outcomes of the range test over the averaging period, as a percent",
                             "Quality metric that summarizes when the range test could not be run over the averaging period, as a percent",
                             "Quality metric that summarizes the failed outcomes of the persistence test over the averaging period, as a percent",
                             "Quality metric that summarizes the passed outcomes of the persistence test over the averaging period, as a percent",
                             "Quality metric that summarizes when the persistence test could not be run over the averaging period, as a percent",
                             "Quality metric that summarizes the failed outcomes of the step test over the averaging period, as a percent",
                             "Quality metric that summarizes the passed outcomes of the step test over the averaging period, as a percent",
                             "Quality metric that summarizes when the step test could not be run over the averaging period, as a percent",
                             "Quality metric that summarizes the failed outcomes of the null test over the averaging period, as a percent",
                             "Quality metric that summarizes the passed outcomes of the null test over the averaging period, as a percent",
                             "Quality metric that summarizes when the null test could not be run over the averaging period, as a percent",
                             "Quality metric that summarizes the failed outcomes of the gap test over the averaging period, as a percent",
                             "Quality metric that summarizes the passed outcomes of the gap test over the averaging period, as a percent",
                             "Quality metric that summarizes when the gap test could not be run over the averaging period, as a percent",
                             "Quality metric that summarizes the failed outcomes of the spike test over the averaging period, as a percent",
                             "Quality metric that summarizes the passed outcomes of the spike test over the averaging period, as a percent",
                             "Quality metric that summarizes when the spike test could not be run over the averaging period, as a percent",
                             "Quality metric that summarizes the failed outcomes of the consistency test over the averaging period, as a percent",
                             "Quality metric that summarizes the passed outcomes of the consistency test over the averaging period, as a percent",
                             "Quality metric that summarizes when the consistency test could not be run over the averaging period, as a percent",
                             "Quality metric detailing the outcomes of the alpha quality flag over the averaging period, as a percent and detailed in NEON.DOC.001113",
                             "Quality metric detailing the outcomes of the beta quality flag over the averaging period, as a percent and detailed in NEON.DOC.001113",
                             "Quality flag indicating whether a data product has passed or failed an overall assessment of its quality, detailed in NEON.DOC.001113 (1=fail, 0=pass)"
        )
        l2 <- length(descriptionL)
        # test if field name list and definition list are same length
        testL <- l1 == l2
        if(testL==FALSE) {print("Your field name and description lists are different lengths")}
        # create output list
        out <- list(fieldNameL, descriptionL, wbLength, testL)
        return(out)
      }
      ##################################################################
            
      ########  CREATE SHORT LIST OF FIELDS AND DESCRIPTIONS  ##########
      shortLists <- function(pFieldName, pFieldDesc) {
        pFieldName <- pFieldName
        pFieldDesc <- pFieldDesc
        # create a field name list for the higher averageing periods
        fieldNameL <- list("startDateTime","endDateTime",paste(pFieldName,"Mean",sep=""),
                           paste(pFieldName,"Minimum",sep=""),
                           paste(pFieldName,"Maximum",sep=""),
                           paste(pFieldName,"Variance",sep=""),
                           paste(pFieldName,"NumPts",sep=""),
                           paste(pFieldName,"ExpUncert",sep=""),
                           paste(pFieldName,"StdErMean",sep=""),
                           "rangeFailQM","rangePassQM","rangeNAQM",
                           "persistenceFailQM","persistencePassQM",
                           "persistenceNAQM",
                           "stepFailQM","stepPassQM","stepNAQM",
                           "nullFailQM","nullPassQM","nullNAQM",
                           "gapFailQM","gapPassQM","gapNAQM",
                           "spikeFailQM","spikePassQM","spikeNAQM",
                           "consistencyFailQM","consistencyPassQM",
                           "consistencyNAQM",
                           "alphaQM","betaQM","finalQF"
        )
        # establish length of fieldNameL
        wbLength <- length(fieldNameL)
        l1 <- length(fieldNameL)
        # create a list of field descriptions
        descriptionL <- list("Date and time at which a sampling is initiated",
                             "Date and time at which a sampling is completed",
                             paste("Arithmetic mean of",pFieldDesc,sep=" "),
                             paste("Minimum",pFieldDesc,sep=" "),
                             paste("Maximum",pFieldDesc,sep=" "),
                             paste("Variance in",pFieldDesc,sep=" "),
                             paste("Number of points used to calculate the arithmetic mean of",pFieldDesc,sep=" "),
                             paste("Expanded uncertainty for",pFieldDesc,sep=" "),
                             paste("Standard error of the mean for",pFieldDesc,sep=" "),
                             "Quality metric that summarizes the failed outcomes of the range test over the averaging period, as a percent",
                             "Quality metric that summarizes the passed outcomes of the range test over the averaging period, as a percent",
                             "Quality metric that summarizes when the range test could not be run over the averaging period, as a percent",
                             "Quality metric that summarizes the failed outcomes of the persistence test over the averaging period, as a percent",
                             "Quality metric that summarizes the passed outcomes of the persistence test over the averaging period, as a percent",
                             "Quality metric that summarizes when the persistence test could not be run over the averaging period, as a percent",
                             "Quality metric that summarizes the failed outcomes of the step test over the averaging period, as a percent",
                             "Quality metric that summarizes the passed outcomes of the step test over the averaging period, as a percent",
                             "Quality metric that summarizes when the step test could not be run over the averaging period, as a percent",
                             "Quality metric that summarizes the failed outcomes of the null test over the averaging period, as a percent",
                             "Quality metric that summarizes the passed outcomes of the null test over the averaging period, as a percent",
                             "Quality metric that summarizes when the null test could not be run over the averaging period, as a percent",
                             "Quality metric that summarizes the failed outcomes of the gap test over the averaging period, as a percent",
                             "Quality metric that summarizes the passed outcomes of the gap test over the averaging period, as a percent",
                             "Quality metric that summarizes when the gap test could not be run over the averaging period, as a percent",
                             "Quality metric that summarizes the failed outcomes of the spike test over the averaging period, as a percent",
                             "Quality metric that summarizes the passed outcomes of the spike test over the averaging period, as a percent",
                             "Quality metric that summarizes when the spike test could not be run over the averaging period, as a percent",
                             "Quality metric that summarizes the failed outcomes of the consistency test over the averaging period, as a percent",
                             "Quality metric that summarizes the passed outcomes of the consistency test over the averaging period, as a percent",
                             "Quality metric that summarizes when the consistency test could not be run over the averaging period, as a percent",
                             "Quality metric detailing the outcomes of the alpha quality flag over the averaging period, as a percent and detailed in NEON.DOC.001113",
                             "Quality metric detailing the outcomes of the beta quality flag over the averaging period, as a percent and detailed in NEON.DOC.001113",
                             "Quality flag indicating whether a data product has passed or failed an overall assessment of its quality, detailed in NEON.DOC.001113 (1=fail, 0=pass)"
        )
        l2 <- length(descriptionL)
        # test for equal list length
        testL <- l1 == l2
        if(testL==FALSE) {print("Your field name and description lists are different lengths")}
        # create output list
        out <- list(fieldNameL, descriptionL, wbLength, testL)
        return(out)
      }
      ##################################################################
      
      #############  CREATE A PORTION OF A PUB WORKBOOK  ###############
      wbBuilder <- function(listObject, dpName, dpCaps, dpId5, dpRev3, code3, 
                            atbd6, units, pFieldNameI, pFieldDescI, fTimeI, 
                            fDescI, wd=getwd()){
        # define inputs
        fieldNameL <- unlist(listObject[1])
        wbDescriptions <- unlist(listObject[2])
        wbLength <- as.numeric(listObject[3][1])
        # headers for 25 columns
        headers <- c("rank","DPName","dpID","DPNumber","table","tableDescription",
                     "fieldName","description","dataType","units","pubFormat",
                     "exampleEntry","inputs","source","timeIndex","timeDescription",
                     "spatialIndex","spatialDescription","horIndex","horDescription",
                     "vertIndex","vertDescription","downloadPkg","dataCategory",
                     "sampleInfo")
        # header as list
        headerL <- as.list(headers)
        # create rank column
        rankL <- list(as.character(seq(1:wbLength)))
        # create a DPName column
        DPNameL <- list(rep(dpName,wbLength))
        # create a dpID column
        dpIDL <- list(rep(paste("NEON.DOM.SITE.DP1",dpId5,dpRev3, sep="."), 
                          wbLength))
        # create an empty DPNumber column
        DPNumberL <- list(rep("NA", wbLength))
        # create initial table name
        tableL <- list(rep(paste(dpCaps,fTimeI,fDescI, sep="_"), wbLength))
        # create initial table description
        tableDescriptionL <- list(rep(paste(dpName,"averaged over",
                                            fTimeI,fDescI,sep=" "), wbLength))
        # create dataType column
        stringDataType <- "QAQCRpt"
        dateTimeDataType <- "Time"
        dataTypeL <- array(data=, dim=wbLength)
        for(i in 1:wbLength){
          if (grepl(x=fieldNameL[i], pattern=stringDataType, ignore.case=TRUE) == TRUE){
            dataTypeL[i]<-"string"
          }
          else if (grepl(x=fieldNameL[i], pattern=dateTimeDataType, ignore.case=TRUE) == TRUE){
            dataTypeL[i]<-"dateTime"
          }
          else {dataTypeL[i]<-"real"
          }
          dataTypeL<-c(dataTypeL)
        }
        # create a units column
        QFunits <- "QF"
        QMunits <- "QM"
        dataunits <- c("Mean","Minimum","Maximum","ExpUncert","Bulk")
        varunits <- "Variance"
        skew_kurt <- c("Skewness","Kurtosis")
        count_units <- "Pts"
        unitsL <- array(data=,dim =wbLength)
        for(i in 1:wbLength){
          if (grepl(x=fieldNameL[i],pattern=QMunits,ignore.case=TRUE)==TRUE){
            unitsL[i]<-"percent"
          }
          else if (grepl(x=fieldNameL[i],pattern=QFunits,ignore.case=TRUE)==TRUE){
            unitsL[i]<-"NA"
          }
          else if (grepl(x=fieldNameL[i],pattern=count_units,ignore.case=TRUE)==TRUE){
            unitsL[i]<-"number"
          }
          else if (grepl(x=fieldNameL[i],pattern=paste(dataunits,collapse="|"),ignore.case=TRUE)==TRUE){
            unitsL[i]<-units
          }
          else if (grepl(x=fieldNameL[i],pattern=varunits,ignore.case=TRUE)==TRUE){
            unitsL[i]<-paste(units,"Squared",sep="")
          }
          else if (grepl(x=fieldNameL[i],pattern=paste(skew_kurt,collapse="|"),ignore.case=TRUE)==TRUE){
            unitsL[i]<-"NA"
          }
          else {unitsL[i]<-"NA"
          }
          unitsL<-c(unitsL)
        }
        # build a pubFormat column
        dateTimeDataType <- "Time"
        pubFormatL <- array(data=,dim =wbLength)
        for(i in 1:wbLength){
          if (grepl(x=fieldNameL[i], pattern=dateTimeDataType, ignore.case=TRUE) == TRUE){
            pubFormatL[i]<-"yyyy-MM-dd'T'HH:mm:ss'Z'(floor)"
          }
          else {pubFormatL[i]<-"asIs"
          }
          pubFormatL<-c(pubFormatL)
        }
        # create an example entry column
        exampleEntryL <- list(rep("NA", wbLength))
        # create an inputs column
        inputsL <- list(rep("NA", wbLength))
        # create a source column
        sourceL <- list(rep("NA", wbLength))
        # create a time index column
        timeIndexL <- list(rep(fTimeI, wbLength))
        # create a time description column
        timeDescriptionL <- list(rep(fDescI, wbLength))
        # create a spatial index column
        spatialIndexL <- list(rep("NA", wbLength))
        # create a spatial description column
        spatialDescriptionL <- list(rep("NA", wbLength))
        # create a horizontal index column
        horIndexL <- list(rep("HOR", wbLength))
        # create a horizontal description column
        horDescriptionL <- list(rep("variable", wbLength))
        # create a vertical index column
        vertIndexL <- list(rep("VER", wbLength))
        # create a vertical description column
        vertDescriptionL <- list(rep("variable", wbLength))
        # build a download package list
        request<-"Rpt"; finalQF<-"finalQF"; expanded<-c("QM","QF")
        downloadPkgL <- array(data=,dim =wbLength)
        for (i in 1:wbLength){
          if (grepl(x=fieldNameL[i],pattern=request,ignore.case=TRUE)==TRUE){
            downloadPkgL[i]<-"request"
          }
          else if (grepl(x=fieldNameL[i],pattern=finalQF,ignore.case=TRUE)==TRUE){
            downloadPkgL[i]<-"basic"
          }
          else if (grepl(x=fieldNameL[i],pattern=paste(expanded,collapse="|"),ignore.case=TRUE)==TRUE){
            downloadPkgL[i]<-"expanded"
          }
          else {downloadPkgL[i]<-"basic"
          }
          downloadPkgL<-c(downloadPkgL)
        }
        # build a data category column
        keywords <- c("Mean","Minimum","Maximum","Variance","ExpUncert",
                      "Skewness","Kurtosis","Bulk")
        dataCategoryL <- array(data=, dim=wbLength)
        for(i in 1:wbLength){
          if (grepl(x=fieldNameL[i],pattern=paste(keywords,collapse="|"),ignore.case=TRUE)==TRUE){
            dataCategoryL[i]<-"Y"
          }
          else {dataCategoryL[i]<-"N"
          }
          dataCategoryL<-c(dataCategoryL)
        }
        # create a sampleInfo column
        sampleInfoL <- list(rep("NA", wbLength))
        # create output file
        newdata <- data.frame(unlist(rankL),unlist(DPNameL),unlist(dpIDL),
                              unlist(DPNumberL),
                              unlist(tableL),unlist(tableDescriptionL),
                              unlist(fieldNameL),unlist(wbDescriptions),
                              unlist(dataTypeL),unlist(unitsL), 
                              unlist(pubFormatL),unlist(exampleEntryL),
                              unlist(inputsL),unlist(sourceL),unlist(timeIndexL),
                              unlist(timeDescriptionL),unlist(spatialIndexL),
                              unlist(spatialDescriptionL),unlist(horIndexL),
                              unlist(horDescriptionL),unlist(vertIndexL),
                              unlist(vertDescriptionL),unlist(downloadPkgL),
                              unlist(dataCategoryL),unlist(sampleInfoL))
        colnames(newdata) <- headers
        return(newdata)
      }
      ##################################################################   
      
      ###################    ASSEMBLE WORKBOOK PARTS      ##############
      wbAssembler <- function() {
        # make lists of sub products and time intervals
        subFieldNameList <- list(pFieldName, sFieldName, tFieldName)
        subFieldNameList <- (subFieldNameList[subFieldNameList != "NA"])
        subFieldDescList <- list(pFieldDesc, sFieldDesc, tFieldDesc)
        subFieldDescList <- unlist(subFieldDescList[subFieldDescList != "NA"])
        fTimeList <- list(fTimeA, fTimeB, fTimeC)
        fTimeList <- unlist(fTimeList[fTimeList != "NA"])
        fDescList <- list(fDescA, fDescB, fDescC)
        fDescList <- unlist(fDescList[fDescList != "NA"])
        nSubProducts <- as.numeric(length(subFieldNameList))
        nTimeIntervals <- as.numeric(length(fTimeList))
        # output data
        mergedData <- data.frame()
        # try to build two sets of subproducts for first time interval
        for(i in 1:nTimeIntervals){
          if(i == 1){
            for(j in 1:nSubProducts){
              pFieldNameI=subFieldNameList[j]
              pFieldDescI=subFieldDescList[j]
              listObject <- longLists(pFieldName=pFieldNameI, pFieldDesc=pFieldDescI)
              fTimeI=fTimeList[i]
              fDescI=fDescList[i]
              wbPart <- wbBuilder(listObject=listObject, dpName=dpName, dpCaps=dpCaps, 
                                  dpId5=dpId5, dpRev3=dpRev3, code3=code3, atbd6=atbd6, 
                                  units=units, pFieldNameI=pFieldNameI, 
                                  pFieldDescI=pFieldDescI, fTimeI=fTimeI, fDescI=fDescI)
              mergedData <- rbind(mergedData, wbPart)
            }
          }
          if(i > 1){
            for(j in 1:nSubProducts){
              pFieldNameI=subFieldNameList[j]
              pFieldDescI=subFieldDescList[j]
              listObject <- shortLists(pFieldName=pFieldNameI, pFieldDesc=pFieldDescI)
              fTimeI=fTimeList[i]
              fDescI=fDescList[i]
              wbPart <- wbBuilder(listObject=listObject, dpName=dpName, dpCaps=dpCaps, 
                                  dpId5=dpId5, dpRev3=dpRev3, code3=code3, atbd6=atbd6, 
                                  units=units, pFieldNameI=pFieldNameI, 
                                  pFieldDescI=pFieldDescI, fTimeI=fTimeI, fDescI=fDescI)
              mergedData <- rbind(mergedData, wbPart)
            }
          }
        }
        return(mergedData)
      }
      ##################################################################
      
      #####################  MAIN FUNCTION FOR SECOND TAB  #############
      main1 <- function() {
        pubWorkbook <- wbAssembler()
        fileName <- paste(code3, "datapub", 
                          paste("NEONDOC", atbd6, ".txt", sep=""), sep="_")
        fullFileName <- paste(dirName, fileName, sep="/")
        write.table(x=pubWorkbook, file=fullFileName, quote=TRUE, sep="\t",
                    row.names=FALSE, na="NA", fileEncoding="UTF-8")
      }
      ##################################################################
      
      # run functions
      main1()
      
      # send message
      message1 <- "If you look in the working directory, you should now see your publication workbook template."
      message1
    })
  

  outVal2 <- eventReactive(
    input$click2, {
      
      ##################  GET VARIABLES  ###############################
      readmeTemplateLoc <- getwd()
      DPWB_lowest_time_res <- input$DPWB_lowest_time_res
      ATBD <- input$atbd6
      DPDescription <- input$DPDescription
      model <- input$model
      manufacturer <- input$manufacturer
      changeLog <- input$changeLog    
      ##################################################################
      
      ####################   MAIN FUNCTION FOR SECOND TAB  #############
      main2 <- function(readmeTemplateLoc,DPWB_lowest_time_res,
                                   ATBD,DPDescription,model,
                                   manufacturer,changeLog)  {
        # read in data publication workbook
        ingest<-read.table(DPWB_lowest_time_res, header=TRUE, 
                           sep="\t", fileEncoding="UTF-8")
        # create names for output files
        readmeName<-paste("NEONDP1",substr(ingest$dpID[1],19,28),
                          "_readme.txt",sep="")
        varName<-paste("NEONDP1",substr(ingest$dpID[1],19,28),
                       "_variables.csv",sep="")
        # list of columns you want to keep for the variables
        varContent<-c("fieldName", "description", "dataType", 
                      "units","downloadPkg")
        # subset the publication workbook
        var<-ingest[,varContent]
        var <- unique(var)
        # write variables .csv file
        write.csv(var, varName, row.names=FALSE)
        # read in the readme template
        readmeTemp<-readLines(paste(readmeTemplateLoc,
                                    "readmeTemplate.txt",sep="/"))
        # identify the keywords for updating
        replace<-c("DPID","DPNAME","READMENAME","DPDESCRIPTION",
                   "MANUFACTURER","MODEL","VARIABLESFILE","AlgoTBD","CHANGELOG")
        # get the applicable information out of the data publication workbook
        info<-c(as.character(ingest$dpID[1]),as.character(ingest$DPName[1]),
                readmeName,DPDescription,manufacturer,model,varName,
                paste(ATBD," - ","Algorithm Theoretical Basis Document ", 
                      as.character(ingest$DPName[1]), sep=""),changeLog)
        # loop to replace the applicable information in the readme template
        for(i in 1:length(replace)){
          readmeTemp <- gsub(pattern=replace[i], replacement=info[i],
                             x=readmeTemp)
        }
        # write out the data prodcut specific readme file
        writeLines(readmeTemp,readmeName)
      }
      
      # run functions
      main2(readmeTemplateLoc=readmeTemplateLoc,DPWB_lowest_time_res=DPWB_lowest_time_res,
            ATBD=ATBD,DPDescription=DPDescription,model=model,
            manufacturer=manufacturer,changeLog=changeLog)
      
      # send message
      message2 <- "If you look in the working directory, you should now see your readme and variable files."
      message2
      
    })
    
  output$message1 <- renderText({
    outVal1()
    })
  
  output$message2 <- renderText({
    outVal2()
    })
    
})
########################################################################
