
##########################  SIMPLE DATA ENTRY  ##########################
### SET UP WORKING DIRECTORY
## output location
dirName <- getwd()

### DATA PRODUCT CHARACTERISTICS
## official data product name in title case
dpName <- "IR Biological Temperature"
## capital letters from name
dpCaps <- "IRBT"
## 5 digit data product ID code
dpId5 <- "00005"
## 3 digit data product revision number
dpRev3 <- "001"
## three letter data product code for pub workbook file name
code3 <- "irb"
## 6 digit Agile document code for pub workbook file name
agile6 <- "002853"
## standard units for all subproducts
units <- "wattsPerSquareMeter"

### DIFFERENT SUBPRODUCTS IN WORKBOOK
## primary (only) subproduct field name
pFieldName <- "inPAR"
## primary (only) subproduct field description
pFieldDesc <- "incoming PAR"
## second subproduct field name
sFieldName <- "outPAR"
## second subproduct field description
sFieldDesc <- "outgoingPAR"
## third subproduct field name
tFieldName <- ""
## third subproduct field description
tFieldDesc <- ""

### DIFFERENT TIME INTERVALS
## most frequent time index
fTimeA <- "1"
## most frequent time description
fDescA <- "minute"
## next-most frequent time index
fTimeB <- "5"
## next-most frequent time description
fDescB <- "minute"
## third-most frequent time index
fTimeC <- "30"
## third-most frequent time description
fDescC <- "minute"
#################################################################






########  CREATE LONG LIST OF FIELDS AND DESCRIPTIONS  ##########
longLists <- function() {
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
  ## create a list of field descriptions --------------------------
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
  testL <- l1 == l2
  out <- list(fieldNameL, descriptionL, wbLength, testL)
  return(out)
}
##################################################################




########  CREATE SHORT LIST OF FIELDS AND DESCRIPTIONS  ##########
shortLists <- function() {
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
  testL <- l1 == l2
  out <- list(fieldNameL, descriptionL, wbLength, testL)
  return(out)
}
###############################################################


# test list building functions
t1 <- shortLists()
t1
t1 <- longLists()
t1











## headers for 25 columns
headers <- c("rank","DPName","dpID","DPNumber","table","tableDescription",
             "fieldName","description","dataType","units","pubFormat",
             "exampleEntry","inputs","source","timeIndex","timeDescription",
             "spatialIndex","spatialDescription","horIndex","horDescription",
             "vertIndex","vertDescription","downloadPkg","dataCategory",
             "sampleInfo"); headers

# header list 
headerL <- as.list(headers); length(headerL); headerL


## create rank column ----------------------------------------
rankL <- list(as.character(seq(1:wbLength))); rankL

## create a DPName column ------------------------------------
DPNameL <- list(rep(dpName,wbLength)); DPNameL

## create a dpID column ------------------------------------
dpIDL <- list(rep(paste("NEON.DOM.SITE.DP1",dpId5,dpRev3, sep="."), 
                  wbLength)); dpIDL

## create an empty DPNumber column ------------------------------------
DPNumberL <- list(rep("NA", wbLength)); DPNumberL

## define initial time index and description ---------------------
fTimeI <- fTimeA
fDescI <- fDescA

## create initial table name ---------------------
tableL <- list(rep(paste(dpCaps,fTimeI,fDescI, sep="_"), wbLength)); tableL

## create initial table description ---------------------
tableDescriptionL <- list(rep(paste(dpName,"averaged over",
                                    fTimeI,fDescI,sep=" "), wbLength)); tableDescriptionL



## create dataType column ------------------------------------------
stringDataType <- "QAQCRpt"
dateTimeDataType <- "Time"
dataTypeL <- array(data=, dim=wbLength)

for(i in 1:wbLength){
  if(grepl(x=fieldNameL[i], pattern=stringDataType, ignore.case=TRUE) == TRUE){
    dataTypeL[i]<-"string"
  }
  else if (grepl(x=fieldNameL[i], pattern=dateTimeDataType, ignore.case=TRUE) == TRUE){
    dataTypeL[i]<-"dateTime"
  }
  else{
    dataTypeL[i]<-"real"
  }
  dataTypeL<-c(dataTypeL)
}; dataTypeL

## create a units column  -----------------------------------
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
  else{
    unitsL[i]<-"NA"
  }
  unitsL<-c(unitsL)
}; unitsL

## build a pubFormat column -----------------------
pubFormatL <- array(data=,dim =wbLength)
dateTimeDataType <- "Time"

for(i in 1:wbLength){
  if(grepl(x=fieldNameL[i], pattern=dateTimeDataType, ignore.case=TRUE) == TRUE){
    pubFormatL[i]<-"yyyy-MM-dd'T'HH:mm:ss'Z'(floor)"
  }
  else{
    pubFormatL[i]<-"asIs"
  }
  pubFormatL<-c(pubFormatL)
}; pubFormatL

## create an example entry column  --------------------
exampleEntryL <- list(rep("NA", wbLength)); exampleEntryL

## create an inputs column  --------------------
inputsL <- list(rep("NA", wbLength)); inputsL

## create a source column  --------------------
sourceL <- list(rep("NA", wbLength)); sourceL

## create a time index column  --------------------
timeIndexL <- list(rep(fTimeI, wbLength)); timeIndexL

## create a time description column  --------------------
timeDescriptionL <- list(rep(fDescI, wbLength)); timeDescriptionL

## create a spatial index column  --------------------
spatialIndexL <- list(rep("NA", wbLength)); spatialIndexL

## create a spatial description column  --------------------
spatialDescriptionL <- list(rep("NA", wbLength)); spatialDescriptionL

## create a horizontal index column -----------------
horIndexL <- list(rep("HOR", wbLength)); horIndexL

## create a horizontal description column --------------
horDescriptionL <- list(rep("variable", wbLength)); horDescriptionL

## create a vertical index column ----------------------
vertIndexL <- list(rep("VER", wbLength)); vertIndexL

## create a vertical description column --------------
vertDescriptionL <- list(rep("variable", wbLength)); vertDescriptionL

## build a download package list -----------------------
downloadPkgL <- array(data=,dim =wbLength)
request<-"Rpt"; finalQF<-"finalQF"; expanded<-c("QM","QF")

for(i in 1:wbLength){
  if (grepl(x=fieldNameL[i],pattern=request,ignore.case=TRUE)==TRUE){
    downloadPkgL[i]<-"request"
  }
  else if (grepl(x=fieldNameL[i],pattern=finalQF,ignore.case=TRUE)==TRUE){
    downloadPkgL[i]<-"basic"
  }
  else if (grepl(x=fieldNameL[i],pattern=paste(expanded,collapse="|"),ignore.case=TRUE)==TRUE){
    downloadPkgL[i]<-"expanded"
  }
  else{
    downloadPkgL[i]<-"basic"
  }
  downloadPkgL<-c(downloadPkgL)
}; downloadPkgL

## build a data category column ---------------------
dataCategoryL <- array(data=, dim=wbLength)
keywords <- c("Mean","Minimum","Maximum","Variance","ExpUncert","Skewness","Kurtosis","Bulk")

for(i in 1:wbLength){
  if (grepl(x=fieldNameL[i],pattern=paste(keywords,collapse="|"),ignore.case=TRUE)==TRUE){
    dataCategoryL[i]<-"Y"
  }
  else{
    dataCategoryL[i]<-"N"
  }
  dataCategoryL<-c(dataCategoryL)
}; dataCategoryL

## create a sampleInfo column  --------------------
sampleInfoL <- list(rep("NA", wbLength)); sampleInfoL

## create output file  ---------------------------
newdata1 <- data.frame(unlist(rankL),unlist(DPNameL),unlist(dpIDL),
                      unlist(DPNumberL),
                      unlist(tableL),unlist(tableDescriptionL),
                      unlist(fieldNameL),unlist(descriptionL),
                      unlist(dataTypeL),unlist(unitsL), 
                      unlist(pubFormatL),unlist(exampleEntryL),
                      unlist(inputsL),unlist(sourceL),unlist(timeIndexL),
                      unlist(timeDescriptionL),unlist(spatialIndexL),
                      unlist(spatialDescriptionL),unlist(horIndexL),
                      unlist(horDescriptionL),unlist(vertIndexL),
                      unlist(vertDescriptionL),unlist(downloadPkgL),
                      unlist(dataCategoryL),unlist(sampleInfoL)); head(newdata1)

















#######################   CODE BLOCK TWO  ##################################

## headers for 25 columns
headers <- c("rank","DPName","dpID","DPNumber","table","tableDescription",
             "fieldName","description","dataType","units","pubFormat",
             "exampleEntry","inputs","source","timeIndex","timeDescription",
             "spatialIndex","spatialDescription","horIndex","horDescription",
             "vertIndex","vertDescription","downloadPkg","dataCategory",
             "sampleInfo"); headers

# header list 
headerL <- as.list(headers); length(headerL); headerL



## create rank column ----------------------------------------
rankL <- list(as.character(seq(1:wbLength))); rankL

## create a DPName column ------------------------------------
DPNameL <- list(rep(dpName,wbLength)); DPNameL

## create a dpID column ------------------------------------
dpIDL <- list(rep(paste("NEON.DOM.SITE.DP1",dpId5,dpRev3, sep="."), 
                  wbLength)); dpIDL

## create an empty DPNumber column ------------------------------------
DPNumberL <- list(rep("NA", wbLength)); DPNumberL

## define initial time index and description ---------------------
fTimeI <- fTimeB
fDescI <- fDescB

## create initial table name ---------------------
tableL <- list(rep(paste(dpCaps,fTimeI,fDescI, sep="_"), wbLength)); tableL

## create initial table description ---------------------
tableDescriptionL <- list(rep(paste(dpName,"averaged over",
                                    fTimeI,fDescI,sep=" "), wbLength)); tableDescriptionL


## create dataType column ------------------------------------------
stringDataType <- "QAQCRpt"
dateTimeDataType <- "Time"
dataTypeL <- array(data=, dim=wbLength)

for(i in 1:wbLength){
  if(grepl(x=fieldNameL[i], pattern=stringDataType, ignore.case=TRUE) == TRUE){
    dataTypeL[i]<-"string"
  }
  else if (grepl(x=fieldNameL[i], pattern=dateTimeDataType, ignore.case=TRUE) == TRUE){
    dataTypeL[i]<-"dateTime"
  }
  else{
    dataTypeL[i]<-"real"
  }
  dataTypeL<-c(dataTypeL)
}; dataTypeL

## create a units column  -----------------------------------
QFunits <- "QF"
QMunits <- "QM"
dataunits <- c("Mean","Minimum","Maximum","ExpUncert","Bulk")
varunits <- "Variance"
skew_kurt <- c("Skewness","Kurtosis")
unitsL <- array(data=,dim =wbLength)

for(i in 1:wbLength){
  if (grepl(x=fieldNameL[i],pattern=QMunits,ignore.case=TRUE)==TRUE){
    unitsL[i]<-"percent"
  }
  else if (grepl(x=fieldNameL[i],pattern=QFunits,ignore.case=TRUE)==TRUE){
    unitsL[i]<-"NA"
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
  else{
    unitsL[i]<-"NA"
  }
  unitsL<-c(unitsL)
}; unitsL

## build a pubFormat column -----------------------
pubFormatL <- array(data=,dim =wbLength)
dateTimeDataType <- "Time"

for(i in 1:wbLength){
  if(grepl(x=fieldNameL[i], pattern=dateTimeDataType, ignore.case=TRUE) == TRUE){
    pubFormatL[i]<-"yyyy-MM-dd'T'HH:mm:ss'Z'(floor)"
  }
  else{
    pubFormatL[i]<-"asIs"
  }
  pubFormatL<-c(pubFormatL)
}; pubFormatL

## create an example entry column  --------------------
exampleEntryL <- list(rep("NA", wbLength)); exampleEntryL

## create an inputs column  --------------------
inputsL <- list(rep("NA", wbLength)); inputsL

## create a source column  --------------------
sourceL <- list(rep("NA", wbLength)); sourceL

## create a time index column  --------------------
timeIndexL <- list(rep(fTimeI, wbLength)); timeIndexL

## create a time description column  --------------------
timeDescriptionL <- list(rep(fDescI, wbLength)); timeDescriptionL

## create a spatial index column  --------------------
spatialIndexL <- list(rep("NA", wbLength)); spatialIndexL

## create a spatial description column  --------------------
spatialDescriptionL <- list(rep("NA", wbLength)); spatialDescriptionL

## create a horizontal index column -----------------
horIndexL <- list(rep("HOR", wbLength)); horIndexL

## create a horizontal description column --------------
horDescriptionL <- list(rep("variable", wbLength)); horDescriptionL

## create a vertical index column ----------------------
vertIndexL <- list(rep("VER", wbLength)); vertIndexL

## create a vertical description column --------------
vertDescriptionL <- list(rep("variable", wbLength)); vertDescriptionL

## build a download package list -----------------------
downloadPkgL <- array(data=,dim =wbLength)
request<-"Rpt"; finalQF<-"finalQF"; expanded<-c("QM","QF")

for(i in 1:wbLength){
  if (grepl(x=fieldNameL[i],pattern=request,ignore.case=TRUE)==TRUE){
    downloadPkgL[i]<-"request"
  }
  else if (grepl(x=fieldNameL[i],pattern=finalQF,ignore.case=TRUE)==TRUE){
    downloadPkgL[i]<-"basic"
  }
  else if (grepl(x=fieldNameL[i],pattern=paste(expanded,collapse="|"),ignore.case=TRUE)==TRUE){
    downloadPkgL[i]<-"expanded"
  }
  else{
    downloadPkgL[i]<-"basic"
  }
  downloadPkgL<-c(downloadPkgL)
}; downloadPkgL

## build a data category column ---------------------
dataCategoryL <- array(data=, dim=wbLength)
keywords <- c("Mean","Minimum","Maximum","Variance","ExpUncert","Skewness","Kurtosis","Bulk")

for(i in 1:wbLength){
  if (grepl(x=fieldNameL[i],pattern=paste(keywords,collapse="|"),ignore.case=TRUE)==TRUE){
    dataCategoryL[i]<-"Y"
  }
  else{
    dataCategoryL[i]<-"N"
  }
  dataCategoryL<-c(dataCategoryL)
}; dataCategoryL

## create a sampleInfo column  --------------------
sampleInfoL <- list(rep("NA", wbLength)); sampleInfoL

## create output file  ---------------------------

newdata2 <- data.frame(unlist(rankL),unlist(DPNameL),unlist(dpIDL),
                      unlist(DPNumberL),
                      unlist(tableL),unlist(tableDescriptionL),
                      unlist(fieldNameL),unlist(descriptionL),
                      unlist(dataTypeL),unlist(unitsL), 
                      unlist(pubFormatL),unlist(exampleEntryL),
                      unlist(inputsL),unlist(sourceL),unlist(timeIndexL),
                      unlist(timeDescriptionL),unlist(spatialIndexL),
                      unlist(spatialDescriptionL),unlist(horIndexL),
                      unlist(horDescriptionL),unlist(vertIndexL),
                      unlist(vertDescriptionL),unlist(downloadPkgL),
                      unlist(dataCategoryL),unlist(sampleInfoL)); head(newdata2)







###############################  CODE BLOCK THREE  ######################

## stack files
newdata3 <- rbind(newdata1, newdata2)

colnames(newdata3) <- headers; head(newdata3)

dirName <- getwd(); dirName
fileName <- paste(code3, "datapub", paste("NEONDOC", agile6, ".txt", sep=""), sep="_"); fileName
fullFileName <- paste(dirName, fileName, sep="/")

write.table(x=newdata3, file=fullFileName, quote=TRUE, sep="\t",
            row.names=FALSE, na="NA", fileEncoding="UTF-8")




