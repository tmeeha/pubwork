#################################################
## Code to create IS DP publication workbook
#################################################
#Arguments

##inputFile = The name of the input file with .csv extension
  ##Example: "test.csv"

##inputLoc = location of the file to be uploaded
  ##Example: "C:/Users/dsmith/Documents/FIU/Git/working_copy/FIU-CI/PAR"

##outputLoc = location of the file to be output. If no output location is specified the function will default to the input location.
  ##Example: "C:/Users/dsmith/Documents/FIU/Git/working_copy/FIU-CI/PAR/dataPubInfo"

##Example of running the function
  #dpwbtemp(inputFile="test.csv",inputLoc= "C:/Users/dsmith/Documents/FIU/Git/working_copy/FIU-CI/PAR",outputLoc= "C:/Users/dsmith/Documents/FIU/Git/working_copy/FIU-CI/PAR/dataPubInfo")

###########################################################
dpwbtemp<-function(inputFile,inputLoc,outputLoc=inputLoc){
  
##Set working directory
setwd<-inputLoc
##read in old data pub workbook as characters and replace all blanks with NA
inFile<-read.table(inputFile,header=TRUE,sep=",",na.strings="",colClasses=c("character"))

####################################################
####create new columns for data publication workbook
####################################################

##Determine how many rows should exist in the publication work book. This is based on the number of field names that exist and exculdes and white space that may exist at the bottom of the input file.
wbLength<-sum(!is.na(inFile$fieldName))

##Column 1 - rank
rank<-seq(from=1,length.out =wbLength,by =1)

##Column 2 - DPName
DPName<-rep(inFile$DPName[1],length.out =wbLength)
new<-data.frame(rank,DPName)
##Column 3 - dpID
dpID<-rep(inFile$dpID[1],length.out =wbLength)

##Column 4 - DPNumber
DPNumber<-array(data=,dim =wbLength)

##Column 5 - table
table<-array(data=,dim =wbLength)

##Column 6 - tableDescription
tableDescription<-array(data=,dim =wbLength)

##Column 7 - fieldName
fieldName<-inFile$fieldName[1:wbLength]

##Column 8 - description
description<-inFile$description[1:wbLength]

##Column 9 - dataType
###create entries for data type

#Will search field names for the following and associate "string" as its data type all others will be given "real" (adjust as needed).
stringDataType<-"QAQCRpt"
newtyp<-array(data=,dim =wbLength)

##For loop which assigins the data type based on the field name.
for(i in 1:wbLength){
  
  ##Logical search for keywords if they exist dataType = "string" else "real"
  if (grepl(x=inFile$fieldName[i],pattern=stringDataType,ignore.case=TRUE)==TRUE){
    
    newtyp[i]<-"string"
  }
  
  else{
    newtyp[i]<-"real"
  }
  dataType<-c(newtyp)
}

##Column 10 - units

###creating character strings to allow the loop to assign units to specific field names (adjust as needed).
QFunits<-"QF"
QMunits<-"QM"
dataunits<-c("Mean","Minimum","Maximum","ExpUncert","Bulk")
varunits<-"Variance"
skew_kurt<-c("Skewness","Kurtosis")

##Create array to store unit info
newunits<-array(data=,dim =wbLength)

##For loop which assigins units for the various fields.

for(i in 1:wbLength){
  
  ##Logical search for keywords in field name which is used to assign units
if (grepl(x=inFile$fieldName[i],pattern=QMunits,ignore.case=TRUE)==TRUE){
    newunits[i]<-"percent"
  }
  else if (grepl(x=inFile$fieldName[i],pattern=QFunits,ignore.case=TRUE)==TRUE){
    newunits[i]<-"NA"
  }
  else if (grepl(x=inFile$fieldName[i],pattern=paste(dataunits,collapse="|"),ignore.case=TRUE)==TRUE){
    newunits[i]<-inFile$units[1]
  }
  else if (grepl(x=inFile$fieldName[i],pattern=varunits,ignore.case=TRUE)==TRUE){
    newunits[i]<-paste(inFile$units[1],"Squared",sep="")
  }
  else if (grepl(x=inFile$fieldName[i],pattern=paste(skew_kurt,collapse="|"),ignore.case=TRUE)==TRUE){
  newunits[i]<-"NA"
  }
  else{
    newunits[i]<-"NA"
  }
  units<-c(newunits)
}

##Column 11 - pubFormat
pubFormat<-rep("asIs",length.out =wbLength)

##Column 12 - exampleEntry
exampleEntry<-array(data=,dim =wbLength)

##Column 13 - inputs
inputs<-array(data=,dim =wbLength)

##Column 14 - source
source<-array(data=,dim =wbLength)

##Column 15 - timeIndex
timeIndex<-rep(inFile$timeIndex[1],length.out =wbLength)

##Column 16 - timeDescription
timeDescription<-rep(inFile$timeDescription[1],length.out =wbLength)

##Column 17 spatialIndex
spatialIndex<-array(data=,dim =wbLength)

##Column 18 in new template
spatialDescription<-array(data=,dim =wbLength)

##Column 19 - horIndex
horIndex<-rep("HOR",length.out=wbLength)

##Column 20 - horDescription
horDescription<-rep("variable",length.out=wbLength)

##Column 21 - vertIndex
vertIndex<-rep("VER",length.out=wbLength)

##Column 22 - vertDescription
vertDescription<-rep("variable",length.out=wbLength)

##Column 23 - downloadPkg
  ##basic = provided for every download
  ##expanded = ancillary QAQC information (e.g. quality metrics)
  ##request = generally more detailed quality information (e.g., quality reports)

###creating character strings to allow the loop to assign download pkg info based on field names (adjust as needed).
request<-"Rpt"
finalQF<-"finalQF"
expanded<-c("QM","QF")


##Create array to store unit info
dwnldpkg<-array(data=,dim =wbLength)

##For loop which assigins units for the various fields.

for(i in 1:wbLength){
  
  ##Logical search for keywords to assign the different download packages to
  if (grepl(x=inFile$fieldName[i],pattern=request,ignore.case=TRUE)==TRUE){
    
    dwnldpkg[i]<-"request"
  }
  else if (grepl(x=inFile$fieldName[i],pattern=finalQF,ignore.case=TRUE)==TRUE){
    dwnldpkg[i]<-"basic"
  }
  else if (grepl(x=inFile$fieldName[i],pattern=paste(expanded,collapse="|"),ignore.case=TRUE)==TRUE){
    dwnldpkg[i]<-"expanded"
  }
  else{
    dwnldpkg[i]<-"basic"
  }
  downloadPkg<-c(dwnldpkg)
}


##Column 24 in new template (Y = check if data is available to determine whether data is availble for that time period (e.g., mean, min, max, etc.), N= don't check to see if data is available for that time period (e.g., flags))
dataCategory<-array(data=,dim =wbLength)
datcat<-array(data=,dim =wbLength)

##Keywords used to determine if a column contains data. This is used on the portal to notify a user whether data exists for a give query (adjust as needed).
keywords<-c("Mean","Minimum","Maximum","Variance","ExpUncert","Skewness","Kurtosis","Bulk")

##For loop which assigins a yes="y" or no="no" to a row indicating whether the fieldName applies to data or not.
for(i in 1:wbLength){

  ##Logical search for keywords if they exist datacat = "y" else "n"
if (grepl(x=inFile$fieldName[i],pattern=paste(keywords,collapse="|"),ignore.case=TRUE)==TRUE){

  datcat[i]<-"y"
}

else{
  datcat[i]<-"n"
}
dataCategory<-c(datcat)
}

##Column 25 in new template
sampleInfo<-array(data=,dim =wbLength)


############################################################################################
##Create new template using existing headers as identifies for column location in new format
############################################################################################

new<-data.frame(rank,DPName,dpID,DPNumber,table,tableDescription,fieldName,description,dataType,units,pubFormat,exampleEntry,inputs,source,timeIndex,timeDescription,spatialIndex,spatialDescription,horIndex,horDescription,vertIndex,vertDescription,downloadPkg,dataCategory,sampleInfo)

colnames(new)<-c("rank","DPName","dpID","DPNumber","table","tableDescription","fieldName","description","dataType","units","pubFormat","exampleEntry","inputs","source","timeIndex","timeDescription","spatialIndex","spatialDescription","horIndex","horDescription","vertIndex","vertDescription","downloadPkg","dataCategory","sampleInfo")

##write new publication work book. The publication workbook will be named based on the ATBD and the time resolution of the L1 DP.
write.csv(new, paste(outputLoc,paste("datapub",paste("NEONDOC",inFile$ATBD[1],sep=""),paste(gsub(" ","",inFile$timeDescription[1]),".csv",sep=""),sep="_"),sep="/"), row.names=FALSE, na="")
}

