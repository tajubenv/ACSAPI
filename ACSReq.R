#function to retrireve  detail data from the American Community Survey based on state
getACSData <- function(year,varname,state,APIkey){
  resURL <- paste("http://api.census.gov/data/",year,"/acs/acs1/subject?get=NAME,",varname,
                  "&for=county:*&in=state:",state,"&key=",APIkey,sep="")
  lJSON <- fromJSON(resURL)
  s <- gsub(",","",varname)
  count <- nchar(varname) - nchar(s) + 4
  lJSON.cou <- sapply(lJSON, function(x) x[count])
  lJSON.st <- sapply(lJSON, function(x) x[count-1])
  df <- data.frame(lJSON.st, lJSON.cou)
  for (i in 1:(count-2)){
    lJSON.temp <- sapply(lJSON, function(x) x[i])
    tempdf <- data.frame(lJSON.temp)
    df <- bind_cols(df, tempdf)
    next
  }
  colnames(df) <- as.character(unlist(df[1,]))
  df = df[-1,]
  #df <- data.frame(lJSON)
  return(df)
  #return(lJSON)
}
library (RJSONIO)
library(dplyr)

fileName <- "APIkey.txt"
APIkey = readChar(fileName, file.info(fileName)$size)

#Total population and percent in age brackets
varname <- paste("S0101_C01_001E","S0101_C01_002E","S0101_C01_003E","S0101_C01_004E",
                 "S0101_C01_005E","S0101_C01_006E","S0101_C01_007E","S0101_C01_008E",
                 "S0101_C01_009E","S0101_C01_010E","S0101_C01_011E","S0101_C01_012E",
                 "S0101_C01_013E","S0101_C01_014E","S0101_C01_015E","S0101_C01_016E",
                 "S0101_C01_017E","S0101_C01_018E","S0101_C01_019E",sep=",")

#varname <- paste("S0101_C01_001E","S0101_C01_002E","S0101_C01_003E",sep=",")
state = "47"
year = "2016"
ACSData <- getACSData(year,varname,state,APIkey)

#TODO remove county from Name in ACSData
ElecData <- read.csv("ElectionResults.csv")
TNData <- ElecData[(ElecData$State == "TN" & ElecData$Year == "2016"),]

ACSData$NAME <- gsub(" County.*","",ACSData$NAME)

colnames(ACSData)[3] <- "COUNTY"
colnames(TNData)[5] <- "COUNTY" 

FinalData <- merge(ACSData,TNData,by="COUNTY")


