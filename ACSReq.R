#function to retrireve  detail data from the American Community Survey based on state
getACSData <- function(year,varname,state,APIkey){
  resURL <- paste("http://api.census.gov/data/",year,"/acs/acs1/subject?get=NAME,",varname,
                  "&for=county:*&in=state:",state,"&key=",APIkey,sep="")
  lJSON <- fromJSON(resURL)
  #s <- gsub(",","",varname)
  #count <- nchar(varname) - nchar(s)
  df <- data.frame(lJSON)
  return(df)
}
library (RJSONIO)

fileName <- "APIkey.txt"
APIkey = readChar(fileName, file.info(fileName)$size)

varname <- paste("S0101_C01_005E","S0101_C01_006E","S0101_C01_007E",sep=",")

state = "47"

ACSData <- getACSData("2016",varname,state,APIkey)


head(ACSData,3)