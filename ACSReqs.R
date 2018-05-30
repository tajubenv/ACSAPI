#functions to retrireve  detail data from the American Community Survey based on state
getACSDataCounty <- function(ACSYear,year,varname,state,APIkey){
  # TODO:Finish and test this function
  resURL <- paste("http://api.census.gov/data/",year,"/acs/acs",ACSYear,"/subject?get=NAME,",varname,
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
getACSDataSub <- function(year,varname,state,APIkey){
  # TODO:Finish and test this function
  resURL <- paste("http://api.census.gov/data/",year,"/acs/acs",ACSYear,"/subject?get=NAME,",varname,
                  "&for=state:",state,"&key=",APIkey,sep="")
  lJSON <- fromJSON(resURL)
  s <- gsub(",","",varname)
  count <- nchar(varname) - nchar(s) + 4
  #lJSON.cou <- sapply(lJSON, function(x) x[count])
  lJSON.st <- sapply(lJSON, function(x) x[count-1])
  df <- data.frame(lJSON.st)
  for (i in 1:(count-2)){
    lJSON.temp <- sapply(lJSON, function(x) x[i])
    tempdf <- data.frame(lJSON.temp)
    df <- bind_cols(df, tempdf)
    next
  }
  colnames(df) <- as.character(unlist(df[1,]))
  df = df[-1,]
  return(df)
}
getACSDataState <- function(ACSYear,year,varname,state,APIkey){
  # This function pulls data from the ACS API. Disclaimer: I have only tested 5 year data.
  #
  # Args:
  #   ACSYear: determine if accessing 1, 3, or 5 year data.
  #   year: Date of Census data
  #   varname: String of Census variables. They should be comma separated with no spaces.
  #   state: String representing the desired state. i.e. "47" for TN
  #   APIkey: API key requested from the US Census Bureau
  
  if(year=="2016" | year=="2015" | year=="2011"){
    resURL <- paste("http://api.census.gov/data/",year,"/acs/acs",ACSYear,"?get=NAME,",varname,
                  "&for=state:",state,"&key=",APIkey,sep="")
  }
  else{
    resURL <- paste("http://api.census.gov/data/",year,"/acs",ACSYear,"?get=NAME,",varname,
                    "&for=state:",state,"&key=",APIkey,sep="")
  }
  lJSON <- fromJSON(resURL)
  s <- gsub(",","",varname)
  count <- nchar(varname) - nchar(s) + 4
  lJSON.st <- sapply(lJSON, function(x) x[count-1])
  df <- data.frame(lJSON.st)
  for (i in 1:(count-2)){
    lJSON.temp <- sapply(lJSON, function(x) x[i])
    tempdf <- data.frame(lJSON.temp)
    df <- bind_cols(df, tempdf)
    next
  }
  colnames(df) <- as.character(unlist(df[1,]))
  df = df[-1,]
  ix <- 4:count-1
  df[ix] <- lapply(df[ix], as.character)
  df[ix] <- lapply(df[ix], as.numeric)
  return(df)
}