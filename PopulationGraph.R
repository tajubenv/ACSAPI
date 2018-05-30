#
# Author: Tyler Jubenville
# This script currently pulls population age data from the American Community Survey 5 year data from 2011-2016 and plots it.

rm(list=ls())
library (RJSONIO)
library(dplyr)
library(ggplot2)
library(tidyr)
source("ACSReqs.R")

# Import APIkey from text file.
fileName <- "APIkey.txt"
APIkey = readChar(fileName, file.info(fileName)$size)

# Sate number for TN
state <- "47"
year <- c("2009","2010","2011","2012","2013","2014","2015","2016")

# Add all age variables by variable gender to the 
varname <- paste("B01001_001E","B01001_002E","B01001_003E",
                 "B01001_004E","B01001_005E","B01001_006E",
                 "B01001_007E","B01001_008E","B01001_009E",
                 "B01001_010E","B01001_011E","B01001_012E",
                 "B01001_013E","B01001_014E","B01001_015E",
                 "B01001_016E","B01001_017E","B01001_018E",
                 "B01001_019E","B01001_020E","B01001_021E",
                 "B01001_022E","B01001_023E","B01001_024E",
                 "B01001_025E","B01001_026E","B01001_027E",
                 "B01001_028E","B01001_029E","B01001_030E",
                 "B01001_031E","B01001_032E","B01001_033E",
                 "B01001_034E","B01001_035E","B01001_036E",
                 "B01001_037E","B01001_038E","B01001_039E",
                 "B01001_040E","B01001_041E","B01001_042E",
                 "B01001_043E","B01001_044E","B01001_045E",
                 "B01001_046E","B01001_047E","B01001_048E",
                 "B01001_049E",sep=",")
# Empty dataframe for combining all the data
ACSData <- data.frame()

# Actually pull all of the data from the API and combine it
for (i in year){
  print(i)
  temp <- getACSDataState("5",i,varname,state,APIkey)
  temp <- mutate(temp,Year=as.numeric(i))
  ACSData <- dplyr::bind_rows(ACSData,temp)
}

# Adding variables for Population percentage calculation
ACSData <- mutate(ACSData,
                  MalesUnder18=(B01001_003E+B01001_004E+B01001_005E+B01001_006E)/B01001_001E*100,
                  Males18to24=(B01001_007E+B01001_008E+B01001_009E+B01001_010E)/B01001_001E*100,
                  Males25to34=(B01001_011E+B01001_012E)/B01001_001E*100,
                  Males35to44=(B01001_013E+B01001_014E)/B01001_001E*100,
                  Males45to54=(B01001_015E+B01001_016E)/B01001_001E*100,
                  Males55to64=(B01001_017E+B01001_018E+B01001_019E)/B01001_001E*100,
                  MalesOver64=(B01001_020E+B01001_021E+B01001_022E+B01001_023E+B01001_024E+B01001_025E)/B01001_001E*100,
                  FemalesUnder18=(B01001_027E+B01001_028E+B01001_029E+B01001_030E)/B01001_001E*100,
                  Females18to24=(B01001_031E+B01001_032E+B01001_033E+B01001_034E)/B01001_001E*100,
                  Females25to34=(B01001_035E+B01001_036E)/B01001_001E*100,
                  Females35to44=(B01001_037E+B01001_038E)/B01001_001E*100,
                  Females45to54=(B01001_039E+B01001_040E)/B01001_001E*100,
                  Females55to64=(B01001_041E+B01001_042E+B01001_043E)/B01001_001E*100,
                  FemalesOver64=(B01001_044E+B01001_045E+B01001_046E+B01001_047E+B01001_048E+B01001_049E)/B01001_001E*100)

# Select percent variables and convert from Wide to Tall for plotting
plotData <- ACSData[52:66]
tallData <- gather(plotData,"Group","Percent",2:15)
ggplot(data=tallData, aes(x=Year, y=Percent, group=Group)) + 
       geom_line(aes(color=Group)) + geom_point(aes(color=Group)) + 
       ggtitle("TN Population based on ACS Data - Gender")

# Combine Gender percentages in ageData dataframe for plotting
ageData <- transmute(plotData, Year=Year,
                     PopUnder18=MalesUnder18+FemalesUnder18,
                     Pop18to24=Males18to24+Females18to24,
                     Pop25to34=Males25to34+Females25to34,
                     Pop35to44=Males35to44+Females35to44,
                     Pop45to54=Males45to54+Females45to54,
                     Pop55to64=Males55to64+Females55to64,
                     PopOver64=MalesOver64+FemalesOver64)
# Repeat plotting.
tallData2 <- gather(ageData,"Group","Percent",2:8)
ggplot(data=tallData2, aes(x=Year, y=Percent, group=Group)) + 
       geom_line(aes(color=Group)) + geom_point(aes(color=Group)) + 
       ggtitle("TN Population based on ACS Data")
