# Cleaning
# PART 1
# Mobile penetration in Rwanda
#library required

library(ggplot2)
library(plotly)

# Figure 1
Penetration_rate = c(29, 40, 53, 61, 69, 76, 75)
YEAR = c(2010, 2011, 2012, 2013, 2014, 2015, 2016)
Rpenetration <- plot(x = YEAR, y = Penetration_rate, type = "o", col="blue", main = "Mobile Phone penetration in Rwanda")


# Mobile phone operators in Rwanda

Year <- c('2010', '2011', '2012', '2013', '2014', '2015', '2016')
MTN <- c(69, 65, 60, 53, 49, 47, 46)
TIGO <- c(17, 35, 33, 33, 36, 35, 35)
Airtel <- c(0, 0, 7, 14, 15, 18, 19)
Rwandatel <- c(14, 0, 0, 0, 0, 0, 0)
data <- data.frame(Year, MTN, TIGO, Airtel, Rwandatel)
p1<- plot_ly(data, x = ~Year, y = ~MTN, type = 'bar', name = 'MTN')%>%
    add_trace(y = ~TIGO, name = 'TIGO')%>% 
    add_trace(y = ~Airtel, name = "Airtel")%>%
    add_trace(y = ~Rwandatel, name = "Randatel")%>%
    layout(yaxis = list(title ='Market share'), barmode = "group", main = "Telco Operators")

p1

## PART 1 Mobile Phone Data

# Preparation for Analysis
## Reading the datasets to use for analysis
df1 <- read.csv("SampleHappy.csv") # Call detail records dataset
df <- read.csv("CellIDs (2).csv") # BTS and GPS location dataset
LatLong <- df[-1,]
dim(df); dim(df1) # checking dimensions for the datasets

# Exploratory analysis
# Transforming date and time in the right format for analysis

df1$EventStartDate <- as.character(df1$EventStartDate)
df1$StartTime <- as.character(df1$StartTime)
str(df1)

#Add zeros in front to make 00hour:00min:00seconds
add_zero = function(isaha)
{
    isaha_str <- as.character(isaha)
    if (length(grep("^[0-9]+", isaha_str)) == 1) {
        if(nchar(isaha,keepNA = TRUE) == 6){
            m = isaha
        }
        if(nchar(isaha,keepNA = TRUE) == 5){
            m = paste0("0",isaha)
        }
        if(nchar(isaha,keepNA = TRUE) == 4){
            m = paste0("00",isaha)
        }
        if(nchar(isaha,keepNA = TRUE) == 3){
            m = paste0("000",isaha)
        }
        if(nchar(isaha,keepNA = TRUE) == 2){
            m = paste0("0000",isaha)
        }
        if(nchar(isaha,keepNA = TRUE) == 1){
            m = paste0("00000",isaha)
        }
        return (m)
    }
}

df1$time <- unlist(lapply(df1$StartTime,add_zero))
df1$DateTime <- as.POSIXct(paste(df1$EventStartDate, df1$time), format="%Y%m%d %H%M%S")


# merging the two datasets to associate CDR to GPS
# Required libraries
library(dplyr)

# dropping un necessary columns 
df1_New <- select(df1, -4, -5)

# Renaming columns 
colnames(df) <- c("btsid","Lat","Long")
colnames(df1_New) <- c("Caller","DestinationNumber","btsid","EventDurationSeconds","Time","DateTime")
df_final <- merge(df,df1_New, by ="btsid", all = TRUE, sort = FALSE)


# average call in Rwanda last 36s, selecting calls that last above 36 seconds only
df_merged <- df_final[df_final$EventDurationSeconds >= 36, ]

# Select only calls in the first week of the months
df_merged1 <- df_merged[df_merged$DateTime < "2016-10-08",]
