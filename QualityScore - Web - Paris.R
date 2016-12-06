source('../LeadsLoaderAllWebsites.R')

data <- all.leads


# Subset and new features -------------------------------------------------

# Exclude Florida & subset to Websites
data <- subset(data, data$OwnerId %in% c("00GD0000003Gm8JMAS","00GD0000003Go7rMAC","00GD0000003GnllMAC") & 
                 data$LeadSource %in% c("Paris Website","Madrid Website","Heidelberg Website") &
                 as.Date(data$CreatedDate)>"2014-03-01"
                 )


# Remove values

data <- subset(data, (data$Status!='NFA' & data$Status!='No Show' & data$Status!='Alumni'
                      & data$Status!='ICT' & data$Status!='Graduate' & data$Status!='Dropped Out'))
sort(table(droplevels(data$Status)))

# WON/LOST

won <- c('Accepted','App In','REG','Paid Tuition')
data$Status2 <- as.factor(ifelse(data$Status %in% won, "WON","LOST"))
sort(table(data$Status2))

# REG Y/N
data$REG <- as.factor(ifelse(data$Status=="REG", "Y", "N"))
table(data$REG)

# Age
data$Age<-as.numeric(round(((Sys.Date() - as.Date(data$Date_of_Birth__c))/365), 0))
median(data$Age, na.rm = T)

# Month of Start Date
library(lubridate)


data$StartMonth <- as.factor(month(data$Start_Date__c, label=TRUE))
barplot(table(droplevels(data$LeadSource),data$StartMonth), 
        col=c("red","blue","green"), legend.text = T, main="Start Month", sub="by Campus")

# Day of Create Date

Sys.setlocale("LC_TIME","English")
data$DayCreate <- as.factor(weekdays(as.Date(data$CreatedDate)))

# # Month of Create Date
# 
# data$MonthCreate <- as.factor(month(data$CreatedDate,label=TRUE))

# Hour & Month of Create Date

data$date <- ymd_hms(data$CreatedDate, tz="Europe/Paris")
range(as.Date(data$CreatedDate))
data$HourCreate <- as.factor(hour(data$date))
data$MonthCreate <- as.factor(month(data$date, label=T))
barplot(table(droplevels(data$LeadSource),data$MonthCreate), 
        col=c("red","blue","green"), legend.text = T, main="Created Month", sub="by Campus")

# data$HourCreate <- data$CreatedDate
# data$HourCreate <- gsub("T"," ",data$HourCreate)
# data$HourCreate <- gsub(".000Z","",data$HourCreate)
# data$HourCreate <- as.POSIXlt(data$HourCreate)
# data$HourCreate <- hour(data$HourCreate)
# data$HourCreate <- as.factor(data$HourCreate)


# Email domain
data$Email <- as.character(data$Email)
findDomain <- function(x) unlist(strsplit(as.character(x),"@"))[2]
data$domain <- as.character(lapply(data$Email, findDomain))

                      


# Deal with NAs -----------------------------------------------------------

vars <- names(data)
df <- data.frame(0,0)
names(df) <- c("Field","CompleteCases")

myvars <- vector()

for (v in vars) {
  
  df <- rbind(df,c(v,table(is.na(data[[v]]))[1] ))
  df$ComplRate <- round((as.integer(df$CompleteCases)/nrow(data)),2)
  
  if (table(is.na(data[[v]]))[1]/nrow(data) >= .55 ) {
    
    myvars <- c(myvars, v)
    
  }
  
}

data.clean <- subset(data, select= names(data) %in% myvars)

# data.clean <- na.omit(data.clean)


# Feature Selection -------------------------------------------------------

features <- c("Program__c","LeadSource","Advisor_assigned_to__c","Campus__c","OwnerId","REG",
              "StartMonth","DayCreate","HourCreate", "MonthCreate", "Campaign_Medium__c","Campaign_Source__c"  )

data.small <- subset(data.clean, select=names(data.clean) %in% features)

# Fit Model ---------------------------------------------------------------


library(caret)

inTrain <- createDataPartition(y=data.small$REG,
                               p=0.7, list=FALSE)
training <- data.small[inTrain,]
testing <- data.small[-inTrain,]
dim(training); dim(testing)

fit <- train(REG~.,data=training, method="rf")



# Prediction --------------------------------------------------------------
res <- predict(fit,testing, type = "prob")
res <- predict(fit,testing)



# sentiment analysis (and knn?) on Description

