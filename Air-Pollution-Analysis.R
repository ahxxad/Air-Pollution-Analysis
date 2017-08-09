# Air-Pollution-Analysis By Ahmed Abdulsalam Ali 


getwd()
setwd("C:/Users/eng_a/Google Drive/ALLOFME/MMU/Feb 16/TPA7021/Assignment")

#Installing required packages
install.packages("plyr")
install.packages("ggplot2")
install.packages("scales")
install.packages("plotly")

## Reading API.csv file.
API <- read.csv("API.csv")

############################################## DATA CLEANING ##################################################

## Date column manipulation set as a Date format
class(API$Date)
API$Date <- as.character(API$Date)
API$Date <- as.Date(API$Date, format = "%d/%m/%Y") #Set Date column as a date
class(API$Date)
summary(is.na(API$Date))

## Time column manipulation
class(API$Time)
API$Time <- as.character(API$Time, format="%I:%M %p")
API$Time <- strptime(API$Time, format="%I:%M %p") #Set Time column as a date (POSIXlt" "POSIXt) format
API$Time <- format(API$Time,"%I:%M %p")
summary(is.na(API$Time))

## State column manipulation
class(API$State)
summary(API$State)
summary(is.na(API$State))
API$State <- toupper(API$State) # Convert states to upper case

## Area column manipulation and removing noisy data such as :- "comma,_ and spaces" 
class(API$Area)
summary(API$Area)
summary(is.na(API$Area))
API$Area <- as.character(API$Area)
summary(grepl(",",API$Area))  
API$Area <- gsub(","," _ ",API$Area)
summary(grepl(", ",API$Area))
summary(grepl(" _",API$Area))  
API$Area <- gsub(" _"," _ ",API$Area)
summary(grepl(" _ ",API$Area))  
summary(grepl("_",API$Area))
summary(grepl("_ ",API$Area))
API$Area <- gsub("_ "," _ ",API$Area)
summary(grepl("_ ",API$Area))
summary(grepl(" _ ",API$Area))
summary(grepl("S K",API$Area))
API$Area <- gsub("S K","SK",API$Area)
summary(grepl("S K",API$Area))  
summary(grepl("Batu Muda_Kuala Lumpur",API$Area))  
API$Area <- gsub("Batu Muda_Kuala Lumpur","Batu Muda _ Kuala Lumpur",API$Area)
summary(grepl("Cheras_Kuala Lumpur",API$Area))  
API$Area <- gsub("Cheras_Kuala Lumpur","Cheras _ Kuala Lumpur",API$Area)
summary(API$Area)

## API column manipulation
summary(grepl("\\D",API$API))
API$API <- gsub("\\D","",API$API) # Removing (puncts) from API column
API$API <- as.integer(API$API) # Set API column as an integer
summary(is.na(API$API))
#API[!complete.cases(API$API),]
mean(API$API, na.rm = TRUE) # Check mean without NA values in API column
summary(is.na(API$API)) #Check summary of 'NA' values in API column

#Imputing "API$API" missing values for each state separately  using "mean" function
library(plyr)
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
API <- ddply(API, ~ State, transform, API = impute.mean(API))
API$API <- as.integer(API$API)

# Creating Status column based on API Values as giving in (TPA7021_Assignment.pdf) file :-
API$Status[API$API<=50]<- "Good"
API$Status[API$API>=51 & API$API<=100]<- "Moderate"
API$Status[API$API>=101 & API$API<=200]<- "Unhealthy"
API$Status[API$API>=201 & API$API<=300]<- "Very Unhealthy"
API$Status[API$API>=301 & API$API<=500]<- "Hazardous"
API$Status[API$API>500]<- "Emergency"
class(API$Status)
# Check summary of API status
summary(grepl("Good",API$Status))
summary(grepl("Moderate",API$Status))
summary(grepl("Unhealthy",API$Status))
summary(grepl("Very Unhealthy",API$Status))
summary(grepl("Hazardous",API$Status))
summary(grepl("Emergency",API$Status))
############################################ End of Data Cleaning ############################################

############################################# Data Analysis #################################################
library(ggplot2)
ggplot(API, aes(x = Status, y = API,fill=Status)) + geom_bar(stat = "identity")

#MAXIMUM API value by State
Max.State <- ddply(API,~State,function(x){x[which.max(x$API),]})
Max.State
ggplot(Max.State, aes(x = State, y = API,fill=State)) + geom_bar(stat = "identity") +
  scale_y_continuous(limits=c(0, 750), breaks=c(0, 50, 100, 200,300,400,500,600,700,750))+
  labs(title="Maximum API value for each state")

library(plotly)
f <- list( family = "Courier New, monospace",size = 18,color = "#7f7f7f") 
x1 <- list(title = "States",titlefont = f)
y1 <- list(title = "API",titlefont = f)

MAX <- plot_ly(x = Max.State$State, y = Max.State$API,
                       text = paste("API for this state")) %>%
  layout(xaxis = x1, yaxis = y1, title = "Max API - Malaysia")

MAX


#MAXIMUM API value by Area
Max.Area <- ddply(API,~Area,function(x){x[which.max(x$API),]})
ggplot(Max.Area, aes(x = Area, y = API,fill=Area)) + geom_bar(stat = "identity")+
  scale_y_continuous(limits=c(0, 750), breaks=c(0, 50, 100, 200,300,400,500,600,700,750))+
  labs(title="Maximum API value for each area")

# MAX API value by Date
Max.Date <- ddply(API,~Date,function(x){x[which.max(x$API),]})
ggplot(Max.Date, aes(x = Date, y = API,fill=Date)) + geom_bar(stat = "identity") +
  scale_y_continuous(limits=c(0, 750), breaks=c(0, 50, 100, 200,300,400,500,600,700,750))+
  labs(title="Maximum API value by Date")

#MAX API value by TIME
Max.Time <- ddply(API,~Time,function(x){x[which.max(x$API),]})
ggplot(Max.Area, aes(x = Time, y = API,fill=Time)) + geom_bar(stat = "identity")+
  scale_y_continuous(limits=c(0, 750), breaks=c(0, 50, 100, 200,300,400,500,600,700,750))+
  labs(title="Maximum API value per time")


#JOHOR
JOHOR <- API[grep("JOHOR",API$State),]
#Subset based on Area
J.Kota_Tinggi <- JOHOR[grep("Kota Tinggi",JOHOR$Area),]
J.Larkin_Lama <- JOHOR[grep("Larkin Lama",JOHOR$Area),]
J.Muar <- JOHOR[grep("Muar",JOHOR$Area),]
J.Pasir_Gudang <- JOHOR[grep("Pasir Gudang",JOHOR$Area),]
#Subset based on Dates randomly
J.M.OCT.2005 <- subset(J.Muar, Date<"2005-11-01")
J.K.NOV.2005 <- subset(J.Kota_Tinggi, Date < "2005-12-01" & Date > "2005-10-31")
J.L.DEC.2005 <- subset(J.Larkin_Lama, Date<"2006-01-01" & Date>"2005-11-30")
J.P.JAN.2006 <- subset(J.Pasir_Gudang, Date>"2005-12-31" & Date<"2006-02-01")
J.ME.JUN.2013 <- subset(J.Muar, Date>"2013-05-31" & Date<"2013-07-1")


#Areas of JOHOR
f <- list( family = "Courier New, monospace",size = 18,color = "#7f7f7f") 
x <- list(title = "Date",titlefont = f)
y <- list(title = "API",titlefont = f)

Kota_Tinggi <- plot_ly(x = J.Kota_Tinggi$Date, y = J.Kota_Tinggi$API,
     text = paste("API for this date")) %>%
  layout(xaxis = x, yaxis = y, title = "API for Kota Tinggi-JOHOR")

Kota_Tinggi

Larkin_Lama <- plot_ly(x = J.Larkin_Lama$Date, y = J.Larkin_Lama$API,
             text = paste("API for this date")) %>%
  layout(xaxis = x, yaxis = y, title = "API for Larkin Lama-JOHOR")

Larkin_Lama

Muar <- plot_ly(x = J.Muar$Date, y = J.Muar$API,
             text = paste("API for this date")) %>%
  layout(xaxis = x, yaxis = y, title = "API for Muar-JOHOR")

Muar

Pasir_Gudang <- plot_ly(x = J.Pasir_Gudang$Date, y = J.Pasir_Gudang$API,
             text = paste("API for this date")) %>%
  layout(xaxis = x, yaxis = y, title = "API for Kota Tinggi-JOHOR")

Pasir_Gudang


JO.OCT.2005 <- plot_ly(x = J.M.OCT.2005$Date, y = J.M.OCT.2005$API,
                text = paste("API for this day")) %>%
  layout(xaxis = x, yaxis = y, title = "API for Muar - JOHOR - OCTOBER 2005")

JO.OCT.2005

JO.NOV.2005 <- plot_ly(x = J.K.NOV.2005$Date, y = J.K.NOV.2005$API,
                       text = paste("API for this date")) %>%
  layout(xaxis = x, yaxis = y, title = "API for Kota_Tinggi - JOHOR - NOVEMBER 2005")

JO.NOV.2005

JO.DEC.2005 <- plot_ly(x = J.L.DEC.2005$Date, y = J.L.DEC.2005$API,
                       text = paste("API for this date")) %>%
  layout(xaxis = x, yaxis = y, title = "API for J.Larkin_Lama - JOHOR - December 2005")

JO.DEC.2005

JO.JAN.2006 <- plot_ly(x = J.P.JAN.2006$Date, y = J.P.JAN.2006$API,
                       text = paste("API for this date")) %>%
  layout(xaxis = x, yaxis = y, title = "API for Pasir Gudang - JOHOR - January 2006")

JO.JAN.2006 

J.MEM.JUN.2013 <- plot_ly(x = J.ME.JUN.2013$Date, y = J.ME.JUN.2013$API,
                          text = paste("API for this date")) %>%
  layout(xaxis = x, yaxis = y, title = "API for Muar - JOHOR - June 2013")

J.MEM.JUN.2013


## GGPLOT2 
class(JOHOR$Date)
JOHOR$Date <- as.character(JOHOR$Date, format("%d/%m/%Y"))
JOHOR$Date <- strptime(JOHOR$Date, format = "%d/%m/%Y")
J.OCT.2005=subset(JOHOR, Date<"2005-11-01")
J.NOV.2005=subset(JOHOR, Date < "2005-12-01" & Date > "2005-10-31")
J.DEC.2005=subset(JOHOR, Date<"2006-01-01" & Date>"2005-11-30")

library(scales)

ggplot(data=JOHOR,aes(x=Date,y=API)) + geom_line(aes(color=Area),size=1) +
  scale_x_datetime(breaks = date_breaks("1 year")) +
  scale_y_continuous(limits=c(0, 650), breaks=c(0, 50, 100, 200,300,500))

j <- ggplot(data=J.OCT.2005,aes(x=Date,y=API)) + geom_line(aes(color=Area),size=1) +
  scale_x_datetime(breaks = date_breaks("1 day")) + 
  scale_y_continuous(limits=c(0, 500), breaks=c(0, 50, 100, 200,300,500,700)) + xlab("Date") +
  ylab("API") + labs(title="State: JOHOR - October")
j

#S0ELANGOR
SELANGOR <- API[grep("SELANGOR",API$State),]
#Subset based on Area
S.Banting <- SELANGOR[grep("Banting",SELANGOR$Area),]
Kuala_Selangor <- SELANGOR[grep("Kuala Selangor",SELANGOR$Area),]
Pelabuhan_Kelang <- SELANGOR[grep("Pelabuhan Kelang",SELANGOR$Area),]
Petaling_Jaya <- SELANGOR[grep("Petaling Jaya",SELANGOR$Area),]
Shah_Alam <- SELANGOR[grep("Shah Alam",SELANGOR$Area),]
#Subset based on Dates randomly
S.Banting.Apr.2012 <- subset(S.Banting, Date<"2012-05-01" & Date>"2012-03-31")
Kuala_Selangor.NOV.2011 <- subset(Kuala_Selangor, Date<"2011-12-01" & Date>"2011-10-31")
Pelabuhan_Kelang.DEC.2012 <- subset(Pelabuhan_Kelang, Date<"2013-01-01" & Date>"2012-11-30")
Petaling_Jaya.MAY.2009 <- subset(Petaling_Jaya, Date>"2009-04-30" & Date<"2009-06-01")
Shah_Alam.JAN.2013 <- subset(Shah_Alam, Date>"2012-12-31" & Date<"2013-02-01")
Pelabuhan_Kelang.JUN.2013 <- subset(Pelabuhan_Kelang, Date<"2013-07-01" & Date>"2013-05-31")


#Areas of SELANGOR
f <- list( family = "Courier New, monospace",size = 18,color = "#7f7f7f") 
x <- list(title = "Date",titlefont = f)
y <- list(title = "API",titlefont = f)

Banting <- plot_ly(x = S.Banting.Apr.2012$Date, y = S.Banting.Apr.2012$API,
                       text = paste("API for this date")) %>%
  layout(xaxis = x, yaxis = y, title = "API for Banting - SELANGOR - April 2012")

Banting

KSelangor <- plot_ly(x = Kuala_Selangor.NOV.2011$Date, y = Kuala_Selangor.NOV.2011$API,
                       text = paste("API for this date")) %>%
  layout(xaxis = x, yaxis = y, title = "API for Kuala Selangor - SELANGOR - November 2011")

KSelangor

PKelang <- plot_ly(x = Pelabuhan_Kelang.DEC.2012$Date, y = Pelabuhan_Kelang.DEC.2012$API,
                text = paste("API for this date")) %>%
  layout(xaxis = x, yaxis = y, title = "API for Pelabuhan_Kelang - SELANGOR - December 2012")

PKelang

P.Jaya <- plot_ly(x = Petaling_Jaya.MAY.2009$Date, y = Petaling_Jaya.MAY.2009$API,
                        text = paste("API for this date")) %>%
  layout(xaxis = x, yaxis = y, title = "API for Petaling_Jaya - SELANGOR May 2009")

P.Jaya


SAlam <- plot_ly(x = Shah_Alam.JAN.2013$Date, y = Shah_Alam.JAN.2013$API,
                       text = paste("API for this day")) %>%
  layout(xaxis = x, yaxis = y, title = "API for Shah Alam - SELANGOR - January 2013")

SAlam

PKelang.JUN <- plot_ly(x = Pelabuhan_Kelang.JUN.2013$Date, y = Pelabuhan_Kelang.JUN.2013$API,
                            text = paste("API for this date")) %>%
  layout(xaxis = x, yaxis = y, title = "API for Pelabuhan_Kelang - SELANGOR - June 2013")

PKelang.JUN

B <- plot_ly(x = S.Banting$Date, y = S.Banting$API,
                       text = paste("API for this date")) %>%
  layout(xaxis = x, yaxis = y, title = "API for Banting - SELANGOR")

B

KS <- plot_ly(x = Kuala_Selangor$Date, y = Kuala_Selangor$API,
                 text = paste("API for this date")) %>%
  layout(xaxis = x, yaxis = y, title = "API for Kuala Selangor - SELANGOR")
KS

PK <- plot_ly(x = Pelabuhan_Kelang$Date, y = Pelabuhan_Kelang$API,
              text = paste("API for this date")) %>%
  layout(xaxis = x, yaxis = y, title = "API for Pelabuhan Kelang - SELANGOR")

PK

PJ <- plot_ly(x = Petaling_Jaya$Date, y = Petaling_Jaya$API,
                 text = paste("API for this date")) %>%
  layout(xaxis = x, yaxis = y, title = "API for Petaling Jaya - SELANGOR")

PJ

SA <- plot_ly(x = Shah_Alam$Date, y = Shah_Alam$API,
              text = paste("API for this date")) %>%
  layout(xaxis = x, yaxis = y, title = "API for Shah Alam - SELANGOR")

SA

## GGPLOT2
class(SELANGOR$Date)
SELANGOR$Date <- as.character(SELANGOR$Date, format("%d/%m/%Y"))
SELANGOR$Date <- strptime(SELANGOR$Date, format = "%d/%m/%Y")
S.Apr.2010 <- subset(SELANGOR, Date>"2010-03-31" & Date<"2010-05-01")
S.NOV.2011 <- subset(SELANGOR, Date<"2011-12-01" & Date>"2011-10-31")
S.DEC.2012 <- subset(SELANGOR, Date<"2013-01-01" & Date>"2012-11-30")
S.MAY.2009 <- subset(SELANGOR, Date>"2009-04-30" & Date<"2009-06-01")
S.JAN.2013 <- subset(SELANGOR, Date>"2012-12-31" & Date<"2013-02-01")
S.JUN.2013 <- subset(SELANGOR, Date<"2013-07-01" & Date>"2013-05-31")


ggplot(data=SELANGOR,aes(x=Date,y=API)) + geom_line(aes(color=Area),size=1) +
  scale_x_datetime(breaks = date_breaks("1 year")) +
  scale_y_continuous(limits=c(0, 650), breaks=c(0, 50, 100, 200,300,500))

s <- ggplot(data=S.Apr.2010,aes(x=Date,y=API)) + geom_line(aes(color=Area),size=1) +
  scale_x_datetime(breaks = date_breaks("1 day")) + 
  scale_y_continuous(limits=c(0, 500), breaks=c(0, 50, 100, 200,300,500,700)) + xlab("Date") +
  ylab("API") + labs(title="State: SELANGOR - April 2010")
s

s1 <- ggplot(data=S.NOV.2011,aes(x=Date,y=API)) + geom_line(aes(color=Area),size=1) +
  scale_x_datetime(breaks = date_breaks("1 day")) + 
  scale_y_continuous(limits=c(0, 500), breaks=c(0, 50, 100, 200,300,500,700)) + xlab("Date") +
  ylab("API") + labs(title="State: SELANGOR - November 2011")
s1

s2 <- ggplot(data=S.DEC.2012,aes(x=Date,y=API)) + geom_line(aes(color=Area),size=1) +
  scale_x_datetime(breaks = date_breaks("1 day")) + 
  scale_y_continuous(limits=c(0, 500), breaks=c(0, 50, 100, 200,300,500,700)) + xlab("Date") +
  ylab("API") + labs(title="State: SELANGOR - December 2012")
s2

s3 <- ggplot(data=S.MAY.2009,aes(x=Date,y=API)) + geom_line(aes(color=Area),size=1) +
  scale_x_datetime(breaks = date_breaks("1 day")) + 
  scale_y_continuous(limits=c(0, 500), breaks=c(0, 50, 100, 200,300,500,700)) + xlab("Date") +
  ylab("API") + labs(title="State: SELANGOR - May 2009")
s3

s4 <- ggplot(data=S.JAN.2013,aes(x=Date,y=API)) + geom_line(aes(color=Area),size=1) +
  scale_x_datetime(breaks = date_breaks("1 day")) + 
  scale_y_continuous(limits=c(0, 500), breaks=c(0, 50, 100, 200,300,500,700)) + xlab("Date") +
  ylab("API") + labs(title="State: SELANGOR - January 2013")
s4

s5 <- ggplot(data=S.JUN.2013,aes(x=Date,y=API)) + geom_line(aes(color=Area),size=1) +
  scale_x_datetime(breaks = date_breaks("1 day")) + 
  scale_y_continuous(limits=c(0, 500), breaks=c(0, 50, 100, 200,300,500,700)) + xlab("Date") +
  ylab("API") + labs(title="State: SELANGOR - June 2013")
s5

#MELAKA
MELAKA <- API[grep("MELAKA",API$State),]
#Subset based on Area
Bukit_Rambai <- MELAKA[grep("Bukit Rambai",MELAKA$Area),]
#Subset based on Date - June
BR.JUN.2013 <- subset(MELAKA, Date<"2013-07-01" & Date>"2013-05-31")

BR <- plot_ly(x = BR.JUN.2013$Date, y = BR.JUN.2013$API,
                   text = paste("API for this date")) %>%
  layout(xaxis = x, yaxis = y, title = "API for Bukit Rambai - MELAKA - June 2013")

BR

BU <- plot_ly(x = Bukit_Rambai$Date, y = Bukit_Rambai$API,
              text = paste("API for this date")) %>%
  layout(xaxis = x, yaxis = y, title = "API for Bukit Rambai - MELAKA ")

BU

#NEGERI SEMBILAN
NEGERI_SEMBILAN <- API[grep("NEGERI SEMBILAN",API$State),]
#Subset based on Area
Port_Dickson <- NEGERI_SEMBILAN[grep("Port Dickson",NEGERI_SEMBILAN$Area),]
#Subset based on Date - June
PD.JUN.2013 <- subset(NEGERI_SEMBILAN, Date<"2013-07-01" & Date>"2013-05-31")

PD.JUN <- plot_ly(x = PD.JUN.2013$Date, y = PD.JUN.2013$API,
              text = paste("API for this date")) %>%
  layout(xaxis = x, yaxis = y, title = "API for Port Dickson - NEGERI SEMBILAN - June 2013")

PD.JUN

PD <- plot_ly(x = Port_Dickson$Date, y = Port_Dickson$API,
              text = paste("API for this date")) %>%
  layout(xaxis = x, yaxis = y, title = "API for Port_Dickson - NEGERI SEMBILAN ")

PD
###################### End of Data analysis for selected sample from giving data set #########################
