#written by Vineet W. Singh - 04-12-2017
#submissions for various parts of assignment of Week 4 - Part 4 of the 
#Exploratory Data Analysis module of the data science course of coursera

#this script checks to see if the data files are present in the current 
#directory, if the files are present, it will open it and load the data 
#into  data frames. 
#If the file is not present it will try to download the main zip file 
#from the url provided and will try to unzip the data files into the current
#direcory and load the data into the required data frames
#The data will be processed by subseting into appropriate sub frames where 
#required and will extract the necessary data and process it as required.
#The script will then produce the required plot and save it as a png file. 
#
#This script addresses Part 6 of the asignment: 
#Compare emissions from motor vehicle sources in Baltimore City 
#with emissions from motor vehicle sources in Los Angeles County, 
#California (fips == "06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?


#check if package curl is installed
if(is.element("curl", installed.packages()[,1])){ #check if curl is installed
  require("curl") #load curl if it is installed
} else{              #curl is not installed - stop
  stop("missing package: curl, please install it first")
}

#check if package ggplot2 is installed
if(is.element("ggplot2", installed.packages()[,1])){ 
  #check if ggplot2 is installed
  require("ggplot2") #load ggplot2 if it is installed
} else{              #ggplot2 is not installed - stop
  stop("missing package: ggplot2, please install it first")
}

#check if package ggplot2 is installed
if(is.element("sqldf", installed.packages()[,1])){ #check if sqldf is installed
  require("sqldf") #load sqldf if it is installed
} else{              #sqldf is not installed - stop
  stop("missing package: sqldf, please install it first")
}


#check to see if input data exists or download it and then read it
if ((file.exists("summarySCC_PM25.rds") 
     & file.exists("Source_Classification_Code.rds"))){
  message("loading emissions (NEI) data")
  NEI <- readRDS("summarySCC_PM25.rds")
  message("loading Source classification code (SCC) data")
  SCC <- readRDS("Source_Classification_Code.rds")  
  
} else {
  url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
  download.file(url,destfile="./datazip.zip",method="curl")
  unzip('./datazip.zip',exdir='./')
  message("loading emissions (NEI) data")
  NEI <- readRDS("summarySCC_PM25.rds")
  message("loading Source classification code (SCC) data")
  SCC <- readRDS("Source_Classification_Code.rds")  
}

message("generating the plot")
# get the number of years out as a factor so that they can be used to plot on 
# the x axis
x<-levels(as.factor(NEI$year))

#try out the sqldf package. make a sql query to extract all rows where 
#the county is baltimore
query<-"select * from NEI where fips = '24510'"

#dispatch the query and get the results
baltimore<-sqldf(query)

baltimore$year<-as.factor(baltimore$year)

#for questions 5 and 6
#from the scc data frame get a subset of rows of interest so that we can 
#extract the SCC codes to further subset the emissions dataset
scc2<-subset(SCC,EI.Sector %in% 
               c("Mobile - On-Road Diesel Heavy Duty Vehicles",
                 "Mobile - On-Road Diesel Light Duty Vehicles",
                 "Mobile - On-Road Gasoline Heavy Duty Vehicles",
                 "Mobile - On-Road Gasoline Light Duty Vehicles"))

#use the list of extracted SCC codes to generate subseted data frame of interest 
#from the baltimore emissions data frame
baltimore_r1<-subset(baltimore,baltimore$SCC %in% scc2$SCC)

#apply the sum and the mean function to all emissions grouped by the years 
#and read it into vectors to be used for plotting
a5s<-tapply(baltimore_r1$Emissions,baltimore_r1$year,sum)
a5m<-tapply(baltimore_r1$Emissions,baltimore_r1$year,mean,na.rm=TRUE)

#get all rows of interest pertaining to los angeles county from the 
#emissions data frame. 
la<-subset(NEI,NEI$fips=="06037")

#use the list of extracted SCC codes to generate subseted data frame of interest 
#from the los angeles emissions data frame
la_r1<-subset(la,la$SCC %in% scc2$SCC)

#apply the sum and the mean function to all emissions grouped by the years 
#and read it into vectors to be used for plotting
a6s<-tapply(la_r1$Emissions,la_r1$year,sum)
a6m<-tapply(la_r1$Emissions,la_r1$year,mean,na.rm=TRUE)

#ans6
#open the device i.e. png file. 
png("plot6.png",res=150,width=25,height=20,units="cm")

#make 1 row and 2 column of plots to give a better plot
par(mfrow=c(1,2),oma=c(3,0,0,0),family="mono",font=1)

#make the plot of points of total emissions in baltimore
#and los angeles  from vehicle sources
plot(x=x,y=a5s,type="p",pch=1,col="blue", ylab="Total PM2.5 Emission", 
     cex.main=1, main="Total missions \nin Baltimore & Los Angeles ",
     xaxt='n', xlab="Year",ylim=c(50,5500))

#ADD data points los angeles to the plot
points(x=x,y=a6s,pch=1,col="red")

#add the lines connecting the points to make the plot a bit more informative
lines(x=x,y=a5s,type="l",lty=1,lwd=2,col="blue")
lines(x=x,y=a6s,lty=3,lwd=2,col="red")

#add years info/title on the x axis
axis(1,at=c(1999,2002,2005,2008),labels = c("1999","2002","2005","2008"))

#add a legend
legend("topright", c("Baltimore","Los Angeles"),lty=c(1,3),lwd=c(2,2),
       col=c("blue","red"),bty="n",cex=0.8,xjust=1,x.intersp = 0.8)

#make the plot of points of mean emissions in baltimore
#and los angeles  from vehicle sources
plot(x=x,y=a5m,type="p",pch=2,col="blue", ylab="Mean PM2.5 Emission", 
     cex.main=1, main="Mean emissions \nin Baltimore & Los Angeles",
     xaxt='n', xlab="Year",ylim=c(0,150))

#add points to the plot for los angeles data
points(x=x,y=a6m,pch=2,col="red")

#add the lines connecting the points to make the plot a bit more informative
lines(x=x,y=a6m,lty=4,lwd=2,col="red")
lines(x=x,y=a5m,type="l",lty=2,lwd=2,col="blue")

#add the lines connecting the points to make the plot a bit more informative
axis(1,at=c(1999,2002,2005,2008),labels = c("1999","2002","2005","2008"))

#add a legend
legend("topright", c("Baltimore","Los Angeles"),lty=c(2,4),lwd=c(2,2),
       col=c("blue","red"),bty="n",cex=0.8,xjust=1,x.intersp = 0.8)

#set the main title
mtext(text=paste("PM2.5 emissions generated by motor vehicle sources" 
      ,"\n in Baltimore & Los Angeles in 1999, 2002, 2005, 2008"),
      side=1,line=1,outer=TRUE,cex=1.2,font=2)

#save the plot 
dev.off()

#remove the data frames from memory for a clean exit
rm(NEI)
rm(SCC)


