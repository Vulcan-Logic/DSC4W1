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
#This script addresses Part 1 of the asignment: 
#Have total emissions from PM2.5 decreased in the United States from 
#1999 to 2008? 
#Using the base plotting system, make a plot
#showing the total PM2.5 emission from all sources for 
#each of the years 1999, 2002, 2005, and 2008.

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

#apply the mean function to all emissions grouped by the years and read it into 
#a vector and use it for plotting
a1s<-tapply(NEI$Emissions,NEI$year,sum)

#ans 1
#open the graphics device i.e. png file
png("plot1.png",res=150,width=20,height=20,units="cm")

#make the plot of points of total emissions in the US 
plot(x=x,y=a1s,type="p",pch=1,ylab="Total PM2.5 Emission",
     main="Total PM2.5 Emissions in the USA in 1999, 2002, 2005 & 2008",
     xaxt='n', xlab="Year")

#add the lines connecting the points to make the plot a bit more informative
lines(x=x,y=a1s,type="l",lty=1,lwd=2)

#add years info/title on the x axis
axis(1,at=c(1999,2002,2005,2008),labels = c("1999","2002","2005","2008")) 

#save the file. 
dev.off()
