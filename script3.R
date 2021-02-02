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
#This script addresses Part 3 of the asignment: 
#Of the four types of sources indicated by the type 
#(point, nonpoint, onroad, nonroad) variable, 
#which of these four sources have seen decreases in emissions 
#from 1999–2008 for Baltimore City? 
#Which have seen increases in emissions from 1999–2008? 
#Use the ggplot2 plotting system to make a plot answer this question.

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

#ans 3 use ggplot2 to plot this answer

baltimore$year<-as.factor(baltimore$year)

#make a quickplot with the emissions points from the baltimore dataframe
#by groups and 4 plots facets based on emission source type
q1<-qplot(x=year,y=Emissions,data=baltimore,group=year,facets=.~type,
          colour=I("lightblue"),xlab="Year",ylab="PM2.5 Emission",
          main= paste("PM2.5 Emissions in Baltimore", 
                    "in 1999, 2002, 2005, 2008", "segregated by source type"))

#add breaks on the x-axis for the year info with the correct labels
q1<-q1 + scale_x_discrete(breaks=c("1999","2002","2005","2008"),
                          labels=c("1999","2002","2005","2008"),
                          limits=c("1999","2002","2005","2008"))

#add information about mean emission values to the 4 plots in the form of 
#points
q1<-q1+stat_summary(fun.y=mean, colour="red", aes(shape="Mean Emi."), 
                    geom="point")

#add information about the trend by connecting the points with a line
q1<-q1+stat_summary(fun.y="mean", geom="line", aes(group=1), colour="red")

#add information about total emission values to the 4 plots in the form of 
#points
q1<-q1+stat_summary(fun.y=sum, colour="blue",aes(shape="Total Emi."), 
                    geom="point")

#add information about the trend by connecting the points with a line
q1<-q1+stat_summary(fun.y=sum, geom="line", aes(group=1), colour="blue") 

#add a legend on what the shape means
q1<-q1 + scale_shape_manual(values=c("Mean Emi." = 2,"Total Emi."= 1)) 

#save the plot
ggsave("plot3.png",plot=q1,width=25,height=20,units="cm")

#remove the data frames from memory for a clean exit
rm(NEI)
rm(SCC)

