library("ggplot2")
library("reshape")
library("gridExtra")

# set the working directory
# setwd("C:\\Users\\xxxx\\Desktop\\blogs\\kmeans_clustering")

# function used to read the data input file and returns the dataframe
fn.readCallDetailRecord <- function(inputFile){
	# read Italia cellular network over the city of Milano
	inputDF <- read.csv(file=inputFile,sep="\t",header=F)
		
	# check the number of observations
	nrow(inputDF)
		
	# check the structure of the dataset
	str(inputDF)
		
	# rename the dataframe columns
	colnames(inputDF) <- c("square_id","time_interval","country_code","sms_in_activity","sms_out_activity","call_in_activity","call_out_activity","internet_traffic_activity")
		
	# subset the input dataframe for the 500 square_ids
	inputSubsetDF <- subset(inputDF,inputDF$square_id<=500)
	return(inputSubsetDF)
}

# function used to additional fields to the dataframe
fn.deriveAdditionalFields <- function(inputSubsetDF){
	# convert numeric column into factor column
	factorColumns <- c("square_id","country_code")
	inputSubsetDF[factorColumns] <- lapply(inputSubsetDF[factorColumns],as.factor)
	
	# derive activity start date and hour from time interval
	inputSubsetDF$activity_start_time <- fn.findStartTimeInterval(inputSubsetDF$time_interval)
	inputSubsetDF$activity_date <- as.Date(as.POSIXct(inputSubsetDF$activity_start_time,origin="1970-01-01"))
	inputSubsetDF$activity_time <- format(inputSubsetDF$activity_start_time,"%H")
	
	# derive total activity from sms in and out, call in and out and internet traffic activity 
	inputSubsetDF$total_activity <- rowSums(inputSubsetDF[, c(4,5,6,7,8)],na.rm=T)
	return(inputSubsetDF)
}

# function used to calculate the datetime from epoch unix time
fn.findStartTimeInterval <- function(inputTime){
	val <- inputTime/1000
	outputTime <- as.POSIXct(val, origin="1970-01-01",tz="UTC")
	return(outputTime)
}

# function used to plot the activity_time Vs total_activity
fn.findPeekActivityByHours <- function(inputSubsetDF){
	# Sum the total acitivity by hours
	totalActivityDF <- aggregate(total_activity ~ activity_time,inputSubsetDF,FUN=sum)
	
	totalActivityPlot <- ggplot(data=totalActivityDF, aes(x=activity_time, y=total_activity)) + geom_bar(stat="identity")+theme_bw()
	
	print(totalActivityPlot)
}

# function used to plot square_id Vs total_activity 
fn.findPeekActivityByGrid <- function(inputSubsetDF){
	# Sum the total acitivity by square grid id
	totalGridActivityDF <- aggregate(total_activity ~ square_id,inputSubsetDF,FUN=sum)
	
	# sort the data frame by total acitivity descending
	totalGridActivityDF <- totalGridActivityDF[order(-totalGridActivityDF$total_activity),]
	
	totalGridActivitySubset <- head(totalGridActivityDF,25)
	
	# top 100 grids by total acitivity
	totalGridActivityPlot <- ggplot(data=totalGridActivitySubset, aes(x=reorder(square_id,-total_activity), y=total_activity)) + geom_bar(stat="identity")+theme_bw()
	
	print(totalGridActivityPlot)
}

# function used to plot country_code Vs total_activity 
fn.findPeekActivityByCountryCode <- function(inputSubsetDF){
	# dummy count variable
	inputSubsetDF$count <- 1

	# Sum the total number of acitivity by country code
	totalActivityCountryDF <- aggregate(count ~ country_code,inputSubsetDF,FUN=sum)
	
	# sort the data frame by 
	totalActivityCountryDF <- totalActivityCountryDF[order(-totalActivityCountryDF$count),]
	
	totalActivityCountryPlot <- ggplot(data=head(totalActivityCountryDF,10), aes(x=factor(1), fill=country_code)) + geom_bar(width=1)+coord_polar("y")
	
	print(totalActivityCountryPlot)
}

fn.findOptimalCluster <- function(inputSubsetDF){
	# subset the required columns
	cdrActivityDF <- subset(inputSubsetDF,select=c("activity_time","total_activity"))
	
	# find the vector of sum of squared error(SSE) for each cluster selection
	wcss <- vector()
	for(i in 1:20) wcss[i] = sum(kmeans(cdrActivityDF,i)$withinss)
	
	# form the dataframe with cluster and its SSE
	clusterFrame <- data.frame(withinss=wcss,Cluster=seq(1:20))
	
	# plot the result and selects the optimal cluster number by looking at the graph. When number of cluster increases then the SSE will be reduced.
	ggplot(data=clusterFrame, aes(x=Cluster, y=withinss, group=1)) + geom_line(colour="red", size=1.5) + geom_point(colour="red", size=4, shape=21, fill="white")+theme_bw()
}

# function used to apply the K-means algorithm
fn.findCDRKMeans <- function(cdrActivityDF){
	# subset the required columns
	cdrActivityDF <- subset(cdrActivityDF,select=c("activity_time","total_activity"))
	
	# Applying k-means on acitivity hours and total_activity
	cdrClusterModel <- kmeans(cdrActivityDF,10,nstart=10)
	
	# print the summary of the model
	print(summary(cdrClusterModel))

	# Append the identified cluster to the input dataframe
	cdrActivityDF$cluster <- as.factor(cdrClusterModel$cluster)
	cdrActivityDF$activity_time <- as.factor(cdrActivityDF$activity_time)
	
	# melt the dataframe
	cdrActivityDF.melt <- melt(cdrActivityDF)
	
	# heat map plot for the clustering results
	cdrActivityClusterPlot <- ggplot(cdrActivityDF.melt, aes(activity_time,cluster)) + geom_tile(aes(fill = value))+ scale_fill_gradient(low = "#FBCAC5",high = "#D66889")+theme_bw()
	print(cdrActivityClusterPlot)
}

# function used to perform CDR cluster analysis
fn.cdrClusterAnalysis <- function(){
	#Data preprocessing
	cdrAcitivityInputDF <- fn.readCallDetailRecord("sms-call-internet-mi-2014-01-01.txt")
	cdrAcitivityInputDF <- fn.deriveAdditionalFields(cdrAcitivityInputDF)
	
	#EDA
	fn.findPeekActivityByHours(cdrAcitivityInputDF)
	fn.findPeekActivityByGrid(cdrAcitivityInputDF)
	fn.findPeekActivityByCountryCode(cdrAcitivityInputDF)
	
	#K-means
	fn.findCDRKMeans(cdrAcitivityInputDF)
}

fn.cdrClusterAnalysis()