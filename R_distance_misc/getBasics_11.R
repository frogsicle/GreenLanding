#libraries
library(geosphere)
#functions
## get DOY ##
#convert dates to day of year
getDOY<-function(x){
	y<-as.character(x)
#	print(y)
	y<-strsplit(y,split='')
#	print(y)
	year<-as.numeric(paste(unlist(y)[1:4],collapse=''))
#	print(year)
	month<-as.numeric(paste(unlist(y)[5:6],collapse=''))
	day<-as.numeric(paste(unlist(y)[7:8],collapse=''))
	monthlengths<-c(0,31,28,31,30,31,30,31,31,30,31,30)
	if(year==2012){
		monthlengths[3]<-29
	}
	doy<-sum(monthlengths[1:(month)],day)
	return(doy)
}
#summarize data
## placemeans ##
placemeans<-function(x,col){
	allunique<-unique(x[,col],)
	out<-c()
	for (i in allunique){
	        #continent, mean distance from, mean trip duration, mean timing
	        my_indexes<-which(x[,col]==i)
	        y<-c(i,apply(x[my_indexes,c('distance','duration','timing')],2,mean))
	        out<-rbind(out,y)
	}
	formatout<-data.frame(out)
	rownames(formatout)<-formatout[,1]
	formatout<-apply(formatout[,-1],c(1,2),as.numeric)
	return(formatout)
}

#import (dummy) data
rawdata<-read.csv('../hackathon_dummy.csv',header=F)
#rawdata<-read.csv('../hackathon.csv',header=F)
colnames(rawdata)<-c("search_year","search_month", "search_day", "search_platform", "dest_city","dest_country","dest_continent","dest_long", "dest_lat", "date_to", "date_from", "group_room", "orig_city","orig_country", "orig_continent", "orig_long", "orig_lat")
numdata<-apply(rawdata[,c(1,2,3,8,9,16,17)],c(1,2),as.numeric)

#calculate length of trips
procdata<-cbind(rawdata,apply(numdata,1,function(x) distVincentyEllipsoid(p1=c(x['dest_long'],x['dest_lat']),p2=c(x['orig_long'],x['orig_lat']))))
colnames(procdata)[length(procdata[1,])]<-'distance'

#calculate duration of trips
duration<-apply(rawdata,1,function(x) getDOY(x['date_from'])-getDOY(x['date_to']))
procdata<-cbind(procdata,duration)
colnames(procdata)[length(procdata[1,])]<-'duration'

#timing in year
timing<-apply(rawdata,1,function(x) mean(getDOY(x['date_from']),getDOY(x['date_to'])))
procdata<-cbind(procdata,timing)
colnames(procdata)[length(procdata[1,])]<-'timing'

###### put together summary stats by each type of place ########
#description source places
#allori_continent<-unique(rawdata[,'orig_continent'],)
#allori_country<-unique(rawdata[,"orig_country"],)
#x<-c()
#for (i in allori_continent){
#	#continent, mean distance from, mean trip duration, mean timing
#	my_indexes<-which(procdata[,'orig_continent']==i)
#	y<-c(i,apply(procdata[my_indexes,c('distance','duration','timing')],2,mean))
#	x<-rbind(x,y)
#}
byoricontinent<-placemeans(procdata,'orig_continent')
byoricountry<-placemeans(procdata,'orig_country')

#description of destination places
#alldest_continent<-unique(rawdata[,'dest_continent'],)
#alldest_country<-unique(rawdata[,"dest_country"],)

bydestcontinent<-placemeans(procdata,'dest_continent')
bydestcountry<-placemeans(procdata,'dest_country')

#### make a few histograms and such for basic data set description.
#and here this aim ended ...


