#libraries
library(geosphere)

## get DOY ##
#convert dates to day of year
#import data
rawdata<-read.csv('../hackathon.csv',header=F)

colnames(rawdata)<-c("search_year","search_month", "search_day", "search_platform", "dest_city","dest_country","dest_continent","dest_long", "dest_lat", "date_to", "date_from", "group_room", "orig_city","orig_country", "orig_continent", "orig_long", "orig_lat")
numdata<-apply(rawdata[,c(1,2,3,8,9,16,17)],c(1,2),as.numeric)

#calculate length of trips

distances<-apply(numdata,1,function(x) distVincentyEllipsoid(p1=c(x['dest_long'],x['dest_lat']),p2=c(x['orig_long'],x['orig_lat'])))
procdata<-cbind(rawdata,distances)
colnames(procdata)[length(procdata[1,])]<-'distance'

write.table(distances,file='distances_trihack.csv',row.names=F,col.names=F)
save.image()
