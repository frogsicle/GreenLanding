#libraries
library(geosphere)
#functions

#import (dummy) data
rawdata<-read.csv('hackathon50000.csv',header=F)
#read.csv('../hackathon.csv',header=F)
colnames(rawdata)<-c("search_year","search_month", "search_day", "search_platform", "dest_city","dest_country","dest_continent","dest_long", "dest_lat", "date_to", "date_from", "group_room", "orig_city","orig_country", "orig_continent", "orig_long", "orig_lat")
numdata<-apply(rawdata[,c(1,2,3,8,9,16,17)],c(1,2),as.numeric)

#calculate length of trips
#set up clusters for parallel calculation
cl<-makeCluster(8)
library(geosphere)#I was trying to figure out the shared memory for a bit
clusterExport(cl, list("distVincentyEllipsoid"))#this is how you really get it into the 'cluster memory'
#parallel apply
distances<-parRapply(cl,numdata,function(x) distVincentyEllipsoid(p1=c(x['dest_long'],x['dest_lat']),p2=c(x['orig_long'],x['orig_lat'])))
stopCluster(cl)#close it
procdata<-cbind(rawdata,distances)#save it
colnames(procdata)[length(procdata[1,])]<-'distance'
#write.csv(distances,file='distances_trihack.csv',quote=F)
write.table(distances,file='distancesPar50000.csv',row.names=F,col.names=F)

#well... never got past the dummy data, but at least I figured out how to do it =)
