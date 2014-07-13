## graphs for presentation, similarity matrix between whole year weather (temp or precip) at various locations. 
load('temp.Robj')
load('prec.Robj')

#temp
mydata<-apply(temp,c(1,2),as.numeric)
mydist<-dist(mydata)
heatmap(as.matrix(mydist),labRow='',labCol='',mar=c(2,2))
dev.copy2eps(file='temperature_heatmap.eps')
dev.copy(png,file='temperature_heatmap.png')
dev.off()

mydata<-apply(prec,c(1,2),as.numeric)
mydist<-dist(mydata)
heatmap(as.matrix(mydist),labRow='',labCol='',mar=c(2,2))
dev.copy2eps(file='precipitation_heatmap.eps')
dev.copy(png,file='precipitation_heatmap.png')
dev.off()



