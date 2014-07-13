### plots for the presentation, histograms of factors going into ecoscore, just for a brief idea of how much variation could be contributed by different factors.

countrystats<-read.csv('COUNTRIES_ecodat.csv')
rownames(countrystats)<-countrystats[,'country_name']
countrynum<-apply(countrystats[,5:14],c(1,2),as.numeric)
#plotting
par(mar=c(2,2,2,2))
#CO2
hist(countrynum[,'co2_emission'],ylab='',xlab='',col='grey20',breaks=30,xaxt='n',yaxt='n',main='')
dev.copy2eps(file='histCO2.eps')
dev.copy(png,file='histCO2.png',height=200,width=300)
dev.off()

#perFor
hist(countrynum[,'forest_area'],ylab='',xlab='',col='grey20',breaks=30,xaxt='n',yaxt='n',main='')
dev.copy2eps(file='histPerFor.eps')
dev.copy(png,file='histPerFor.png',height=200,width=300)
dev.off()

#alten
hist(countrynum[,'renewable_energy'],ylab='',xlab='',col='grey20',breaks=30,xaxt='n',yaxt='n',main='')
dev.copy2eps(file='histAltEn.eps')
dev.copy(png,file='histAltEn.png',height=200,width=300)
dev.off()

#end sp
hist(countrynum[,'endangered_species'],ylab='',xlab='',col='grey20',breaks=30,xaxt='n',yaxt='n',main='')
dev.copy2eps(file='histEndS.eps')
dev.copy(png,file='histEndS.png',height=200,width=300)
dev.off()
