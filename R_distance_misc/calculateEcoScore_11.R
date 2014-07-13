###attempt to precauculate eco-score in R (because easy parallel), so that average statistics for trips from each country could be calculated. Never finished, uploaded as is. 

#libraries
library(geosphere)

###first try dummy weightings... never fine tuned 
CO2_DOM_SHORT<-259
CO2_DOM_LONG<-178
CO2_INT_LONG<-114
CO2_SHORT_TRHD<-4630000
CO2_LONG_TRHD<-7000000
CO2_MAX_FLIGHT<-10488000
#CO2_MAX_LAND<-1
#CO2_MIN_LAND<-0 
countrystats<-read.csv('COUNTRIES_ecodat.csv')
rownames(countrystats)<-countrystats[,'country_name']
countrynum<-apply(countrystats[,5:14],c(1,2),as.numeric)
subvalues<-apply(countrynum,2,function(x) mean(na.omit(x)))
for (i in 1:10){
	countrystats[which(is.na(countrystats[,i])),i]<-subvalues[i]
}
MAX_RENEWS<-max(countrynum['renewable_energy'])
CO2_MAX_LAND<-max(countrynum['co2_emission'])
CO2_MIN_LAND<-min(countrynum['co2_emission'])
#calculation of EcoScore
calculateEcoScore <-function (Distance,StartLatitude=NA,StartLongitude=NA,DestLatitude=NA,DestLongitude=NA,StartCountry=NA,DestCountry,StartMonth=NA){
#	co2_factor = 0
	print(Distance)
	print(DestCountry)
	#co2scaling (longer is more efficienty per kilometer)
	if(Distance <= CO2_SHORT_TRHD){
	        co2_factor = CO2_DOM_SHORT
	}else if(Distance > CO2_LONG_TRHD){   
		co2_factor = CO2_INT_LONG
	}else{
		co2_factor = CO2_DOM_LONG
	}
	##flight-related
	co2_flight_abs =  Distance * co2_factor
	co2_flight_relscore = 1 - (co2_flight_abs / (CO2_MAX_FLIGHT*CO2_INT_LONG))
	if (co2_flight_relscore < 0) {
		co2_flight_relscore<-0
	}
	##country-related
	##TODO: $data = database query
	#get data from country
	mydata<-countrynum[DestCountry,]
	co2_land_abs = as.numeric(as.vector(mydata["co2_emission"]))
	co2_land_relscore = 1 - ((co2_land_abs -CO2_MIN_LAND)/(CO2_MAX_LAND-CO2_MIN_LAND))
	renewable_energy_relscore = as.numeric(as.vector(mydata["renewable_energy"]))
	forest_relscore = as.numeric(as.vector(mydata["forest_area"]))/100
	endangered_species_relscore = as.numeric(as.vector(mydata["endangered_species"]))
	##EcoScore with weights
	##TODO: Discuss weighting
	co2_flight_final           = 40 *  co2_flight_relscore
	co2_land_final             = 10 *  co2_land_relscore
	renewable_energy_final     = 10 *  renewable_energy_relscore
	forest_land_final          = 20 *  forest_relscore
	endangered_species_final   = 10 *  endangered_species_relscore
	out<-sum(co2_flight_final,co2_land_final,renewable_energy_final,forest_land_final,endangered_species_final)
#	print(c(out,co2_flight_final,co2_land_final,renewable_energy_final,forest_land_final,endangered_species_final))
	#return c(out,co2_flight_final,co2_land_final,renewable_energy_final,forest_land_final,endangered_species_final)
	return(out)	
	##TODO: insert strings, Happy index, Guilty thing
}

##try out
#import (dummy) data
rawdata<-read.csv('../../hackathon_dummy.csv',header=F)
#read.csv('../hackathon.csv',header=F)
colnames(rawdata)<-c("search_year","search_month", "search_day", "search_platform", "dest_city","dest_country","dest_continent","dest_long", "dest_lat", "date_to", "date_from", "group_room", "orig_city","orig_country", "orig_continent", "orig_long", "orig_lat")
numdata<-apply(rawdata[,c(1,2,3,8,9,16,17)],c(1,2),as.numeric)

#calculate length of trips
#procdata<-c(rawdata,apply(rawdata,1,function(x) distCosine(p1=c(x["dest_long"]+180,x["dest_lat"]),p2=c(x["orig_long"]+180,x["orig_lat"]))))
procdata<-cbind(rawdata,apply(numdata,1,function(x) distVincentyEllipsoid(p1=c(x['dest_long'],x['dest_lat']),p2=c(x['orig_long'],x['orig_lat']))))
colnames(procdata)[length(procdata[1,])]<-'distance'

blah<-apply(procdata,1,function(x) tryCatch(calculateEcoScore(Distance=as.numeric(x['distance']),DestCountry=x['dest_country']),error=function(x) x<-NA))
