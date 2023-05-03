
#filtering eBird data using auk
#eBird data is available for download through ebird.org
#The query information for the eBird Basic Dataset used in this project:
#Species: Common Loon
#Region: Washington State
#Date Range: All Dates
#Options: did not include unvetted data

#to include non-detections, the 'Sampling Event Data' was also downloaded

#information on the auk package and detailed instructions can be found here:
# https://cornelllabofornithology.github.io/auk/index.html
library(auk)
library(dplyr)
library(devtools)

basicdat<-file("ebd_US-WA_relNov-2017.txt")
samplingdat<-file("ebd_sampling_relNov-2017.txt")
ebd<-auk_ebd(file="ebd_US-WA_relNov-2017.txt", 
             file_sampling="ebd_sampling_relNov-2017.txt") 
ebd_filters_all<- ebd %>%
  auk_country("US") %>%
  auk_extent(extent=c(-124.5, 44.5, -115.5, 50))%>% #lat and long bbox around WA
  auk_date(date=c("2000-01-01","2017-12-31"))%>%
  auk_species(species="Common Loon")%>%
  auk_complete() #argument for complete checklists only
ebd_filters_all


#execute filters
ebdfilt_all<-auk_filter(ebd_filters_all,
                        file="ebd_filter.txt",
                        file_sampling="filter_sampling.txt", overwrite=TRUE)

XA<-read_ebd(ebdfilt_all)
#remove states that arent Washington (just to make sure there arent any)
XA<-XA[!XA$state_province=="Idaho",]
XA<-XA[!XA$state_province=="Montana",]
XA<-XA[!XA$state_province=="Oregon",]


#remove non-summer months, only want June, July, August
library(lubridate)
XA$observation_date<-as.Date(XA$observation_date)
monthA<-yearA<-dayA<-numeric(length((XA$observation_date)))
X_ALL<-as.data.frame(XA,month,year,day)
X_ALL$month<-month(XA$observation_date)
X_ALL$year<-year(XA$observation_date)
X_ALL$day<-day(XA$observation_date)


X_ALL<-X_ALL[!X_ALL$month<6,]
X_ALL<-X_ALL[!X_ALL$month>8,]

library(lubridate)

#convert month and days
#for both change the format of the date
#sort by month then day
day<-month<-year<-Date<-numeric(length(X_ALL[,1]))

day<-day(as.Date(X_ALL$observation_date, "%m /%d /%Y"))
month<-month(as.Date(X_ALL$observation_date, "%m /%d /%Y"))
year<-year(as.Date(X_ALL$observation_date, "%m /%d /%Y"))
for(i in 1:length(X_ALL$observation_date)){
  Date[i]<-paste(month[i], day[i], sep="")
}


#remove any 'casual' observations, i.e. only want stationary, travelling, or area protocols
X_ALLt<-X_ALL[which(X_ALL$protocol_type=="Traveling"),]
X_ALLa<-X_ALL[which(X_ALL$protocol_type=="Area"),]
X_ALLs<-X_ALL[which(X_ALL$protocol_type=="Stationary"),]

#output separate csv files to be using in ArcMap
#for each of the different protocol types, only unique checklist ids were
# retained

#Stationary protocols were given a 20 meter buffer around the observation point in ArcMap
X_ALLs<-travel[which(!duplicated(X_ALLs$checklist_id)),]
write.csv(X_ALLs, file="stationaryEBD.csv")

#Area and traveling protocol types were given an extra column for buffer distance

#Area protocols include information on the area covered during the observation
#this effort information was transformed into a buffered distance to be used in ArcMap
#effort area needs a buffer distance for ArcMap, in X Meters
#Note that for a buffer to be mapped in ArcMap, the entries need to include the units 
#of the distance in writing, so an entry would be 'X Meters'
area<-EBD[which(EBD$protocol_type=="Area"),]
radiusM<-numeric(length(area[,1]))
for(i in 1:length(radiusM)){
  radiusM[i]<-sqrt((area$effort_area_ha[i]/pi)*10000)
  ifelse(radiusM[i]<10, radiusM[i]<-20, radiusM[i]<-radiusM[i])
  radiusM[i]<-signif(radiusM[i], 7)
}
for(i in 1:length(radiusM)){
  radiusM[i]<-paste(radiusM[i], "Meters", sep=" ")
}
area<-cbind(area, radiusM)
area<-area[which(!duplicated(area$checklist_id)),]
write.csv(area, file="areaEBD.csv")

#For travel protocol, the distance travelled is given the word 'Kilometers' after it
# for mapping and buffering in ArcMap
travel<-EBD[which(EBD$protocol_type=="Traveling"),]
distance<-numeric(length(travel[,1]))
for(i in 1:length(distance)){
  distance[i]<-travel$effort_distance_km[i]
  distance[i]<-paste(travel$effort_distance_km[i], "Kilometers", sep=" ")
}

travel<-cbind(travel, distance)
travel<-travel[which(!duplicated(travel$checklist_id)),]
write.csv(travel, file="travelEBD.csv")

#each csv was imported into ArcMap and those observations that intersected one of the 
#COLO minimum requirement sites were retained, while those observations that did not 
#intersect a site or intersected marine waters were removed


