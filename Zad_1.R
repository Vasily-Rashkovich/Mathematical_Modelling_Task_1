library(tidyverse)
library(rnoaa)
library(lubridate)

station_data=read.csv('station_data.csv',header = TRUE,sep=",",dec=".")
Voronezh=data.frame(id="VORONEZH",latitude=51.6701,longitude=39.2105)
Voronezh_around=meteo_nearby_stations(lat_lon_df = Voronezh, station_data = station_data,
                                      limit=15, var = c("PRCP","TAVG"),
                                      year_min = 2007, year_max=2009)
All_voronezh_data=list()
for(i in 1:15){
  Voronezh_id=Voronezh_around[["VORONEZH"]][["id"]][i]
  All_voronezh_data[i]=list(meteo_tidy_ghcnd(stationid = Voronezh_id,
                                             var=c("PRCP","TAVG"),
                                             date_min = "2007-04-01",
                                             date_max = "2009-10-31"))}
  
act_sum5=vector()
d_coeff=vector()

for(year in 2007:2009){
  for(month in 4:10){
    month_sort=vector()
    for(station in 1:15){
      month_sort=(filter(All_voronezh_data[[station]],
                         date>=ymd(paste(toString(year),
                                         toString(month),"01",sep="-"))
                         &date<ymd(paste(toString(year),toString(month+1),"01",sep="-"))))
      c[station]=sum(month_sort[month_sort>50],na.rm = TRUE)/10
    }
    act_sum5=c(act_sum5,mean(a))
    d_coeff=c(d_coeff,length(month_sort[month_sort>70]/(length(month_sort[station]))))
  }
}
  
