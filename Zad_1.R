library(tidyverse)
library(rnoaa)
library(lubridate)

af = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bf = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
di = c(0.00,0.00,0.00,0.33,1.00,1.00,1.00,0.32,0.00,0.00,0.00,0.00)
Kf = 300
Qj = 1600
Lj = 2.2 
Ej = 25

station_data=read.csv('station_data.csv',header = TRUE,sep=",",dec=".")
Voronezh=data.frame(id="VORONEZH",latitude=51.6701,longitude=39.2105)
Voronezh_around=meteo_nearby_stations(lat_lon_df = Voronezh, station_data = station_data,
                                      limit=15, var = "all",
                                      year_min = 2007, year_max=2009)
All_voronezh_data=tibble()
for(i in c(1,5,8,12:15)){
  Voronezh_id=Voronezh_around[["VORONEZH"]][["id"]][i]
   st_data=meteo_tidy_ghcnd(stationid = Voronezh_id,
                             var="all",
                             date_min = "2007-01-01",
                             date_max = "2009-12-31")
    All_voronezh_data = bind_rows(All_voronezh_data, st_data %>%
                           mutate(year = year(date), month = month(date)) %>%
                           group_by(month, year) %>%
                           summarise (tavg = sum(tavg[tavg>50])/10 ))}
  
Voronezh_clean=All_voronezh_data%>%
  group_by(month) %>%
  summarise(s = mean(tavg, na.rm = TRUE)) %>%
  mutate (a = af, b = bf) %>%
  mutate (fert = ((a + b * 1.0 * s) * di * Kf) / (Qj * Lj * (100-Ej)) )
Yield = sum(Voronezh_clean$fert); Yield
  
