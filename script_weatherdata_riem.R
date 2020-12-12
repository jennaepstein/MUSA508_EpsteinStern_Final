# gathering data for southern california stations using Riem

library(riem)
library(dplyr)
library(sf)

# vector of southern california station ids
weather_station_ids <- c("AVX", "BFL", "DAG", "L35", "BLH"," L08", "SDM", "BUR", "L71", "CMA", "CRQ", "CNO", "AJO", "DLO", "SEE", "EMT", "L18", "FUL", "HHR", "HMT" , "IPL" , "POC", "WJF" ,"LPC" , "LGB" , "WHP", "LAX" , "MHV" , "F70", "EED", "L52", "OKB", "ONT" , "OXR" , "PSP", "PMD" , "PRB", "RNM", "RAL", "SBD", "MYF", "SAN", "SBP", "SDB", "SNA", "SBA", "SMX", "SMO", "IZA", "MIT", "TSP", "TRM", "CCB", "VNY", "VCV")


# function
get_weather_features_by_station <- function(weather_station_ids, start_year, end_year, na.rm = TRUE){
  
  year_vec <- seq(start_year, end_year)
  i <- 1
  weather_data_list <- list()
  for(station_id in weather_station_ids){
    print(paste("Processing station", station_id))
    for(year in year_vec){
      start_date = paste0(year, "01-01")
      end_date = paste0(year, "12-31")
      weather_data <- riem_measures(station = station_id, date_start = start_date, date_end = end_date) %>% 
        dplyr::summarize(weather_station_id = station_id,
                         year = year,
                         mean_tmpf = mean(tmpf, na.rm = TRUE),
                         mean_precipitation = mean(p01i, na.rm = TRUE),
                         mean_humidity = mean(relh, na.rm = TRUE),
                         mean_wind_Speed = mean(sknt, na.rm = TRUE),
                         
        ) 
      weather_data_list[[i]] <- weather_data
      i <- i + 1
    }
  }
  
  do.call("rbind", weather_data_list) 
}

#then, run this to get the data
weather_data <- get_weather_features_by_station(weather_station_ids, 2015, 2019)


# after data is gathered, write to a csv (write.csv)
##  write.csv(weather_data, "weather_data_socal.csv")

# then, get the list of stations with names and lat/lon, then write that also to a csv
## asos_socal_stations <- riem_stations("CA_ASOS") %>% filter(str_detect(id, paste(weather_station_ids, collapse="|")))
## write.csv(asos_socal_stations, "asos_socal_stations_latlon.csv")

# combine the two csv files in excel and ready to use back in this script

```