## 01-setup_data.R
## For creating data frames that can be used to analyze ridership by 
## distance to CaBi docks

#load relevant .csv files
cabiDistance <- read.csv('cabi-distance.csv', header=TRUE)
cabiRidership <- read.csv('cabi-ridership.csv', header=TRUE)
metroRidership <- read.csv('station_monthly_ridership.csv', header=TRUE)

#clean up cabiDistance variable names
cabiDistance <- rename(cabiDistance,dockName=name,
                       nearestMetro=nearest_metro_station,distance=distance_miles)
#clean up cabiRidership vairable names
cabiRidership <- rename(cabiRidership,dockName=ï..Station.Name,
                        endingTrips=Ending.Trips,startingTrips=Starting.Trips)
#clean up metroRidership variable names
metroRidership <- rename(metroRidership,station=ï..station)

#create a new data frame for metro station's closest cabi dock
stations <- metroRidership %>% 
  distinct(station)

#create data frame
metroDistance <- cabiDistance %>% 
  group_by(nearestMetro) %>% 
  filter(distance == min(distance)) %>% 
  filter(station_id == min(station_id)) %>% 
  ungroup() %>%
  mutate(feet = distance*5280) %>% 
  mutate(category = case_when(feet <= median(feet) ~ "immediate",
                    feet > median(feet) & feet <= quantile(feet,.75) ~ "close",
                    feet > quantile(feet,.75) & feet <= 1320 ~"walkable",
                    feet > 1320 ~ "far")) %>% 
  rename(station=nearestMetro,dock_id=station_id)

#combine ridership and cabi dock distance
# this command omits the mismatched station names
metro <- merge(metroRidership, metroDistance[,c("station","feet")], by="station")

# get cross table of category and district
counts <- metroDistance %>%
  count(category,district)

fuzzy_join(metroDistance,metroRidership,by=4,1)
