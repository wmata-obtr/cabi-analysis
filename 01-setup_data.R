## 01-setup_data.R
## For creating data frames that can be used to analyze ridership by 
## distance to CaBi docks

#load relevant packages
library(tidyverse)

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
  mutate(feet = distance*5280) #%>% 
  mutate(category = case_when(feet<= 250 ~ "At Station",
                             feet > 250 & feet <= 1320 ~ "Walkable",
                             feet > 1320 ~ "far")) %>%
  rename(station=nearestMetro, dock_id=station_id)


#combine ridership and cabi dock distance
# this command omits the mismatched station names
metro <- merge(metroRidership, metroDistance[,
                               c("station","feet","category")], by="station")

# get cross table of category and district
counts <- metroDistance %>%
  count(category,district)

#look at spread of distances to cabi dock
summary(metroDistance$feet)
hist(metroDistance$feet[metroDistance$feet < 1000], main="Histogram of distance
     in feet from cabi docks",xlab="distance (ft)")

#create a pivot-table of ridership in 2 years
rides_by_cat2019 <- metro %>%
  filter(year==2019, month>3, month<12) %>% 
  group_by(category,month) %>% 
  summarize(avg2019 = mean(ridership)) %>% 
  ungroup()

rides_by_cat2020 <- metro %>% 
  filter(year==2020, month > 3, month < 12) %>% 
  group_by(category, month) %>% 
  summarize(avg2020 = mean(ridership)) %>% 
  ungroup()

rides_by_cat <- merge(rides_by_cat2019,rides_by_cat2020,by=c("category","month"))
rides_by_cat <- rides_by_cat %>%
  mutate(pct_recover = avg2020/avg2019) %>% 
  mutate_if(is.character,as.factor)
rides_by_cat$category <- factor(rides_by_cat$category, 
                                levels = c("At Station","Walkable","Far"))
levels(rides_by_cat$category)
  

#make sure to have ggplot package ready
ggplot(data=rides_by_cat, aes(x=month,y=pct_recover,fill=category)) +
  geom_bar(position = "dodge", stat="identity") +
  ggtitle("Ridership recovery by station type") +
  theme(legend.title = element_blank(), legend.position = c(.11,.85)) +
  scale_fill_manual(values=c("green4","red3","mediumblue"))
  

# try other categorization scheme
station_cat <- cabiDistance %>% 
  filter(distance < .25) %>% 
  count(nearestMetro) %>% 
  rename(walkable_cabis = n)

station_cat <- merge(station_cat, metroDistance[,
                                                c("nearestMetro","feet")],by="nearestMetro")
