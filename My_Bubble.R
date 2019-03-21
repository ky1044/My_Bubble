library(jsonlite)
library(ggplot2)
library(tidyverse)
library(ggmap)
library(geosphere)
library(grid)
library(ggforce)

#some things for you to define

#how big do you want your bubble(which is shaped like a circle) to be in meters?
my_radius=1000
#how far back do you want your location data to go back? (EDT is the East Coast time zone)
my_date="2019-01-28 0:0:0 EDT"
#how many times(squared and multiplied by 2) do you want to run the calculations to find the optimal center for your bubble?
#as you increase specificity, the center of the bubble will become more precise, and run time will increase quadratically.
#a good start may be 30, which should take less than take less than 1 minute.
specificity=30
# set your zoom on the map you create: the higher the value, the more zoomed in.
my_zoom=13

#load location history from JSON
#if you get an error message like "incomplete final line found on 'Location History.json'":
#go into your json file and go all the way down to the last line of the file and add a few lines at the bottom of the file by hitting the return key. 
data <- fromJSON(readLines("Location History.json"))
loc = data$locations
#convert latitudeE7 and longitudeE7 to readable coordinates
loc$lat = loc$latitudeE7 / 1e7
loc$lon = loc$longitudeE7/ 1e7
#convert time from miliseconds to year, month, date, time
loc$date <- ISOdatetime(1970,1,1,0,0,0)+as.numeric(loc$timestampMs)/1000

#define subset data in time frame of spring semester 2019
myloc <- subset(loc,loc$date>(my_date)& loc$accuracy<100, select = c("lon", "lat", "accuracy", "date") )

#calculate avg_coordinates for data set in timeframe
median_lon = median(myloc$lon)
median_lat = median(myloc$lat)
median_coordinates = matrix(c(median_lon, median_lat), ncol = 2)

myloc$distance <- distHaversine(matrix(c(myloc$lon, myloc$lat), ncol = 2),median_coordinates)
search_range=as.numeric(quantile(myloc$distance, c(.9)) )

#find distance search range
k=0
range_distance=0
while(range_distance<search_range){
  range_distance=distHaversine(matrix(c(median_lon+k, median_lat+k), ncol = 2),median_coordinates)
  k=k+.0001
}
k=k*2^.5

best_fit_lon=median_lon
best_fit_lat=median_lat
percent_fit=sum(myloc$distance<my_radius)/sum(myloc$distance>0)

#first loops compute first iteration of"best_fit" coordinates, covering as much area as possible
for(moving_lon in seq(from=median_lon-k, to=median_lon+k, by=k/specificity)){
  for(moving_lat in seq(from=median_lat-k, to=median_lat+k, by=k/specificity)){
    myloc$distance <- distHaversine(matrix(c(myloc$lon, myloc$lat), ncol = 2),matrix(c(moving_lon, moving_lat), ncol = 2))
    if(percent_fit<sum(myloc$distance<my_radius)/sum(myloc$distance>0)){
      best_fit_lon=moving_lon
      best_fit_lat=moving_lat
      percent_fit=sum(myloc$distance<my_radius)/sum(myloc$distance>0)
    }
  }
}

current_best_fit_lon=best_fit_lon
current_best_fit_lat=best_fit_lat

#second loops compute on a much smaller scale, closing in on the previously calculated "best_fit" coordinates
for(moving_lon in seq(from=current_best_fit_lon-(k*3/specificity), to=current_best_fit_lon+(k*3/specificity), by=k*3/specificity/specificity)){
  for(moving_lat in seq(from=current_best_fit_lat-(k*3/specificity), to=current_best_fit_lat+(k*3/specificity), by=k*3/specificity/specificity)){
    myloc$distance <- distHaversine(matrix(c(myloc$lon, myloc$lat), ncol = 2),matrix(c(moving_lon, moving_lat), ncol = 2))
    if(percent_fit<sum(myloc$distance<my_radius)/sum(myloc$distance>0)){
      best_fit_lon=moving_lon
      best_fit_lat=moving_lat
      percent_fit=sum(myloc$distance<my_radius)/sum(myloc$distance>0)
    }
  }
}

#finalize distance values based on computed "best_fit" coordinates
myloc$distance <- distHaversine(matrix(c(myloc$lon, myloc$lat), ncol = 2),matrix(c(best_fit_lon, best_fit_lat), ncol = 2))




#downloading map data from Google API, using personal key
ggmap::register_google(key = "YOUR KEY HERE")
map <- get_map( location = c(lon =best_fit_lon, lat=best_fit_lat), zoom = my_zoom, source = 'stamen', maptype = "toner-lite")


ggmap(map) +
  geom_point(data=myloc,aes(x = lon, y = lat, color = distance<my_radius)) +
  geom_point(aes(x=best_fit_lon, y=best_fit_lat), size = 3,alpha=1,colour="white")+
  scale_colour_manual(name = 'all points in my location history', values = setNames(c('black',"#1e90ff"),c(F,T)),labels = c("not in the bubble", "in the bubble"))+
  labs( x = "Longitude", y = "Latitude", title = paste0("My bubble: contains ",round((percent_fit*100),2),"% of my location history!"))
ggsave("My Bubble.jpeg")


paste0("Based on my location history, I have spent ",(percent_fit*100),"% of my time in a bubble defined by a longitude of ",best_fit_lon,", a latitude of ",best_fit_lat,", and a radius of ",my_radius," meters.")


