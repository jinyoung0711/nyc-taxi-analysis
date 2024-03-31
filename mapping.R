#### mapping ####

map.center.loc <- c(40.6643, -73.938)
input_zoom <- 11

NewYork_hotplace <- data.frame(latitude = c(40.755, 40.741, 40.753, 40.748, 40.759, 40.779, 
                                            40.762, 40.689, 40.709, 40.753 ), 
                               longitude = c(-73.987, -73.990, -73.982, -73.986, -73.981, -73.963, 
                                             -73.977, -74.045, -74.000, -73.977))

NewYork_hotelplace <- data.frame(latitude = c(40.747, 40.718, 40.743, 40.766, 40.765,
                                              40.7111, 40.614, 40.640, 40.711, 40.801),
                                 longitude = c(-74.025, -74.047, -73.902, -73.978, -73.930,
                                               -74.066, -74.002, -74.282, -73.807, -74.011))

#### pick mapping ####

maps.pickinfo <- selectedDataG1[, c(8, 7)]
maps.dropinfo <- selectedDataG1[, c(10, 9)]

map <- GetMap(center = map.center.loc,
              zoom = input_zoom, maptype = "roadmap", format = 'png8',
              destfile= "map.png")

PlotOnStaticMap(map, lat = maps.pickinfo$pickup_latitude, lon = maps.pickinfo$pickup_longitude,
                destfile = 'map.point.png', cex = 0.5, pch = 20, col = 'purple', add= FALSE)

PlotOnStaticMap(map, lat = NewYork_hotplace$latitude, lon = NewYork_hotplace$longitude,
                destfile = 'map.point.png', cex = 3, pch = '*', col = 'red', add= TRUE)

PlotOnStaticMap(map, lat = NewYork_hotelplace$latitude, lon = NewYork_hotelplace$longitude,
                destfile = 'map.point.png', cex = 3, pch = '*', col = 'black', add= TRUE)

legend("bottomright",  
       legend=c("pickup", "hot place", "hotel place"),   
       col=c("purple", "red", "black"), lwd=3)  

#### drop mapping ####

#map <- GetMap(center = map.center.loc,
zoom = input_zoom, maptype = "roadmap", format = 'png8',
destfile= "map.png")

#PlotOnStaticMap(map, lat = maps.dropinfo$dropoff_latitude, lon = maps.dropinfo$dropoff_longitude,
destfile = 'map.point.png', cex = 0.5, pch = 20, col = 'purple', add= FALSE)

#PlotOnStaticMap(map, lat = NewYork_hotplace$latitude, lon = NewYork_hotplace$longitude,
destfile = 'map.point.png', cex = 3, pch = '*', col = 'red', add= TRUE)

PlotOnStaticMap(map, lat = NewYork_hotelplace$latitude, lon = NewYork_hotelplace$longitude,
                destfile = 'map.point.png', cex = 3, pch = '*', col = 'black', add= TRUE)

legend("bottomright",  
       legend=c("dropoff", "hot place", "hotel place"),   
       col=c("purple", "red", "black"), lwd=3)  

ggplot(data = selectedDataG1, aes(x=pickup_day, fill=pickup_day))+
  geom_bar(position="dodge")+
  ggtitle("Plot of vendor_id by pickup_day\n")+
  theme(plot.title = element_text(hjust = 0.5))


# 전체 0.1% 상위 택시 기사 데이터 
selectedData <- rbind(selectedDataG1,selectedDataG2,selectedDataG3,selectedDataG4,selectedDataG5,selectedDataG6)

ggplot(selectedData, aes(x= factor(pickup_day), fill = pickup_day)) +
  geom_bar() + 
  ggtitle("Plot of Frequency in taxi by pickup_day\n")

ggplot(selectedData, aes(x= factor(dropoff_day), fill = dropoff_day)) +
  geom_bar() + 
  ggtitle("Plot of Frequency in taxi by dropoff_day\n")
