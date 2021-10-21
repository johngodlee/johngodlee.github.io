# Importing gpx tracks from Android AAT and plotting them


# Packages ----
library(rgdal)  # readOGR(), ogrListLayers()
library(ggplot2)  # ggplot()
library(ggmap)  # get_map(), ggmap()

# setwd ----
setwd("~/tracks")

# Import file ----
# Find out what layers are in the file
(layers <- ogrListLayers("2018_04_18_0.gpx"))

# Import the points layer, which contains elevation data
track_points <- readOGR("2018_04_19_0.gpx", layer = layers[5])
# Import the tracks layer as a spatiallinesdataframe

# Test plot
plot(track_points)

# Transform data to data frame for plotting ----
# Create data frame from spatial object
track_df <- data.frame(track_points@coords, 
											 track_points$ele, 
											 track_points$time,
											 track_points$track_seg_point_id)

# Rename columns
names(track_df) <- c("lon", "lat", "elev", "time", "seg_id")

# Convert time to posixCT
track_df$time_posix <- track_df$time %>%
	as.POSIXct(., format = "%Y/%m/%d %H:%M:%S ")

# Create plots ----
# Create elevation plot
(elev_plot <- ggplot(track_df, aes(x = time_posix, y = elev)) + 
	geom_point() + 
	geom_smooth(method = "loess", span = 0.1) + 
	scale_x_datetime() + 
	theme_classic() + 
	xlab("Elevation (m)") + 
	ylab("Time"))

# Plot map using ggmap
goog_map <- get_map(location = track_points@bbox, 
										zoom = 15, 
										maptype = "roadmap", color = "bw")

(route_map <- ggmap(goog_map) + 
	geom_path(data = track_df,
						aes(colour = elev), size = 1.5) + 
	scale_color_gradientn(colours = rainbow(4)) +
	guides(colour = guide_colourbar(title="Elevation (m)")) + 
	xlab("Longitude") + 
	ylab("Latitude"))

