# Function allowing lat long to x y coordinate conversion
# John Godlee (johngodlee@gmail.com)
# 2018_04_20

# Packages ----
library(dplyr)  # Data manip.
library(rgdal)  # CRS stuff
library(raster)  # spLines()
library(rgeos)  # gDistance()

# Function ----

# stem_id = a unique ID string for each stem
# stem_lon, stem_lat = latitude longitude coordinates for each stem
# corner_id = unique ID string for each plot corner
# corner_lon, corner_lat = latitude longitude coordinates for each plot corner

latlong_xy <- function(stem_id, stem_lon, stem_lat, corner_id, corner_lon, corner_lat){
	
	long_2_utm <- function(x,y) {
		paste("UTM zone ",
					(floor((x + 180)/6) %% 60) + 1,
					ifelse(y < 0, "S", "N"), 
					sep = "")
	}
	
	epsg <- make_EPSG()  # Create list of EPSG dataset to search for CRS
	wgs84 <- epsg[grep("WGS 84", epsg$note, ignore.case=TRUE),]  # Search for wgs84
	wgs84[grep("longlat", wgs84$prj4, ignore.case=TRUE),]  # grep proj4string to check
	wgs84_crs <- CRS(wgs84[grep("longlat", wgs84$prj4, ignore.case=TRUE),]$prj4[2])  # Store string as vector
	
	# Must change the UTM zone to match location of your plot.
	utm_id <- long_2_utm(mean(corner_lon), 
											 mean(corner_lat))
	
	utm_zone_crs <- CRS(wgs84[grep(utm_id, wgs84$note, ignore.case=TRUE),]$prj4[1])  # grep for UTM zone and store
	
	# Convert stem data to utm
	stems_points <- SpatialPointsDataFrame(as.matrix(cbind(stem_lon, stem_lat)),  # extract only long lat coords
																				 proj4string = wgs84_crs,
																				 data = data.frame(stem_id))
	
	# Transform SPDF to utm
	stems_points_sp_utm <- spTransform(stems_points, utm_zone_crs)
	
	# Convert back to dataframe
	stems_points_df_utm <- as.data.frame(stems_points_sp_utm)
	
	# Give column names
	colnames(stems_points_df_utm) <- c("id", "x_utm", "y_utm")

	# Convert plot corners to utm ----
	plot_corners_clean <- data.frame(corner_id, "x" = corner_lon, "y" = corner_lat)
	
	plot_corners_points <- SpatialPointsDataFrame(plot_corners_clean[,2:3], 
																								proj4string = wgs84_crs, 
																								data = data.frame(plot_corners_clean[,1]))
	
	# Transform SPDF to utm
	plot_corners_points_utm <- spTransform(plot_corners_points, utm_zone_crs)
	
	# Convert back to dataframe
	plot_corners_df_utm <- as.data.frame(plot_corners_points_utm)
	
	# Give column names
	colnames(plot_corners_df_utm) <- c("id", "x_coord", "y_coord")
	
	
	# Make spatial lines from corners ----
	
	# Get corner locations
	corner_nw <- plot_corners_df_utm[1,]
	
	corner_ne <- plot_corners_df_utm[2,]
	
	corner_sw <- plot_corners_df_utm[4,]
	
	# Create x axis line
	x_line <- rbind(corner_nw, corner_ne) %>%
		dplyr::select(x_coord, y_coord) %>%
		as.matrix(.) %>%
		spLines(., crs = utm_zone_crs)
	
	# Create y axis line 
	y_line <- rbind(corner_nw, corner_sw) %>%
		dplyr::select(x_coord, y_coord) %>%
		as.matrix(.) %>%
		spLines(., crs = utm_zone_crs)
	
	# Calculate x y distances from line to point for each stem and append to data frame ----
	stems_points_df_utm$x_coord <- as.vector(gDistance(stems_points_sp_utm, y_line, byid = T))
	stems_points_df_utm$y_coord <- as.vector(gDistance(stems_points_sp_utm, x_line, byid = T))
	
	# Clean up old and unnecessary columns
	stems_loc_df <- stems_points_df_utm %>%
		dplyr::select("id", "x_coord", "y_coord")
	
	stems_loc_df
}

