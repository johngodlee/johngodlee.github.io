# Plotting elevation profile Bridge of Allan to Blackford
# Data from https://www.doogal.co.uk/RouteElevation.php

# Packages
library(ggplot2)

# Import data
route <- read.csv("route.csv")
str(route)

# Make plot
ggplot(route, aes(x = distance_km, y = elevation_m)) + 
	geom_area(fill = "#4BA8AD", alpha = 0.7) + 
	geom_line() +
	theme_bw()
