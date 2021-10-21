# Making logos for ourcodingclub.github.io
# John Godlee 2017_09_22

# Set working directory to the location of the source file
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Packages
library(grid)
library(ggplot2)
library(png)

# Image
img <- readPNG("sample_image.png")
g <- rasterGrob(img, interpolate=TRUE, width = 0.5)
ggplot() + annotation_custom(g)

# ggplot2 ----
# Hexagon
ord_hex <- c(1, 2, 3, 4, 5, 6, 7, 8) 
x_hex <- c(-10, -5, 5, 10, 5, -5, -10, -5)
y_hex <- c(-0, 10, 10, 0, -10, -10, -0, 10)
hex <- data.frame(ord_hex, x_hex, y_hex)

# Shade
ord_shad_main <- c(1, 2, 3, 4)
x_shad_main <- c(-4.8, 5.3, 10.5, 5.5)
y_shad_main <- c(10.5, 10.5, 0, -10)
shad_main <- data.frame(ord_shad_main, x_shad_main, y_shad_main)

ord_shad_bot <- c(1, 2)
x_shad_bot <- c(5.5, 0)
y_shad_bot <- c(-10, -10)
shad_bot <- data.frame(ord_shad_bot, x_shad_bot, y_shad_bot)

ord_shad_top <- c(1, 2)
x_shad_top <- c(-4.8, -7.5)
y_shad_top <- c(10.5, 5)
shad_top <- data.frame(ord_shad_top, x_shad_top, y_shad_top)

# Clipping mask
org_clip_l <- c(1, 2, 3, 4, 5)
x_clip_l <- c(-4.8, -30, -30, -5, -10)
y_clip_l <- c(10.6, 10.6, -10.2, -10.2, 0)
clip_l <- data.frame(org_clip_l, x_clip_l, y_clip_l)

org_clip_r <- c(1, 2, 3, 4, 5)
x_clip_r <- c(5.3, 30, 30, 5.5, 10.5)
y_clip_r <- c(10.6, 10.6, -10.2, -10.2, 0)
clip_r <- data.frame(org_clip_r, x_clip_r, y_clip_r)

org_clip_t <- c(1, 2, 3, 4)
x_clip_t <- c(-30, -30, 30, 30)
y_clip_t <- c(10.5, 30, 30, 10.5)
clip_t <- data.frame(org_clip_t, x_clip_t, y_clip_t)

org_clip_b <- c(1, 2, 3, 4)
x_clip_b <- c(-30, 30, 30, -30)
y_clip_b <- c(-10, -10, -30, -30)
clip_b <- data.frame(org_clip_b, x_clip_b, y_clip_b)

# Text
text <- c("TEST")

# Test
ggplot() + 
	geom_point(data = hex, aes(x = x_hex, y = y_hex)) + 
	geom_path(data = shad_main, aes(x = x_shad_main, y = y_shad_main), color = "grey") + 
	geom_path(data = shad_top, aes(x = x_shad_top, y = y_shad_top), color = "grey") +
	geom_path(data = shad_bot, aes(x = x_shad_bot, y = y_shad_bot), color = "grey") + 
	geom_path(data = hex, aes(x = x_hex, y = y_hex), color = "orange") + 
	geom_text(aes(x = 0, y = -5, label = text), size = 25) + 
	geom_polygon(data = clip_l, aes(x = x_clip_l, y = y_clip_l), fill = "white") + 
	geom_polygon(data = clip_r, aes(x = x_clip_r, y = y_clip_r), fill = "white") +
	geom_polygon(data = clip_t, aes(x = x_clip_t, y = y_clip_t), fill = "white") + 
	geom_polygon(data = clip_b, aes(x = x_clip_b, y = y_clip_b), fill = "white") 

# With extras
ggplot() + 
	annotation_custom(g) +
	geom_polygon(data = clip_l, aes(x = x_clip_l, y = y_clip_l), fill = "white") + 
	geom_polygon(data = clip_r, aes(x = x_clip_r, y = y_clip_r), fill = "white") +
	geom_polygon(data = clip_t, aes(x = x_clip_t, y = y_clip_t), fill = "white") + 
	geom_polygon(data = clip_b, aes(x = x_clip_b, y = y_clip_b), fill = "white") +
	geom_path(data = shad_main, aes(x = x_shad_main, y = y_shad_main), color = "grey", size = 5, lineend = "round") + 
	geom_path(data = shad_bot, aes(x = x_shad_bot, y = y_shad_bot), color = "grey", size = 5, lineend = "round") + 
	geom_path(data = shad_top, aes(x = x_shad_top, y = y_shad_top), color = "grey", size = 5, lineend = "round") + 
	geom_path(data = hex, aes(x = x_hex, y = y_hex), color = "orange", size = 5) + 
	geom_text(aes(x = 0, y = -5, label = "TEST"), size = 15) +
	theme(axis.title = element_blank(),
				axis.text = element_blank(),
				axis.ticks = element_blank(),
				panel.grid.major.x = element_blank(),
				panel.grid.minor.x = element_blank(),
				panel.grid.minor.y = element_blank(),
				panel.grid.major.y = element_blank(),
				panel.background = element_blank(),
				legend.position = "none")