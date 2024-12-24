# Activating repositories ----
setRepositories()  # Set repositories for installing packages

# Install and load necessary libraries
# pacman is a package manager for easy handling of package installations
# 'dependencies = TRUE' ensures that required dependencies are also installed
library(pacman)  # Load the pacman package for managing libraries
pacman::p_load(  # Load required libraries using pacman
  arcgis,         # ArcGIS R package for accessing ArcGIS data
  sf,             # Simple Features package for handling spatial data
  tidyverse,      # Collection of R packages for data manipulation and visualization
  elevatr,        # Elevation data retrieval package
  terra,          # Terra package for raster data manipulation
  rayshader       # Rayshader package for creating 3D visualizations
)

# Install remotes and other required packages
install.packages("remotes", dependencies = TRUE)  # Install remotes package for installing packages from GitHub
library(remotes)  # Load the remotes package to enable installation from GitHub

# Install and load terra package for raster data manipulation
install.packages("terra", dependencies = TRUE)  # Install terra package
library(terra)  # Load terra for working with raster data

install.packages("classInt", dependencies = TRUE)  # Install classInt for creating classification intervals
library(classInt)  # Load classInt for interval classification

install.packages("rayshader", dependencies = TRUE)  # Install rayshader for 3D plotting
library(rayshader)  # Load rayshader for 3D visualizations

library(sf)         # Load sf for spatial data handling
library(arcgis)     # Load ArcGIS for accessing ArcGIS data
library(tidyverse)  # Load tidyverse for data manipulation and plotting

# Install ArcGIS package from R-ArcGIS universe repository
install.packages("arcgis", repos = c("https://r-arcgis.r-universe.dev", "https://cloud.r-project.org"))

# Define a vector of libraries to check and install if necessary
libs <- c("arcgis", 
          "tidyverse", 
          "sf", 
          "terra", 
          "classInt", 
          "rayshader")  # List of libraries to be checked and installed

# Check which libraries are already installed
installed_libs <- libs %in% rownames(installed.packages())  # Check if libraries are already installed

# Install missing libraries if any are not installed
if(any(installed_libs == FALSE)) {  # If any libraries are missing, install them
  install.packages(libs[!installed_libs], dependencies = TRUE)  # Install missing libraries
}

# Load all required libraries
invisible(lapply(libs, library, character.only = TRUE))  # Load all libraries from the list

# Define URL for accessing ArcGIS city data
url1 <- "https://services.arcgis.com/P3ePLMYs2RVChkJx/ArcGIS/rest/services/World_Urban_Areas/FeatureServer/0"  # URL to access urban areas data

# Open the city data from ArcGIS service
city_data <- arcgislayers::arc_open(url1)  # Access the ArcGIS data from the specified URL

# Filter data for the city of Sukkur
city_sf <- arcgislayers::arc_select(city_data, fields = "Name", where = "Name = 'Sukkur'", crs = 4326)  # Select only Sukkur city data

# Plot the geometry of the selected city (Sukkur)
plot(sf::st_geometry(city_sf))  # Plot the geometry of Sukkur city

# Get the bounding box (spatial extent) for Sukkur city
city_bbox <- sf::st_bbox(city_sf)  # Retrieve the bounding box for Sukkur

# Define the URL for accessing Landsat data
url2 <- "https://landsat2.arcgis.com/arcgis/rest/services/Landsat8_Views/ImageServer"  # URL for Landsat 8 imagery

# Open Landsat data from ArcGIS service
landsat_data <- arcgislayers::arc_open(url2)  # Access Landsat data from the specified URL

# Extract raster data for Sukkur city from Landsat imagery
city_raster <- arcgislayers::arc_raster(
  x = landsat_data, 
  xmin = city_bbox[["xmin"]],  # Set minimum x coordinate of bounding box
  xmax = city_bbox[["xmax"]],  # Set maximum x coordinate of bounding box
  ymin = city_bbox[["ymin"]],  # Set minimum y coordinate of bounding box
  ymax = city_bbox[["ymax"]],  # Set maximum y coordinate of bounding box
  crs = sf::st_crs(city_sf),    # Set coordinate reference system to match the city's CRS
  width = 500,                  # Set width of raster to 500 pixels
  height = 500                  # Set height of raster to 500 pixels
)  # Extract the region of interest from Landsat data based on Sukkur's bounding box

# Check the names of the raster bands in the Landsat image
names(city_raster)  # Print the names of raster bands in the Landsat data

# NDVI Calculation --------
# Calculate NDVI (Normalized Difference Vegetation Index) using Landsat bands
b5 <- city_raster[[5]]  # Band 5 (Near-Infrared)
b4 <- city_raster[[4]]  # Band 4 (Red)

# NDVI formula: (NIR - Red) / (NIR + Red)
ndvi <- (b5 - b4) / (b5 + b4)  # Calculate NDVI

# Clamp the NDVI values to the range [0, 1]
ndvi_clamped <- terra::clamp(x = ndvi, lower = 0, upper = 1, values = TRUE)  # Clamp NDVI values to range from 0 to 1

# BREAKS AND COLORS --------
# Define classification breaks for NDVI values using equal intervals
breaks <- classInt::classIntervals(terra::values(ndvi_clamped), n = 7, style = "equal")$brks  # Classify NDVI values into equal intervals

# Create a color palette for visualizing NDVI values
colors <- hcl.colors(n = length(breaks), palette = "Green-Brown", rev = TRUE)  # Generate a green-brown color palette for NDVI

# 2D MAP OF NDVI --------
# Convert the NDVI raster data into a data frame for use in ggplot
ndvi_clamped_df <- ndvi_clamped |> as.data.frame(xy = T)  # Convert the raster to a data frame

# Rename the third column of the data frame to 'value' for clarity
names(ndvi_clamped_df)[3] <- "value"  # Rename the 'value' column in the data frame

# Create a ggplot visualization for NDVI using the data frame
p <- ggplot() +
  geom_raster(data = ndvi_clamped_df, aes(x = x, y = y, fill = value)) +  # Create a raster plot with 'value' as the fill
  scale_fill_gradientn(
    name = "NDVI", 
    colors = colors, 
    breaks = breaks, 
    labels = round(breaks, 2)
  ) +  # Apply a gradient color scale for the NDVI
  guides(
    fill = guide_colorbar(
      direction = "horizontal",
      barheight = unit(1.5 , "mm"),  # Set the height of the color bar
      barwidth = unit(100 , "mm"),   # Set the width of the color bar
      title.position = "top",         # Position the color bar title at the top
      title.hjust = .5,               # Center the title horizontally
      label.hjust = .5,               # Center the labels horizontally
      label.position = "bottom",      # Position the labels at the bottom
      nrow = 1,                       # Set the number of rows for the color bar
      byrow = TRUE                    # Ensure the color bar labels are placed by row
    )
  ) +  # Customize the color bar guide
  theme_minimal() +  # Use a minimal theme for the plot
  theme(
    axis.line = element_blank(),      # Remove axis lines
    axis.title.x = element_blank(),   # Remove x-axis title
    axis.title.y = element_blank(),   # Remove y-axis title
    axis.text.x = element_blank(),    # Remove x-axis text labels
    axis.text.y = element_blank(),    # Remove y-axis text labels
    legend.position = "top",          # Position the legend at the top
    legend.title = element_text(size = 11, color = "grey10"),  # Customize legend title text
    legend.text = element_text(size = 10, color = "grey10"),   # Customize legend label text
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.grid.major = element_blank(),  # Remove major grid lines
    plot.background = element_blank(),  # Remove plot background
    legend.background = element_rect(fill = "white", color = NA),  # Set legend background to white
    plot.margin = unit(c(t = .25, r = -1, b = -1, l = -1), "lines")  # Adjust plot margins
  )  # Customize plot theme (removing gridlines, axes, etc.)

# RENDER SCENE --------
# Generate a 3D plot of the NDVI map using rayshader
w <- ncol(ndvi_clamped)  # Get the width of the raster (number of columns)
h <- nrow(ndvi_clamped)  # Get the height of the raster (number of rows)

rayshader::plot_gg(
  ggobj = p,  # Input the ggplot object for 3D rendering
  width = w / 100,  # Scale the plot width
  height = h / 100,  # Scale the plot height
  windowsize = c(w, h),  # Set the window size for rendering
  scale = 300,  # Set the scaling factor for rendering
  solid = F,  # Set whether the surface is solid or not (not solid here)
  shadow = T,  # Enable shadow for 3D effect
  shadowcolor = "white",  # Set shadow color to white
  shadowwidth = 0,  # Set shadow width to 0
  shadow_intensity = 1,  # Set shadow intensity
  zoom = .5,  # Set zoom level for the 3D plot
  phi = 85,  # Set the vertical viewing angle for the 3D plot
  theta = 0  # Set the horizontal viewing angle for the 3D plot
)  # Render the 3D visualization using rayshader
