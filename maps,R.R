# Set the repository to access the required packages
setRepositories()

# Install the 'sf' package for working with spatial data
install.packages("sf", dependencies = TRUE)

# Install the 'rnaturalearth' package for obtaining natural earth geographical data
install.packages("rnaturalearth", dependencies = TRUE)

# Load all required libraries ----
library(readxl)        # For reading Excel files
library(tidyverse)     # For data manipulation and visualization
library(ggplot2)       # For advanced plotting capabilities
library(ggthemes)      # For additional ggplot2 themes
library(sf)            # For handling spatial data (simple features)
library(rnaturalearth) # For accessing geographical data from Natural Earth
library(rnaturalearthdata) # For additional natural earth data support
library(ggiraph)       # For creating interactive ggplot2 visualizations
library(giscoR)        # For accessing geospatial data and maps from GISCO

# Load the world map as a Simple Features (sf) object ----
world <- ne_countries(scale = "medium",  # Load countries at medium resolution
                      returnclass = "sf")  # Return the data as an sf object

# View the structure of the 'world' dataset in a tabular format
view(world)

# Practice Plot 1: Create a global map colored by income groups ----
ggplot(data = world, aes(fill = income_grp)) +  # Use 'income_grp' to fill map regions
  geom_sf() +                                 # Add the simple feature geometry
  theme_map() +                               # Apply a theme suitable for maps
  scale_fill_viridis_d()                      # Use Viridis color palette for discrete values

# Practice Plot 2: Create a population map for Africa ----
world %>%                                       # Start with the 'world' dataset
  filter(region_un == "Africa") %>%            # Filter data for African countries
  ggplot() +                                   # Initialize ggplot object
  geom_sf(aes(fill = pop_est / 1e6),           # Map population (in millions) to fill
          color = "white",                     # Set the country borders to white
          lwd = 0.3) +                         # Set the line width for borders
  theme_map()                                  # Apply the map-specific theme

# Practice Plot 3: Create a population map for Asia ----
world %>%                                       # Start with the 'world' dataset
  filter(region_un == "Asia") %>%              # Filter data for Asian countries
  ggplot() +                                   # Initialize ggplot object
  geom_sf(aes(fill = pop_est / 1e6),           # Map population (in millions) to fill
          color = "white",                     # Set the country borders to white
          lwd = 0.3) +                         # Set the line width for borders
  theme_map()                                  # Apply the map-specific theme

# Practice Plot 4: Create a population map for Europe ----
world %>%                                       # Start with the 'world' dataset
  filter(region_un == "Europe") %>%            # Filter data for European countries
  ggplot() +                                   # Initialize ggplot object
  geom_sf(aes(fill = pop_est / 1e6),           # Map population (in millions) to fill
          color = "white",                     # Set the country borders to white
          lwd = 0.3) +                         # Set the line width for borders
  theme_map()                                  # Apply the map-specific theme

# Install additional required packages ----
install.packages("janitor", dependencies = TRUE) # Install 'janitor' for cleaning data
install.packages("giscoR", dependencies = TRUE)  # Install 'giscoR' for geospatial data

# Load additional libraries
library(janitor)  # Load 'janitor' for cleaning column names
library(giscoR)   # Load 'giscoR' for geospatial data from GISCO

# Fetch the NUTS (Nomenclature of Units for Territorial Statistics) data for Germany ----
gisco_get_nuts(country = 'Germany') %>%   # Retrieve NUTS data for Germany
  as_tibble() %>%                         # Convert the data to a tibble format
  janitor::clean_names() %>%              # Clean column names for consistency
  count(levl_code)                        # Count the occurrences of each NUTS level code

# Fetch detailed NUTS level-3 data for Germany (2021) ----
germany_dist <- gisco_get_nuts(country = 'Germany',  # Specify country
                               nuts_level = 3,       # Specify NUTS level 3 (districts)
                               year = "2021",        # Set the year
                               epsg = 3035) %>%      # Set the projection (EPSG code)
  as_tibble() %>%                                    # Convert to a tibble
  janitor::clean_names()                             # Clean column names for consistency

# Fetch state-level (NUTS level-1) data for Germany ----
germany_states <- gisco_get_nuts(country = 'Germany',  # Specify country
                                 nuts_level = 1,       # Specify NUTS level 1 (states)
                                 year = "2021",        # Set the year
                                 epsg = 3035) %>%      # Set the projection (EPSG code)
  as_tibble() %>%                                     # Convert to a tibble
  janitor::clean_names()                              # Clean column names for consistency

# Plot Germany's districts ----
germany_dist %>%                              # Use the district-level data
  ggplot(aes(geometry = geometry)) +          # Specify geometry for the plot
  geom_sf()                                   # Add the simple feature geometry layer

# Beautify the plot and make it interactive using ggiraph ----
install.packages("ggiraph", dependencies = TRUE)  # Install 'ggiraph' for interactivity
library(ggiraph)                                  # Load 'ggiraph' library

# Create an interactive map of German districts with tooltips ----
gg_plt <- germany_dist %>%  # Use the district-level data
  ggplot(aes(geometry = geometry)) +  # Specify geometry for the plot
  geom_sf(data = germany_states,  # Overlay the state boundaries
          aes(fill = nuts_name,  # Fill states with their names
              color = "black",   # Set the outline color to black
              lwd = 0.5)) +      # Set the line width for boundaries
  geom_sf_interactive(fill = NA,  # Set fill color to transparent
                      aes(data_id = nuts_id,  # Use NUTS ID for interaction
                          tooltip = nuts_name),  # Set tooltips to NUTS names
                      color = 'black',  # Set border color for interactivity
                      lwd = 0.1) +      # Set border line width
  theme_void()  # Remove all background and axis elements

# Render the interactive map ----
girafe(ggobj = gg_plt)  # Render and display the interactive plot
