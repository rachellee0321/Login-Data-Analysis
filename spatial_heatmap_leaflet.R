########################################################################################
#
# Program:       Create spatial heatmaps using leaflet
#
########################################################################################

rm(list=ls())         # clear all object from the current workspace 

# Load libraries
library(haven)
library(ggplot2) 
library(tigris)       # Download shapefiles from the US Census Bureau and load into R
library(leaflet)      # Create spatial maps
library(leafem)       # Using features with leaflet maps
library(leafsync)     # View multiple leaflet plots at once
library(mapview)      # Save leaflet plots

# For tigis package, we also have to run:
options(tigris_use_cache = FALSE)      


########################################
#   Part 1. Load School District Data 
########################################

# Read in data
tracts_all <- read_dta("login_racts_all.dta")

# Subset 2020 data
mytract <- subset(mytract, school_year == 2020)
nrow(mytract)

# Make sure tract number is string (because GEOID in the census data is character var)
mytract$tract <- as.character(mytract$tract_id) 


########################################
#   Part 2. Load Census Data 
########################################

# Get US census tract info for regions we are interested in
# For instance, state = "MA" and county is first three digits in census tract number
census_tract <- tracts(state = _____ , county = _____) 

# Only select tracts that are in my original data 
# We are only interested in the census tracts in which our students reside
census_tract <- census_tract[census_tract$GEOID %in% mytract$tract, ]

# Check structure of the data 
str(census_tract) 

# Merge census data with the original data
df <- geo_join(census_tract, mytract, "GEOID", "tract")


######################################################
#   Part 3. Create Spatial Heatmaps
######################################################

#--- 3.1 Create sptial heatmap for absence rate  --- #

# Turn abs_rate into percent
df$abs_rate <- round(df$abs_rate * 100, 1)

# Assign palette 
palette1 <- colorNumeric(palette =  "inferno",         # choose the color palette to use
                           domain = df$abs_rate,       # variable of interest
                           reverse = TRUE)             # direction of the palette
                                                       # Here,we want dark color for higher abs rate. 

# Create heatmap
m1 <-leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%         # background maps. ex)"OpenStreetMap.DE". see more here http://leaflet-extras.github.io/leaflet-providers/preview/index.html
      addPolygons(data = df, 
                  fillColor = ~palette1(abs_rate),     # call the palette
                  color = "#b2aeae",                   # boundary line 
                  fillOpacity = 0.9,                   # opacity of color
                  weight = 1,
                  smoothFactor = 0.2) %>% 
      addLegend(pal = palette1,                        # add legend
                values = df$abs_rate, 
                position = "bottomright",
                title = "Absence Rate",
                labFormat = labelFormat(suffix = "%"), # format it as '%' 
                opacity=1) 


# Let's overlay text over the graph  
m1.2<- leafem::addStaticLabels(m1, 
                               data=df, 
                               label = df$abs_rate,
                               style=list("color" ="black"))
# Plot the map
m1.2

# Save the map
mapshot(m1.2, file = "abs_rate_by_census_tract.png")



#--- 3.2 Map showing % Households below poverty level by Tract  --- #

# 'pct_hh_pov' is the pre-cleaned variable indicating percent of households below poverty level in our data.
# Turn the variable into percent
df$pct_hh_pov <- round(df$pct_hh_pov * 100, 1)

# We want to use different palette for this graph
palette2 <- colorNumeric(palette = "YlGnBu",      
                         domain = df$pct_hh_pov,
                         reverse = FALSE)    


# Create graph
m2 <-leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%         
      addPolygons(data = df, 
                  fillColor = ~palette2(pct_hh_pov),     
                  color = "#b2aeae",                   
                  fillOpacity = 0.9,                   
                  weight = 1,
                  smoothFactor = 0.2) %>%
      addLegend(pal = palette2,                      
                values = df$pct_hh_pov, 
                position = "bottomright",
                title = "Absence Rate",
                labFormat = labelFormat(suffix = "%"),  
                opacity=1) 

# Plot the map
m2

# Save the map
mapshot(m2, file = "hhlds_poverty_by_census_tract.png")


#  Let's see abs_rate and poverty maps side by side 
latticeView(m1,m2)


