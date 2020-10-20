########################################################################################
#
# Program:       Create spatial heatmaps using ggplot
#
########################################################################################
rm(list=ls())

# load library
library(tigris)        # Download shapefiles from the US Census Bureau and load into R
library(leaflet)       # Create spatial maps
library(RColorBrewer) 
library(ggplot2)
library(sf)
library(haven)
library(leafem)        # Using features with leaflet maps
library(mapview)
library(tidycensus)
library(dplyr)


########################################
#   Part 1. Load School District Data 
########################################

# Read in district data
login_dat <- read_dta("login_tracts_by_gradelevel.dta") 

# Make sure tract number is character since GEOID in census tract is a character variable
login_dat$tract <- as.character(login_dat$tract_id)


########################################
#   Part 2. Load Census Data 
########################################

# Get Tract Info
census <- get_acs(state =____, county=____, 
               geography = "tract", 
               variables = "B19013_001", 
               geometry = TRUE)

# Select tracts that are in our data since we are interested in census tracts in which our students reside
census <- census[census$GEOID %in% login_dat$tract, ]

# We want consistent variable name to merge
census$tract <- census$GEOID

# Join census data with school data  
df <- inner_join(census, login_dat, by="tract")

# Reorder the grade level span for graphing
table(df$grade_span)
df$grade_span <- factor(df$grade_span, levels=c("K-2", "Gr 3-5", "Gr 6-8", "Gr 9-12"))


######################################################
#   Part 3. Create Spatial Heatmaps
######################################################

# Set theme
no_axes <- theme(
  axis.text = element_blank(),
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank()
)


#--- 3.1 Create spatial heatmap for percentage of chronic absentee by tract and grade span --- #

# Create map showing percentage of chronically absent students
m2<- df %>%
  ggplot(aes(fill = pct_no_login)) +
  geom_sf() +
  theme_bw() + 
  no_axes +
  scale_fill_viridis_c(option = "inferno", direction=-1) +
  facet_wrap(~grade_span, ncol=4)+ 
  theme(legend.position = "bottom",
        strip.text.x = element_text(size = 30))

# Plot and save
m2
ggsave("login_by_grade_onerow.png")






