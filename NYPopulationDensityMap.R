#Set Working Directory 
#setwd("/cloud/project")

#Loaded libraries 
library(readxl)
library(psych)
library(ggplot2)
library(maps)
library(leaflet)
library(dplyr)
library(readr)
library(sf)
library(stringr)

#Functions that counts hospitals by city
hospital_city_numbers <- function(data,subregion)
{
  data%>%
    group_by(!!sym(subregion)) %>%
    summarise(num_hospitals = n(), .groups = "drop")
}

#Read Excel data
NYHospitals <- read_excel("NYS_ED_loc-2.xlsx")
NYCountyData2010_2020<- read_excel("NYCountyData.xlsx")
NYCountyData2020_2023 <- read_excel("NYPopulationCounties2020-2023.xlsx")
counties_geojson <- st_read("new-york-counties.geojson")
NYPopulationCounties <- read.csv("table-data.csv")

#Renames column in NYCountyData2010_2020 and NYCountyData2020_2023
NYCountyData2010_2020 <- NYCountyData2010_2020 %>%
  rename(counties = `table with row headers in column A and column headers in rows 3 through 4 (leading dots indicate sub-parts)`) 
NYCountyData2020_2023 <- NYCountyData2020_2023 %>%
  rename(counties = `table with row headers in column A and column headers in rows 3 through 4 (leading dots indicate sub-parts)`) 

#Combines NYCounty Data Sheets into one county sheet
NYTotalCountyData <- NYCountyData2010_2020 %>%
  left_join(NYCountyData2020_2023, by = c("counties" = "counties"))

NYTotalCountyData$counties <- gsub("^\\.", "", NYTotalCountyData$counties)  # Remove leading period
NYTotalCountyData$counties <- gsub(", New York$", "", NYTotalCountyData$counties)  # Remove ", New York" at the end

#Filter data by year
NYHospitals_2020 <- NYHospitals %>%
  filter(year == 2021)

# Function Execution that counts number of hospitals in cities 
# and adds it to counties_data
city_numbers <- hospital_city_numbers(NYHospitals_2020, "subregion")

NYTotalCountyData <- NYTotalCountyData %>%
  left_join(city_numbers, by = c("counties" = "subregion"))

hospital_data_with_counts <- NYHospitals_2020 %>%
  left_join(city_numbers, by = c("subregion" = "subregion"))
counties_data <- counties_geojson %>%
  left_join(hospital_data_with_counts, by = c("name" = "subregion"))

# Determines Density of Hospital per 100,000 residents 
# To change number of hospitals to reflect year, change filer data by year value
# To change population year, change '...5' according to the desired year( see table, '...5' reflects 2020, '...6' reflects)
NYTotalCountyData <- NYTotalCountyData %>%
  mutate(
    num_hospitals = as.numeric(num_hospitals),
    population = as.numeric(`...4.y`),
    hospital_density_per10k = (num_hospitals / population) * 100000)

#Merges hospital density from NYTotalCountyData into counties_data
counties_data <- counties_data %>% 
  left_join(NYTotalCountyData %>% select(counties, hospital_density_per10k),
            by = c("name" = "counties"))

#Hospital Quartiles 
Q1 <- quantile(NYTotalCountyData$hospital_density_per10k, prob = 0.25, na.rm = TRUE)
Q2 <- quantile(NYTotalCountyData$hospital_density_per10k, prob = 0.50, na.rm = TRUE)
Q3 <- quantile(NYTotalCountyData$hospital_density_per10k, prob = 0.75, na.rm = TRUE)
max_value <- max(NYTotalCountyData$hospital_density_per10k, na.rm = TRUE)

breaks <- c(0, Q1, Q2, Q3, max_value)

# Check for non-unique breaks and adjust if necessary
# If there are duplicates, slightly modify the max value
if (length(unique(breaks)) != length(breaks)) {
  breaks <- unique(breaks)
  breaks[length(breaks)] <- breaks[length(breaks)] + 0.001  # Add a small buffer to max_value if it's duplicated
}

hospital_density_bins <- colorBin(palette = "YlGnBu", domain = counties_data$num_hospitals, bins = breaks)

# Create the interactive map based on hospital density
leaflet(counties_data) %>%
  addTiles() %>%
  addPolygons(
    data = counties_data, 
    color = ~hospital_density_bins(hospital_density_per10k),  # Use hospital density for color
    weight = 2, 
    opacity = 0.7, 
    fillOpacity = 0.7,
    popup = ~paste("County:", name, 
                   "<br> Hospital Density (per 100k residents):", round(hospital_density_per10k, 2))  # Show density value in popup
  ) %>%
  addLegend(
    position = "bottomright",
    pal = hospital_density_bins,  # Link the legend to the hospital density bins
    values = ~hospital_density_per10k,  # Legend values based on hospital density
    title = "Hospital Density (per 100k)",
    opacity = 1
  )
