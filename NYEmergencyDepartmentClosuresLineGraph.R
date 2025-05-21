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

#Read Excel data
NYHospitals <- read_excel("NYS_ED_loc-2.xlsx")

#Filter data for specifc county
county_name <- "Orange County"
county_hospital_data <- NYHospitals %>%
  filter(subregion == county_name) %>%
  group_by(year) %>%
  summarise(num_hospitals = n(), .groups = "drop")

# Create the line plot for the specific county
ggplot(county_hospital_data, aes(x = year, y = num_hospitals)) +
  geom_line(color = "blue") +  # Adds the line
  geom_point(color = "red") +   # Adds points on the line
  labs(
    title = paste("Number of Emergency Department Closures", county_name, "Over Time"),
    x = "Year",
    y = "Number of Emergency Department Closures"
  ) +
  theme_minimal()


# Aggregate the total number of hospitals by year
hospital_count_over_time <- NYHospitals %>%
  group_by(year) %>%
  summarise(total_hospitals = n(), .groups = "drop")

# Create the line plot for total hospitals over time
ggplot(hospital_count_over_time, aes(x = year, y = total_hospitals)) +
  geom_line(color = "blue") +  # Adds the line
  geom_point(color = "red") +   # Adds points on the line
  labs(
    title = "Total Number of Hospitals Over Time",
    x = "Year",
    y = "Total Number of Hospitals"
  ) +
  theme_minimal()




# Assuming the dataset has columns for hospital name ('hospital_name') and year ('year')

# Step 1: Count the number of hospitals per year
hospital_count_per_year <- NYHospitals %>%
  group_by(fac_name, year, subregion) %>%
  summarise(num_hospitals = n(), .groups = "drop")

# Step 2: Calculate the change in the number of hospitals from one year to the next
hospital_changes <- hospital_count_per_year %>%
  arrange(fac_name, year,subregion) %>%
  group_by(fac_name) %>%
  mutate(change_in_hospitals = num_hospitals - lag(num_hospitals)) %>%
  ungroup()

# Step 3: Identify hospitals that gained or lost hospitals
hospital_changes <- hospital_changes %>%
  filter(!is.na(change_in_hospitals))  # Remove NA rows (those without previous year data)

# View the result
head(hospital_changes)

