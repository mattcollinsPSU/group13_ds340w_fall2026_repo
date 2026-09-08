rm(list = ls())

library(dplyr)
library(lubridate)

years <- 2009:2020

# Create an empty list to store the data frames
airquality_list <- list()

for (year in years) {
  dataset <- read.csv(paste0("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/airquality/daily_aqi_by_county_", year, ".csv"))
  
  # Subset the dataset for Ohio and Hamilton
  subsetted_dataset <- subset(dataset, State.Name == "Ohio" & county.Name == "Hamilton"|State.Name == "Ohio" & county.Name == "Cuyahoga"|State.Name == "Colorado" & county.Name == "Denver"|State.Name == "Michigan" & county.Name == "Wayne"|State.Name == "Texas" & county.Name == "Harris"|State.Name == "Missouri" & county.Name == "Jackson"|State.Name == "Wisconsin" & county.Name == "Milwaukee"|State.Name == "Washington" & county.Name == "King"|State.Name == "Texas" & county.Name == "Tarrant"|State.Name == "Minnesota" & county.Name == "Hennepin")
  
  # Assign the subsetted dataset to a data frame with a dynamically generated name
  assign(paste0("airquality_", year), subsetted_dataset)
  
  # Add the data frame to the list
  airquality_list[[year]] <- subsetted_dataset
}

# Create an empty data frame to store the combined data
airqualitydata <- data.frame()

# Combine the individual data frames using rbind
for (year in years) {
  if (exists(paste0("airquality_", year))) {
    airqualitydata <- rbind(airqualitydata, get(paste0("airquality_", year)))
  }
}

airqualitydata <- airqualitydata %>%
  mutate(
    team = case_when(
      county.Name == "Hamilton" ~ "CIN",
      county.Name == "Cuyahoga" ~ "CLE",
      county.Name == "Denver" ~ "COL",
      county.Name == "Wayne" ~ "DET",
      county.Name == "Harris" ~ "HOU",
      county.Name == "Jackson" ~ "KC",
      county.Name == "Milwaukee" ~ "MIL",
      county.Name == "King" ~ "SEA",
      county.Name == "Tarrant" ~ "TEX",
      county.Name == "Hennepin" ~ "MIN"
    )
  )

saveRDS(airqualitydata, "C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/airquality/dailyairqualitybycounty_2009_2020.rds")
