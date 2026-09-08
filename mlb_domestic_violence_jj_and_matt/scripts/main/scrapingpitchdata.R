## NEW TESTING STUFF ##
rm(list = ls())
library(baseballr)

# Set the desired years
years <- c(2009:2012)

# Create a directory to store the CSV files
output_dir <- "C:\\Users\\Kal\\Desktop\\Health\\MLB_Domestic_Violence\\Data\\mlb\\ScrapedPitches"
dir.create(output_dir, showWarnings = FALSE)

for (year in years) {
  for (month in 3:11) {
    if (month == 4 | month == 6 | month == 9 | month == 11) {
      # For months with 30 days
      days_in_month <- 30
    } else {
      # For months with 31 days
      days_in_month <- 31
    }
    
    for (day in 1:days_in_month) {
      date <- paste0(year, "-", sprintf("%02d", month), "-", sprintf("%02d", day))
      print(paste("Scraping data for date:", date))
      
      # Scrape pitch data for the specific date
      day_pitch_data <- scrape_statcast_savant(start_date = date, end_date = date, player_type = "pitcher")
      
      # Save the pitch data to a CSV file
      filename <- paste(output_dir, "/pitch_data_", date, ".csv", sep = "")
      write.csv(day_pitch_data, file = filename, row.names = FALSE)
      
      Sys.sleep(1)
    }
  }
}


## combining the datasets ##
setwd("C:\\Users\\Kal\\Desktop\\Health\\MLB_Domestic_Violence\\Data\\mlb\\ScrapedPitches")
pitch_data_combined <- data.frame()

for (year in years) {
  pitch_data_year <- data.frame()
  for (month in 3:11) {
    if (month == 4 | month == 6 | month == 9 | month == 11) {
      # For months with 30 days
      days_in_month <- 30
    } else {
      # For months with 31 days
      days_in_month <- 31
    }
    for (day in 1:days_in_month) {
      filename <- paste0("pitch_data_", year, "-", sprintf("%02d", month), "-", sprintf("%02d", day), ".csv")
      if (file.exists(filename)) {
        print(paste("Adding data for:", filename))
        day_pitch_data <- read.csv(filename)
        pitch_data_year <- rbind(pitch_data_year, day_pitch_data)
      }
    }
  }
  print(paste("Binding data for:", year))
  pitch_data_combined <- rbind(pitch_data_combined, pitch_data_year)
  write.csv(pitch_data_year, file = paste0("pitch_data_", year, ".csv"), row.names = FALSE)
}

for (year in years) {
  assign(paste0("pitches_", year), read.csv(paste0("C:\\Users\\Kal\\Desktop\\Health\\MLB_Domestic_Violence\\Data\\mlb\\ScrapedPitches\\pitch_data_", year, ".csv")))
}

keeps <- c("game_date", "events", "description", "zone", "game_type", "home_team", "away_team", "type", "hit_location", "bb_type", "balls", "strikes", "game_year", "plate_x", "plate_z", "on_3b", "on_2b", "on_1b", "outs_when_up", "inning", "inning_topbot", "sz_top", "sz_bot", "game_pk", "at_bat_number", "pitch_number", "home_score", "away_score", "bat_score", "fld_score", "post_away_score", "post_home_score", "post_bat_score", "post_fld_score", "delta_home_win_exp", "delta_run_exp")

for (year in years) {
  dataset_name <- paste0("pitches_", year)
  assign(dataset_name, get(dataset_name)[, keeps])
}

################################################################################################ LOOP STARTS HERE ##################################################################################
rm(list = ls())
library(baseballr)

# Set the desired years
years <- c(2009:2009)

# Create a directory to store the CSV files
output_dir <- "C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/mlb/ScrapedPitches"
dir.create(output_dir, showWarnings = FALSE)

for (year in years) {
  year_pitch_data <- data.frame()
  for (month in 3:11) {
    if (month == 4 | month == 6 | month == 9 | month == 11) {
      # For months with 30 days
      days_in_month <- 30
    } else {
      # For months with 31 days
      days_in_month <- 31
    }
    
    for (day in 1:days_in_month) {
      date <- paste0(year, "-", sprintf("%02d", month), "-", sprintf("%02d", day))
      print(paste("Scraping data for date:", date))
      
      # Scrape pitch data for the specific date
      day_pitch_data <- scrape_statcast_savant(start_date = date, end_date = date, player_type = "pitcher")
      
      # Append the pitch data to the year's dataframe
      year_pitch_data <- rbind(year_pitch_data, day_pitch_data)
    }
  }
  
  # Save the pitch data for the year to a CSV file
  output_file <- paste0(output_dir, "/pitch_data_", year, ".csv")
  write.csv(year_pitch_data, file = output_file, row.names = FALSE)
}

