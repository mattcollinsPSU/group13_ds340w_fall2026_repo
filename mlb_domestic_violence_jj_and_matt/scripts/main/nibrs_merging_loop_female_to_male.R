rm(list = ls())
library(lubridate)
library(dplyr)

# Create an empty data frame to store the results
dvcount <- data.frame()

# Loop over the years
for (year in 2009:2019) {
  # Construct the file paths
  administrative_file <- paste0("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_1991_2020_administrative_segment_rds/nibrs_administrative_segment_", year, ".rds")
  offender_file <- paste0("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_1991_2020_offender_segment_rds/nibrs_offender_segment_", year, ".rds")
  victim_file <- paste0("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_1991_2020_victim_segment_rds/nibrs_victim_segment_", year, ".rds")
  batch_header_file <- paste0("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_batch_headers_rds/nibrs_batch_header_", year, ".rds")
  
  # Load the data for the current year
  administrative_data <- readRDS(administrative_file)
  offender_data <- readRDS(offender_file)
  victim_data <- readRDS(victim_file)
  batch_header_data <- readRDS(batch_header_file)
  
  # Reduce the size of the files to only domestic violence victims
  keeps <- c("ori","population","city_name","population_group","country_division","country_region","core_city","covered_by_ori")
  batch_header_data <- batch_header_data[keeps]
  
  drops <- c("report_date_indicator","total_offense_segments","total_victim_segments","total_offender_segments","total_arrestee_segments","city_submissions","cleared_exceptionally","exceptional_clearance_date")
  administrative_data <- administrative_data[ , !(names(administrative_data) %in% drops)]
  
  administrative_data <- merge(administrative_data, batch_header_data, by = "ori")
  
  domestic_victim_data <- victim_data
  domestic_victim_data$spouse <- ifelse(domestic_victim_data$relation_of_vict_to_offender1 == "victim was spouse", 1, 0)
  domestic_victim_data$boyfriend <- ifelse(domestic_victim_data$relation_of_vict_to_offender1 == "victim was boyfriend/girlfriend", 1, 0)
  domestic_victim_data$domestic <- domestic_victim_data$spouse + domestic_victim_data$boyfriend
  domestic_victim_data <- domestic_victim_data[domestic_victim_data$domestic == 1, ]
  
  data_subset <- domestic_victim_data[, c("ori")]
  domestic_victim_data <- domestic_victim_data[complete.cases(data_subset), ]
  
  domestic_victim_data$simpleassault <- ifelse(domestic_victim_data$ucr_offense_code_1 == "simple assault", 1, 0)
  domestic_victim_data$simpleassault[is.na(domestic_victim_data$simpleassault)] <- 0
  
  domestic_victim_data$intimidation <- ifelse(domestic_victim_data$ucr_offense_code_1 == "intimidation", 1, 0)
  domestic_victim_data$intimidation[is.na(domestic_victim_data$intimidation)] <- 0
  
  domestic_victim_data$aggravatedassault <- ifelse(domestic_victim_data$ucr_offense_code_1 == "aggravated assault", 1, 0)
  domestic_victim_data$aggravatedassault[is.na(domestic_victim_data$aggravatedassault)] <- 0
  
  domestic_victim_data <- domestic_victim_data %>%
    filter(simpleassault == 1 | intimidation == 1 | aggravatedassault == 1)%>%
    filter(sex_of_victim=="male")
  
  drops <- c("ucr_offense_code_6","ucr_offense_code_7","ucr_offense_code_8","ucr_offense_code_9","ucr_offense_code_10","offender_number_to_be_related1","offender_number_to_be_related2","offender_number_to_be_related3","offender_number_to_be_related4","offender_number_to_be_related5","offender_number_to_be_related6","offender_number_to_be_related7","offender_number_to_be_related8","offender_number_to_be_related9","offender_number_to_be_related10","relation_of_vict_to_offender1","relation_of_vict_to_offender2","relation_of_vict_to_offender3","relation_of_vict_to_offender4","relation_of_vict_to_offender5","relation_of_vict_to_offender6","relation_of_vict_to_offender7","relation_of_vict_to_offender8","relation_of_vict_to_offender9","relation_of_vict_to_offender10")
  domestic_victim_data <- domestic_victim_data[ , !(names(domestic_victim_data) %in% drops)]
  
  female_offender_data <- offender_data %>%
    filter(sex_of_offender=="female")
  
  merge1 <- merge(domestic_victim_data, administrative_data, by = c("unique_incident_id","ori","year","state","state_abb","incident_number","incident_date"))
  merge1 <- merge1[as.Date(merge1$incident_date) >= as.Date(paste0(year, "-01-01")) & as.Date(merge1$incident_date) <= as.Date(paste0(year, "-12-31")), ]
  workingdata <- merge(merge1, female_offender_data, by = c("unique_incident_id","ori","year","state","state_abb","incident_number","incident_date"))
  
  ##female victim and male offenders only ##
  # workingdata <- merge3[merge3$maleoffender==1 & merge3$femalevictim==1,]
  # workingdata <- workingdata %>%
  #   filter(simpleassault == 1 | intimidation == 1 | aggravatedassault == 1)
  
  ####shifting morning calls to the day before####
  
  workingdata$incident_date_shifted <- ifelse(workingdata$incident_date_hour == "on or between midnight and 00:59" | workingdata$incident_date_hour == "on or between 01:00 and 01:59" | workingdata$incident_date_hour == "on or between 02:00 and 02:59" | workingdata$incident_date_hour == "on or between 03:00 and 03:59" | workingdata$incident_date_hour == "on or between 04:00 and 04:59" | workingdata$incident_date_hour == "on or between 05:00 and 05:59",1,0)
  workingdata$incident_date <- as.Date(workingdata$incident_date)
  workingdata$incident_date <- if_else(workingdata$incident_date_shifted == 1, workingdata$incident_date - 1, workingdata$incident_date)
  workingdata$incident_date <- as.character(workingdata$incident_date)
  workingdata$after6andbefore6 <- ifelse(workingdata$incident_date_hour == "on or between 18:00 and 18:59" | workingdata$incident_date_hour == "on or between 19:00 and 19:59" | workingdata$incident_date_hour == "on or between 20:00 and 20:59" | workingdata$incident_date_hour == "on or between 21:00 and 21:59" | workingdata$incident_date_hour == "on or between 22:00 and 22:59" | workingdata$incident_date_hour == "on or between 23:00 and 23:59" | workingdata$incident_date_hour == "on or between midnight and 00:59" | workingdata$incident_date_hour == "on or between 01:00 and 01:59" | workingdata$incident_date_hour == "on or between 02:00 and 02:59" | workingdata$incident_date_hour == "on or between 03:00 and 03:59" | workingdata$incident_date_hour == "on or between 04:00 and 04:59" | workingdata$incident_date_hour == "on or between 05:00 and 05:59",1,0)
  
  workingdata$hour_7 <- ifelse(grepl("on or between 07:00 and 07:59", workingdata$incident_date_hour), 1, 0)
  workingdata$hour_8 <- ifelse(grepl("on or between 08:00 and 08:59", workingdata$incident_date_hour), 1, 0)
  workingdata$hour_9 <- ifelse(grepl("on or between 09:00 and 09:59", workingdata$incident_date_hour), 1, 0)
  workingdata$hour_10 <- ifelse(grepl("on or between 10:00 and 10:59", workingdata$incident_date_hour), 1, 0)
  workingdata$hour_11 <- ifelse(grepl("on or between 11:00 and 11:59", workingdata$incident_date_hour), 1, 0)
  workingdata$hour_12 <- ifelse(grepl("on or between 12:00 and 12:59", workingdata$incident_date_hour), 1, 0)
  workingdata$hour_13 <- ifelse(grepl("on or between 13:00 and 13:59", workingdata$incident_date_hour), 1, 0)
  workingdata$hour_14 <- ifelse(grepl("on or between 14:00 and 14:59", workingdata$incident_date_hour), 1, 0)
  workingdata$hour_15 <- ifelse(grepl("on or between 15:00 and 15:59", workingdata$incident_date_hour), 1, 0)
  workingdata$hour_16 <- ifelse(grepl("on or between 16:00 and 16:59", workingdata$incident_date_hour), 1, 0)
  workingdata$hour_17 <- ifelse(grepl("on or between 17:00 and 17:59", workingdata$incident_date_hour), 1, 0)
  workingdata$hour_18 <- ifelse(grepl("on or between 18:00 and 18:59", workingdata$incident_date_hour), 1, 0)
  workingdata$hour_19 <- ifelse(grepl("on or between 19:00 and 19:59", workingdata$incident_date_hour), 1, 0)
  workingdata$hour_20 <- ifelse(grepl("on or between 20:00 and 20:59", workingdata$incident_date_hour), 1, 0)
  workingdata$hour_21 <- ifelse(grepl("on or between 21:00 and 21:59", workingdata$incident_date_hour), 1, 0)
  workingdata$hour_22 <- ifelse(grepl("on or between 22:00 and 22:59", workingdata$incident_date_hour), 1, 0)
  workingdata$hour_23 <- ifelse(grepl("on or between 23:00 and 23:59", workingdata$incident_date_hour), 1, 0)
  workingdata$hour_0 <- ifelse(grepl("on or between 00:00 and 00:59", workingdata$incident_date_hour), 1, 0)
  workingdata$hour_1 <- ifelse(grepl("on or between 01:00 and 01:59", workingdata$incident_date_hour), 1, 0)
  workingdata$hour_2 <- ifelse(grepl("on or between 02:00 and 02:59", workingdata$incident_date_hour), 1, 0)
  workingdata$hour_3 <- ifelse(grepl("on or between 03:00 and 03:59", workingdata$incident_date_hour), 1, 0)
  workingdata$hour_4 <- ifelse(grepl("on or between 04:00 and 04:59", workingdata$incident_date_hour), 1, 0)
  workingdata$hour_5 <- ifelse(grepl("on or between 05:00 and 05:59", workingdata$incident_date_hour), 1, 0)
  workingdata$hour_6 <- ifelse(grepl("on or between 06:00 and 06:59", workingdata$incident_date_hour), 1, 0)
  
  dvcount_year <- workingdata %>%
    group_by(incident_date,ori) %>%
    summarize(,
              dv_hour_0 = sum(hour_0),
              dv_hour_1 = sum(hour_1),
              dv_hour_2 = sum(hour_2),
              dv_hour_3 = sum(hour_3),
              dv_hour_4 = sum(hour_4),
              dv_hour_5 = sum(hour_5),
              dv_hour_6 = sum(hour_6),
              dv_hour_7 = sum(hour_7),
              dv_hour_8 = sum(hour_8),
              dv_hour_9 = sum(hour_9),
              dv_hour_10 = sum(hour_10),
              dv_hour_11 = sum(hour_11),
              dv_hour_12 = sum(hour_12),
              dv_hour_13 = sum(hour_13),
              dv_hour_14 = sum(hour_14),
              dv_hour_15 = sum(hour_15),
              dv_hour_16 = sum(hour_16),
              dv_hour_17 = sum(hour_17),
              dv_hour_18 = sum(hour_18),
              dv_hour_19 = sum(hour_19),
              dv_hour_20 = sum(hour_20),
              dv_hour_21 = sum(hour_21),
              dv_hour_22 = sum(hour_22),
              dv_hour_23 = sum(hour_23)
    )
  
  
  # Store the results for the current year in the main data frame
  dvcount_year_less_zeros <- dvcount_year[!is.na(dvcount_year$incident_date), ]
  
  dvcount <- rbind(dvcount, dvcount_year_less_zeros)
  print(paste0("added data for year " , year))
}

# Save the final results as an RDS file
saveRDS(dvcount, file = "C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_domestic_counts_2009-2020_female_to_male.rds")
