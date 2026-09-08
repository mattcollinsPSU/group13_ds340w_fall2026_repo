library(dplyr)
library(lubridate)

rm(list = ls())
##2015##
##this section loads in the nibrs segments##
nibrs_administrative_segment_2015 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_1991_2020_administrative_segment_rds/nibrs_administrative_segment_2015.rds")
nibrs_offender_segment_2015 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_1991_2020_offender_segment_rds/nibrs_offender_segment_2015.rds")
nibrs_victim_segment_2015 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_1991_2020_victim_segment_rds/nibrs_victim_segment_2015.rds")
nibrs_batch_header_2015 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_batch_headers_rds/nibrs_batch_header_2015.rds")

##trying to reduce the size of the files to only domestic violence victims##
keeps <- c("ori","population","city_name","population_group","country_division","country_region","core_city","covered_by_ori")
nibrs_batch_header_2015 <- nibrs_batch_header_2015[keeps]
drops <- c("report_date_indicator","total_offense_segments","total_victim_segments","total_offender_segments","total_arrestee_segments","city_submissions","cleared_exceptionally","exceptional_clearance_date")
nibrs_administrative_segment_2015 <- nibrs_administrative_segment_2015[ , !(names(nibrs_administrative_segment_2015) %in% drops)]
nibrs_administrative_segment_2015 <- merge(nibrs_administrative_segment_2015, nibrs_batch_header_2015, by = "ori")
nibrs_domestic_victim_segment_2015 <- nibrs_victim_segment_2015
nibrs_domestic_victim_segment_2015$spouse <- ifelse(nibrs_domestic_victim_segment_2015$relation_of_vict_to_offender1 == "victim was spouse",1,0)
nibrs_domestic_victim_segment_2015$boyfriend <- ifelse(nibrs_domestic_victim_segment_2015$relation_of_vict_to_offender1 == "victim was boyfriend/girlfriend",1,0)
nibrs_domestic_victim_segment_2015$domestic <- nibrs_domestic_victim_segment_2015$spouse + nibrs_domestic_victim_segment_2015$boyfriend
nibrs_domestic_victim_segment_2015 <- nibrs_domestic_victim_segment_2015[nibrs_domestic_victim_segment_2015$domestic==1,]
data_subset <- nibrs_domestic_victim_segment_2015[ , c("ori")]
nibrs_domestic_victim_segment_2015 <- nibrs_domestic_victim_segment_2015[complete.cases(data_subset),]
nibrs_domestic_victim_segment_2015$simpleassault <- ifelse(nibrs_domestic_victim_segment_2015$ucr_offense_code_1 == "simple assault",1,0)
nibrs_domestic_victim_segment_2015$simpleassault[is.na(nibrs_domestic_victim_segment_2015$simpleassault)] <- 0
nibrs_domestic_victim_segment_2015$intimidation <- ifelse(nibrs_domestic_victim_segment_2015$ucr_offense_code_1 == "intimidation",1,0)
nibrs_domestic_victim_segment_2015$intimidation[is.na(nibrs_domestic_victim_segment_2015$intimidation)] <- 0
nibrs_domestic_victim_segment_2015$aggravatedassault <- ifelse(nibrs_domestic_victim_segment_2015$ucr_offense_code_1 == "aggravated assault",1,0)
nibrs_domestic_victim_segment_2015$aggravatedassault[is.na(nibrs_domestic_victim_segment_2015$aggravatedassault)] <- 0
drops <- c("ucr_offense_code_6","ucr_offense_code_7","ucr_offense_code_8","ucr_offense_code_9","ucr_offense_code_10","offender_number_to_be_related1","offender_number_to_be_related2","offender_number_to_be_related3","offender_number_to_be_related4","offender_number_to_be_related5","offender_number_to_be_related6","offender_number_to_be_related7","offender_number_to_be_related8","offender_number_to_be_related9","offender_number_to_be_related10","relation_of_vict_to_offender1","relation_of_vict_to_offender2","relation_of_vict_to_offender3","relation_of_vict_to_offender4","relation_of_vict_to_offender5","relation_of_vict_to_offender6","relation_of_vict_to_offender7","relation_of_vict_to_offender8","relation_of_vict_to_offender9","relation_of_vict_to_offender10")
nibrs_domestic_victim_segment_2015 <- nibrs_domestic_victim_segment_2015[ , !(names(nibrs_domestic_victim_segment_2015) %in% drops)]

##nibrs_domestic_victim_segment_2015$malevictim <- ifelse(nibrs_domestic_victim_segment_2015$sex_of_victim == "male", 1, 0)
nibrs_domestic_victim_segment_2015$femalevictim <- ifelse(nibrs_domestic_victim_segment_2015$sex_of_victim == "female", 1, 0)
##nibrs_domestic_victim_segment_2015$whitevictim <- ifelse(nibrs_domestic_victim_segment_2015$race_of_victim == "white",1,0)
##nibrs_domestic_victim_segment_2015$blackvictim <- ifelse(nibrs_domestic_victim_segment_2015$race_of_victim == "black",1,0)

nibrs_offender_segment_2015$maleoffender <- ifelse(nibrs_offender_segment_2015$sex_of_offender == "male",1,0)
##nibrs_offender_segment_2015$femaleoffender <- ifelse(nibrs_offender_segment_2015$sex_of_offender == "female",1,0)
##nibrs_offender_segment_2015$whiteoffender <- ifelse(nibrs_offender_segment_2015$race_of_offender == "white",1,0)
##nibrs_offender_segment_2015$blackoffender <- ifelse(nibrs_offender_segment_2015$race_of_offender == "black",1,0)


merge1_2015 <- merge(nibrs_domestic_victim_segment_2015,nibrs_administrative_segment_2015, by = c("unique_incident_id","ori","year","state","state_abb","incident_number","incident_date"))
merge1_2015 <- merge1_2015[merge1_2015$incident_date >= "2015-01-01" & merge1_2015$incident_date <= "2015-12-31",] 
merge2_2015 <- merge(merge1_2015,nibrs_offender_segment_2015, by = c("unique_incident_id","ori","year","state","state_abb","incident_number","incident_date"))
merge3_2015 <- merge2_2015[merge2_2015$domestic==1,]
summary(merge3_2015)
colnames(merge3_2015)

##female victim and male offenders only ##
workingdata_2015 <- merge3_2015[merge3_2015$maleoffender==1 & merge3_2015$femalevictim==1,]
summary(workingdata_2015)
table(workingdata_2015$incident_date_hour)

####shifting morning calls to the day before####

workingdata_2015$incident_date_shifted <- ifelse(workingdata_2015$incident_date_hour == "on or between midnight and 00:59" | workingdata_2015$incident_date_hour == "on or between 01:00 and 01:59" | workingdata_2015$incident_date_hour == "on or between 02:00 and 02:59" | workingdata_2015$incident_date_hour == "on or between 03:00 and 03:59" | workingdata_2015$incident_date_hour == "on or between 04:00 and 04:59" | workingdata_2015$incident_date_hour == "on or between 05:00 and 05:59",1,0)
workingdata_2015$incident_date <- as.Date(workingdata_2015$incident_date)
workingdata_2015$incident_date <- if_else(workingdata_2015$incident_date_shifted == 1, workingdata_2015$incident_date - 1, workingdata_2015$incident_date)
workingdata_2015$incident_date <- as.character(workingdata_2015$incident_date)
workingdata_2015$after6andbefore6 <- ifelse(workingdata_2015$incident_date_hour == "on or between 18:00 and 18:59" | workingdata_2015$incident_date_hour == "on or between 19:00 and 19:59" | workingdata_2015$incident_date_hour == "on or between 20:00 and 20:59" | workingdata_2015$incident_date_hour == "on or between 21:00 and 21:59" | workingdata_2015$incident_date_hour == "on or between 22:00 and 22:59" | workingdata_2015$incident_date_hour == "on or between 23:00 and 23:59" | workingdata_2015$incident_date_hour == "on or between midnight and 00:59" | workingdata_2015$incident_date_hour == "on or between 01:00 and 01:59" | workingdata_2015$incident_date_hour == "on or between 02:00 and 02:59" | workingdata_2015$incident_date_hour == "on or between 03:00 and 03:59" | workingdata_2015$incident_date_hour == "on or between 04:00 and 04:59" | workingdata_2015$incident_date_hour == "on or between 05:00 and 05:59",1,0)


dvcount2015 <- as.data.frame(table(ifelse(workingdata_2015$after6andbefore6==1,workingdata_2015$incident_date,0),workingdata_2015$ori))
colnames(dvcount2015)[1] = "incident_date"
colnames(dvcount2015)[2] = "ori"
colnames(dvcount2015)[3] = "oridvcount2015"
summary(dvcount2015)
dvcount2015lesszeros<-dvcount2015[!dvcount2015$incident_date==0,]
summary(dvcount2015lesszeros)
saveRDS(dvcount2015, file = "C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_domestic_2015.rds")

rm(list = ls())
##2016##
##this section loads in the nibrs segments##
nibrs_administrative_segment_2016 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_1991_2020_administrative_segment_rds/nibrs_administrative_segment_2016.rds")
nibrs_offender_segment_2016 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_1991_2020_offender_segment_rds/nibrs_offender_segment_2016.rds")
nibrs_victim_segment_2016 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_1991_2020_victim_segment_rds/nibrs_victim_segment_2016.rds")
nibrs_batch_header_2016 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_batch_headers_rds/nibrs_batch_header_2016.rds")

##trying to reduce the size of the files to only domestic violence victims##
keeps <- c("ori","population","city_name","population_group","country_division","country_region","core_city","covered_by_ori")
nibrs_batch_header_2016 <- nibrs_batch_header_2016[keeps]
drops <- c("report_date_indicator","total_offense_segments","total_victim_segments","total_offender_segments","total_arrestee_segments","city_submissions","cleared_exceptionally","exceptional_clearance_date")
nibrs_administrative_segment_2016 <- nibrs_administrative_segment_2016[ , !(names(nibrs_administrative_segment_2016) %in% drops)]
nibrs_administrative_segment_2016 <- merge(nibrs_administrative_segment_2016, nibrs_batch_header_2016, by = "ori")
nibrs_domestic_victim_segment_2016 <- nibrs_victim_segment_2016
nibrs_domestic_victim_segment_2016$spouse <- ifelse(nibrs_domestic_victim_segment_2016$relation_of_vict_to_offender1 == "victim was spouse",1,0)
nibrs_domestic_victim_segment_2016$boyfriend <- ifelse(nibrs_domestic_victim_segment_2016$relation_of_vict_to_offender1 == "victim was boyfriend/girlfriend",1,0)
nibrs_domestic_victim_segment_2016$domestic <- nibrs_domestic_victim_segment_2016$spouse + nibrs_domestic_victim_segment_2016$boyfriend
nibrs_domestic_victim_segment_2016 <- nibrs_domestic_victim_segment_2016[nibrs_domestic_victim_segment_2016$domestic==1,]
data_subset <- nibrs_domestic_victim_segment_2016[ , c("ori")]
nibrs_domestic_victim_segment_2016 <- nibrs_domestic_victim_segment_2016[complete.cases(data_subset),]
nibrs_domestic_victim_segment_2016$simpleassault <- ifelse(nibrs_domestic_victim_segment_2016$ucr_offense_code_1 == "simple assault",1,0)
nibrs_domestic_victim_segment_2016$simpleassault[is.na(nibrs_domestic_victim_segment_2016$simpleassault)] <- 0
nibrs_domestic_victim_segment_2016$intimidation <- ifelse(nibrs_domestic_victim_segment_2016$ucr_offense_code_1 == "intimidation",1,0)
nibrs_domestic_victim_segment_2016$intimidation[is.na(nibrs_domestic_victim_segment_2016$intimidation)] <- 0
nibrs_domestic_victim_segment_2016$aggravatedassault <- ifelse(nibrs_domestic_victim_segment_2016$ucr_offense_code_1 == "aggravated assault",1,0)
nibrs_domestic_victim_segment_2016$aggravatedassault[is.na(nibrs_domestic_victim_segment_2016$aggravatedassault)] <- 0
drops <- c("ucr_offense_code_6","ucr_offense_code_7","ucr_offense_code_8","ucr_offense_code_9","ucr_offense_code_10","offender_number_to_be_related1","offender_number_to_be_related2","offender_number_to_be_related3","offender_number_to_be_related4","offender_number_to_be_related5","offender_number_to_be_related6","offender_number_to_be_related7","offender_number_to_be_related8","offender_number_to_be_related9","offender_number_to_be_related10","relation_of_vict_to_offender1","relation_of_vict_to_offender2","relation_of_vict_to_offender3","relation_of_vict_to_offender4","relation_of_vict_to_offender5","relation_of_vict_to_offender6","relation_of_vict_to_offender7","relation_of_vict_to_offender8","relation_of_vict_to_offender9","relation_of_vict_to_offender10")
nibrs_domestic_victim_segment_2016 <- nibrs_domestic_victim_segment_2016[ , !(names(nibrs_domestic_victim_segment_2016) %in% drops)]

##nibrs_domestic_victim_segment_2016$malevictim <- ifelse(nibrs_domestic_victim_segment_2016$sex_of_victim == "male", 1, 0)
nibrs_domestic_victim_segment_2016$femalevictim <- ifelse(nibrs_domestic_victim_segment_2016$sex_of_victim == "female", 1, 0)
##nibrs_domestic_victim_segment_2016$whitevictim <- ifelse(nibrs_domestic_victim_segment_2016$race_of_victim == "white",1,0)
##nibrs_domestic_victim_segment_2016$blackvictim <- ifelse(nibrs_domestic_victim_segment_2016$race_of_victim == "black",1,0)

nibrs_offender_segment_2016$maleoffender <- ifelse(nibrs_offender_segment_2016$sex_of_offender == "male",1,0)
##nibrs_offender_segment_2016$femaleoffender <- ifelse(nibrs_offender_segment_2016$sex_of_offender == "female",1,0)
##nibrs_offender_segment_2016$whiteoffender <- ifelse(nibrs_offender_segment_2016$race_of_offender == "white",1,0)
##nibrs_offender_segment_2016$blackoffender <- ifelse(nibrs_offender_segment_2016$race_of_offender == "black",1,0)


merge1_2016 <- merge(nibrs_domestic_victim_segment_2016,nibrs_administrative_segment_2016, by = c("unique_incident_id","ori","year","state","state_abb","incident_number","incident_date"))
merge1_2016 <- merge1_2016[merge1_2016$incident_date >= "2016-01-01" & merge1_2016$incident_date <= "2016-12-31",] 
merge2_2016 <- merge(merge1_2016,nibrs_offender_segment_2016, by = c("unique_incident_id","ori","year","state","state_abb","incident_number","incident_date"))
merge3_2016 <- merge2_2016[merge2_2016$domestic==1,]
summary(merge3_2016)
colnames(merge3_2016)

##female victim and male offenders only ##
workingdata_2016 <- merge3_2016[merge3_2016$maleoffender==1 & merge3_2016$femalevictim==1,]
summary(workingdata_2016)
table(workingdata_2016$incident_date_hour)

####shifting morning calls to the day before####

workingdata_2016$incident_date_shifted <- ifelse(workingdata_2016$incident_date_hour == "on or between midnight and 00:59" | workingdata_2016$incident_date_hour == "on or between 01:00 and 01:59" | workingdata_2016$incident_date_hour == "on or between 02:00 and 02:59" | workingdata_2016$incident_date_hour == "on or between 03:00 and 03:59" | workingdata_2016$incident_date_hour == "on or between 04:00 and 04:59" | workingdata_2016$incident_date_hour == "on or between 05:00 and 05:59",1,0)
workingdata_2016$incident_date <- as.Date(workingdata_2016$incident_date)
workingdata_2016$incident_date <- if_else(workingdata_2016$incident_date_shifted == 1, workingdata_2016$incident_date - 1, workingdata_2016$incident_date)
workingdata_2016$incident_date <- as.character(workingdata_2016$incident_date)
workingdata_2016$after6andbefore6 <- ifelse(workingdata_2016$incident_date_hour == "on or between 18:00 and 18:59" | workingdata_2016$incident_date_hour == "on or between 19:00 and 19:59" | workingdata_2016$incident_date_hour == "on or between 20:00 and 20:59" | workingdata_2016$incident_date_hour == "on or between 21:00 and 21:59" | workingdata_2016$incident_date_hour == "on or between 22:00 and 22:59" | workingdata_2016$incident_date_hour == "on or between 23:00 and 23:59" | workingdata_2016$incident_date_hour == "on or between midnight and 00:59" | workingdata_2016$incident_date_hour == "on or between 01:00 and 01:59" | workingdata_2016$incident_date_hour == "on or between 02:00 and 02:59" | workingdata_2016$incident_date_hour == "on or between 03:00 and 03:59" | workingdata_2016$incident_date_hour == "on or between 04:00 and 04:59" | workingdata_2016$incident_date_hour == "on or between 05:00 and 05:59",1,0)


dvcount2016 <- as.data.frame(table(ifelse(workingdata_2016$after6andbefore6==1,workingdata_2016$incident_date,0),workingdata_2016$ori))
colnames(dvcount2016)[1] = "incident_date"
colnames(dvcount2016)[2] = "ori"
colnames(dvcount2016)[3] = "oridvcount2016"
summary(dvcount2016)
dvcount2016lesszeros<-dvcount2016[!dvcount2016$incident_date==0,]
summary(dvcount2016lesszeros)
saveRDS(dvcount2016, file = "C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_domestic_2016.rds")

rm(list = ls())
##2017##
##this section loads in the nibrs segments##
nibrs_administrative_segment_2017 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_1991_2020_administrative_segment_rds/nibrs_administrative_segment_2017.rds")
nibrs_offender_segment_2017 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_1991_2020_offender_segment_rds/nibrs_offender_segment_2017.rds")
nibrs_victim_segment_2017 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_1991_2020_victim_segment_rds/nibrs_victim_segment_2017.rds")
nibrs_batch_header_2017 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_batch_headers_rds/nibrs_batch_header_2017.rds")

##trying to reduce the size of the files to only domestic violence victims##
keeps <- c("ori","population","city_name","population_group","country_division","country_region","core_city","covered_by_ori")
nibrs_batch_header_2017 <- nibrs_batch_header_2017[keeps]
drops <- c("report_date_indicator","total_offense_segments","total_victim_segments","total_offender_segments","total_arrestee_segments","city_submissions","cleared_exceptionally","exceptional_clearance_date")
nibrs_administrative_segment_2017 <- nibrs_administrative_segment_2017[ , !(names(nibrs_administrative_segment_2017) %in% drops)]
nibrs_administrative_segment_2017 <- merge(nibrs_administrative_segment_2017, nibrs_batch_header_2017, by = "ori")
nibrs_domestic_victim_segment_2017 <- nibrs_victim_segment_2017
nibrs_domestic_victim_segment_2017$spouse <- ifelse(nibrs_domestic_victim_segment_2017$relation_of_vict_to_offender1 == "victim was spouse",1,0)
nibrs_domestic_victim_segment_2017$boyfriend <- ifelse(nibrs_domestic_victim_segment_2017$relation_of_vict_to_offender1 == "victim was boyfriend/girlfriend",1,0)
nibrs_domestic_victim_segment_2017$domestic <- nibrs_domestic_victim_segment_2017$spouse + nibrs_domestic_victim_segment_2017$boyfriend
nibrs_domestic_victim_segment_2017 <- nibrs_domestic_victim_segment_2017[nibrs_domestic_victim_segment_2017$domestic==1,]
data_subset <- nibrs_domestic_victim_segment_2017[ , c("ori")]
nibrs_domestic_victim_segment_2017 <- nibrs_domestic_victim_segment_2017[complete.cases(data_subset),]
nibrs_domestic_victim_segment_2017$simpleassault <- ifelse(nibrs_domestic_victim_segment_2017$ucr_offense_code_1 == "simple assault",1,0)
nibrs_domestic_victim_segment_2017$simpleassault[is.na(nibrs_domestic_victim_segment_2017$simpleassault)] <- 0
nibrs_domestic_victim_segment_2017$intimidation <- ifelse(nibrs_domestic_victim_segment_2017$ucr_offense_code_1 == "intimidation",1,0)
nibrs_domestic_victim_segment_2017$intimidation[is.na(nibrs_domestic_victim_segment_2017$intimidation)] <- 0
nibrs_domestic_victim_segment_2017$aggravatedassault <- ifelse(nibrs_domestic_victim_segment_2017$ucr_offense_code_1 == "aggravated assault",1,0)
nibrs_domestic_victim_segment_2017$aggravatedassault[is.na(nibrs_domestic_victim_segment_2017$aggravatedassault)] <- 0
drops <- c("ucr_offense_code_6","ucr_offense_code_7","ucr_offense_code_8","ucr_offense_code_9","ucr_offense_code_10","offender_number_to_be_related1","offender_number_to_be_related2","offender_number_to_be_related3","offender_number_to_be_related4","offender_number_to_be_related5","offender_number_to_be_related6","offender_number_to_be_related7","offender_number_to_be_related8","offender_number_to_be_related9","offender_number_to_be_related10","relation_of_vict_to_offender1","relation_of_vict_to_offender2","relation_of_vict_to_offender3","relation_of_vict_to_offender4","relation_of_vict_to_offender5","relation_of_vict_to_offender6","relation_of_vict_to_offender7","relation_of_vict_to_offender8","relation_of_vict_to_offender9","relation_of_vict_to_offender10")
nibrs_domestic_victim_segment_2017 <- nibrs_domestic_victim_segment_2017[ , !(names(nibrs_domestic_victim_segment_2017) %in% drops)]

##nibrs_domestic_victim_segment_2017$malevictim <- ifelse(nibrs_domestic_victim_segment_2017$sex_of_victim == "male", 1, 0)
nibrs_domestic_victim_segment_2017$femalevictim <- ifelse(nibrs_domestic_victim_segment_2017$sex_of_victim == "female", 1, 0)
##nibrs_domestic_victim_segment_2017$whitevictim <- ifelse(nibrs_domestic_victim_segment_2017$race_of_victim == "white",1,0)
##nibrs_domestic_victim_segment_2017$blackvictim <- ifelse(nibrs_domestic_victim_segment_2017$race_of_victim == "black",1,0)

nibrs_offender_segment_2017$maleoffender <- ifelse(nibrs_offender_segment_2017$sex_of_offender == "male",1,0)
##nibrs_offender_segment_2017$femaleoffender <- ifelse(nibrs_offender_segment_2017$sex_of_offender == "female",1,0)
##nibrs_offender_segment_2017$whiteoffender <- ifelse(nibrs_offender_segment_2017$race_of_offender == "white",1,0)
##nibrs_offender_segment_2017$blackoffender <- ifelse(nibrs_offender_segment_2017$race_of_offender == "black",1,0)


merge1_2017 <- merge(nibrs_domestic_victim_segment_2017,nibrs_administrative_segment_2017, by = c("unique_incident_id","ori","year","state","state_abb","incident_number","incident_date"))
merge1_2017 <- merge1_2017[merge1_2017$incident_date >= "2017-01-01" & merge1_2017$incident_date <= "2017-12-31",] 
merge2_2017 <- merge(merge1_2017,nibrs_offender_segment_2017, by = c("unique_incident_id","ori","year","state","state_abb","incident_number","incident_date"))
merge3_2017 <- merge2_2017[merge2_2017$domestic==1,]
summary(merge3_2017)
colnames(merge3_2017)

##female victim and male offenders only ##
workingdata_2017 <- merge3_2017[merge3_2017$maleoffender==1 & merge3_2017$femalevictim==1,]
summary(workingdata_2017)
table(workingdata_2017$incident_date_hour)

####shifting morning calls to the day before####

workingdata_2017$incident_date_shifted <- ifelse(workingdata_2017$incident_date_hour == "on or between midnight and 00:59" | workingdata_2017$incident_date_hour == "on or between 01:00 and 01:59" | workingdata_2017$incident_date_hour == "on or between 02:00 and 02:59" | workingdata_2017$incident_date_hour == "on or between 03:00 and 03:59" | workingdata_2017$incident_date_hour == "on or between 04:00 and 04:59" | workingdata_2017$incident_date_hour == "on or between 05:00 and 05:59",1,0)
workingdata_2017$incident_date <- as.Date(workingdata_2017$incident_date)
workingdata_2017$incident_date <- if_else(workingdata_2017$incident_date_shifted == 1, workingdata_2017$incident_date - 1, workingdata_2017$incident_date)
workingdata_2017$incident_date <- as.character(workingdata_2017$incident_date)
workingdata_2017$after6andbefore6 <- ifelse(workingdata_2017$incident_date_hour == "on or between 18:00 and 18:59" | workingdata_2017$incident_date_hour == "on or between 19:00 and 19:59" | workingdata_2017$incident_date_hour == "on or between 20:00 and 20:59" | workingdata_2017$incident_date_hour == "on or between 21:00 and 21:59" | workingdata_2017$incident_date_hour == "on or between 22:00 and 22:59" | workingdata_2017$incident_date_hour == "on or between 23:00 and 23:59" | workingdata_2017$incident_date_hour == "on or between midnight and 00:59" | workingdata_2017$incident_date_hour == "on or between 01:00 and 01:59" | workingdata_2017$incident_date_hour == "on or between 02:00 and 02:59" | workingdata_2017$incident_date_hour == "on or between 03:00 and 03:59" | workingdata_2017$incident_date_hour == "on or between 04:00 and 04:59" | workingdata_2017$incident_date_hour == "on or between 05:00 and 05:59",1,0)


dvcount2017 <- as.data.frame(table(ifelse(workingdata_2017$after6andbefore6==1,workingdata_2017$incident_date,0),workingdata_2017$ori))
colnames(dvcount2017)[1] = "incident_date"
colnames(dvcount2017)[2] = "ori"
colnames(dvcount2017)[3] = "oridvcount2017"
summary(dvcount2017)
dvcount2017lesszeros<-dvcount2017[!dvcount2017$incident_date==0,]
summary(dvcount2017lesszeros)
saveRDS(dvcount2017, file = "C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_domestic_2017.rds")

rm(list = ls())
##2018##
##this section loads in the nibrs segments##
nibrs_administrative_segment_2018 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_1991_2020_administrative_segment_rds/nibrs_administrative_segment_2018.rds")
nibrs_offender_segment_2018 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_1991_2020_offender_segment_rds/nibrs_offender_segment_2018.rds")
nibrs_victim_segment_2018 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_1991_2020_victim_segment_rds/nibrs_victim_segment_2018.rds")
nibrs_batch_header_2018 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_batch_headers_rds/nibrs_batch_header_2018.rds")

##trying to reduce the size of the files to only domestic violence victims##
keeps <- c("ori","population","city_name","population_group","country_division","country_region","core_city","covered_by_ori")
nibrs_batch_header_2018 <- nibrs_batch_header_2018[keeps]
drops <- c("report_date_indicator","total_offense_segments","total_victim_segments","total_offender_segments","total_arrestee_segments","city_submissions","cleared_exceptionally","exceptional_clearance_date")
nibrs_administrative_segment_2018 <- nibrs_administrative_segment_2018[ , !(names(nibrs_administrative_segment_2018) %in% drops)]
nibrs_administrative_segment_2018 <- merge(nibrs_administrative_segment_2018, nibrs_batch_header_2018, by = "ori")
nibrs_domestic_victim_segment_2018 <- nibrs_victim_segment_2018
nibrs_domestic_victim_segment_2018$spouse <- ifelse(nibrs_domestic_victim_segment_2018$relation_of_vict_to_offender1 == "victim was spouse",1,0)
nibrs_domestic_victim_segment_2018$boyfriend <- ifelse(nibrs_domestic_victim_segment_2018$relation_of_vict_to_offender1 == "victim was boyfriend/girlfriend",1,0)
nibrs_domestic_victim_segment_2018$domestic <- nibrs_domestic_victim_segment_2018$spouse + nibrs_domestic_victim_segment_2018$boyfriend
nibrs_domestic_victim_segment_2018 <- nibrs_domestic_victim_segment_2018[nibrs_domestic_victim_segment_2018$domestic==1,]
data_subset <- nibrs_domestic_victim_segment_2018[ , c("ori")]
nibrs_domestic_victim_segment_2018 <- nibrs_domestic_victim_segment_2018[complete.cases(data_subset),]
nibrs_domestic_victim_segment_2018$simpleassault <- ifelse(nibrs_domestic_victim_segment_2018$ucr_offense_code_1 == "simple assault",1,0)
nibrs_domestic_victim_segment_2018$simpleassault[is.na(nibrs_domestic_victim_segment_2018$simpleassault)] <- 0
nibrs_domestic_victim_segment_2018$intimidation <- ifelse(nibrs_domestic_victim_segment_2018$ucr_offense_code_1 == "intimidation",1,0)
nibrs_domestic_victim_segment_2018$intimidation[is.na(nibrs_domestic_victim_segment_2018$intimidation)] <- 0
nibrs_domestic_victim_segment_2018$aggravatedassault <- ifelse(nibrs_domestic_victim_segment_2018$ucr_offense_code_1 == "aggravated assault",1,0)
nibrs_domestic_victim_segment_2018$aggravatedassault[is.na(nibrs_domestic_victim_segment_2018$aggravatedassault)] <- 0
drops <- c("ucr_offense_code_6","ucr_offense_code_7","ucr_offense_code_8","ucr_offense_code_9","ucr_offense_code_10","offender_number_to_be_related1","offender_number_to_be_related2","offender_number_to_be_related3","offender_number_to_be_related4","offender_number_to_be_related5","offender_number_to_be_related6","offender_number_to_be_related7","offender_number_to_be_related8","offender_number_to_be_related9","offender_number_to_be_related10","relation_of_vict_to_offender1","relation_of_vict_to_offender2","relation_of_vict_to_offender3","relation_of_vict_to_offender4","relation_of_vict_to_offender5","relation_of_vict_to_offender6","relation_of_vict_to_offender7","relation_of_vict_to_offender8","relation_of_vict_to_offender9","relation_of_vict_to_offender10")
nibrs_domestic_victim_segment_2018 <- nibrs_domestic_victim_segment_2018[ , !(names(nibrs_domestic_victim_segment_2018) %in% drops)]

##nibrs_domestic_victim_segment_2018$malevictim <- ifelse(nibrs_domestic_victim_segment_2018$sex_of_victim == "male", 1, 0)
nibrs_domestic_victim_segment_2018$femalevictim <- ifelse(nibrs_domestic_victim_segment_2018$sex_of_victim == "female", 1, 0)
##nibrs_domestic_victim_segment_2018$whitevictim <- ifelse(nibrs_domestic_victim_segment_2018$race_of_victim == "white",1,0)
##nibrs_domestic_victim_segment_2018$blackvictim <- ifelse(nibrs_domestic_victim_segment_2018$race_of_victim == "black",1,0)

nibrs_offender_segment_2018$maleoffender <- ifelse(nibrs_offender_segment_2018$sex_of_offender == "male",1,0)
##nibrs_offender_segment_2018$femaleoffender <- ifelse(nibrs_offender_segment_2018$sex_of_offender == "female",1,0)
##nibrs_offender_segment_2018$whiteoffender <- ifelse(nibrs_offender_segment_2018$race_of_offender == "white",1,0)
##nibrs_offender_segment_2018$blackoffender <- ifelse(nibrs_offender_segment_2018$race_of_offender == "black",1,0)


merge1_2018 <- merge(nibrs_domestic_victim_segment_2018,nibrs_administrative_segment_2018, by = c("unique_incident_id","ori","year","state","state_abb","incident_number","incident_date"))
merge1_2018 <- merge1_2018[merge1_2018$incident_date >= "2018-01-01" & merge1_2018$incident_date <= "2018-12-31",] 
merge2_2018 <- merge(merge1_2018,nibrs_offender_segment_2018, by = c("unique_incident_id","ori","year","state","state_abb","incident_number","incident_date"))
merge3_2018 <- merge2_2018[merge2_2018$domestic==1,]
summary(merge3_2018)
colnames(merge3_2018)

##female victim and male offenders only ##
workingdata_2018 <- merge3_2018[merge3_2018$maleoffender==1 & merge3_2018$femalevictim==1,]
summary(workingdata_2018)
table(workingdata_2018$incident_date_hour)

####shifting morning calls to the day before####

workingdata_2018$incident_date_shifted <- ifelse(workingdata_2018$incident_date_hour == "on or between midnight and 00:59" | workingdata_2018$incident_date_hour == "on or between 01:00 and 01:59" | workingdata_2018$incident_date_hour == "on or between 02:00 and 02:59" | workingdata_2018$incident_date_hour == "on or between 03:00 and 03:59" | workingdata_2018$incident_date_hour == "on or between 04:00 and 04:59" | workingdata_2018$incident_date_hour == "on or between 05:00 and 05:59",1,0)
workingdata_2018$incident_date <- as.Date(workingdata_2018$incident_date)
workingdata_2018$incident_date <- if_else(workingdata_2018$incident_date_shifted == 1, workingdata_2018$incident_date - 1, workingdata_2018$incident_date)
workingdata_2018$incident_date <- as.character(workingdata_2018$incident_date)
workingdata_2018$after6andbefore6 <- ifelse(workingdata_2018$incident_date_hour == "on or between 18:00 and 18:59" | workingdata_2018$incident_date_hour == "on or between 19:00 and 19:59" | workingdata_2018$incident_date_hour == "on or between 20:00 and 20:59" | workingdata_2018$incident_date_hour == "on or between 21:00 and 21:59" | workingdata_2018$incident_date_hour == "on or between 22:00 and 22:59" | workingdata_2018$incident_date_hour == "on or between 23:00 and 23:59" | workingdata_2018$incident_date_hour == "on or between midnight and 00:59" | workingdata_2018$incident_date_hour == "on or between 01:00 and 01:59" | workingdata_2018$incident_date_hour == "on or between 02:00 and 02:59" | workingdata_2018$incident_date_hour == "on or between 03:00 and 03:59" | workingdata_2018$incident_date_hour == "on or between 04:00 and 04:59" | workingdata_2018$incident_date_hour == "on or between 05:00 and 05:59",1,0)


dvcount2018 <- as.data.frame(table(ifelse(workingdata_2018$after6andbefore6==1,workingdata_2018$incident_date,0),workingdata_2018$ori))
colnames(dvcount2018)[1] = "incident_date"
colnames(dvcount2018)[2] = "ori"
colnames(dvcount2018)[3] = "oridvcount2018"
summary(dvcount2018)
dvcount2018lesszeros<-dvcount2018[!dvcount2018$incident_date==0,]
summary(dvcount2018lesszeros)
saveRDS(dvcount2018, file = "C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_domestic_2018.rds")

rm(list = ls())
##2019##
##this section loads in the nibrs segments##
nibrs_administrative_segment_2019 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_1991_2020_administrative_segment_rds/nibrs_administrative_segment_2019.rds")
nibrs_offender_segment_2019 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_1991_2020_offender_segment_rds/nibrs_offender_segment_2019.rds")
nibrs_victim_segment_2019 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_1991_2020_victim_segment_rds/nibrs_victim_segment_2019.rds")
nibrs_batch_header_2019 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_batch_headers_rds/nibrs_batch_header_2019.rds")

##trying to reduce the size of the files to only domestic violence victims##
keeps <- c("ori","population","city_name","population_group","country_division","country_region","core_city","covered_by_ori")
nibrs_batch_header_2019 <- nibrs_batch_header_2019[keeps]
drops <- c("report_date_indicator","total_offense_segments","total_victim_segments","total_offender_segments","total_arrestee_segments","city_submissions","cleared_exceptionally","exceptional_clearance_date")
nibrs_administrative_segment_2019 <- nibrs_administrative_segment_2019[ , !(names(nibrs_administrative_segment_2019) %in% drops)]
nibrs_administrative_segment_2019 <- merge(nibrs_administrative_segment_2019, nibrs_batch_header_2019, by = "ori")
nibrs_domestic_victim_segment_2019 <- nibrs_victim_segment_2019
nibrs_domestic_victim_segment_2019$spouse <- ifelse(nibrs_domestic_victim_segment_2019$relation_of_vict_to_offender1 == "victim was spouse",1,0)
nibrs_domestic_victim_segment_2019$boyfriend <- ifelse(nibrs_domestic_victim_segment_2019$relation_of_vict_to_offender1 == "victim was boyfriend/girlfriend",1,0)
nibrs_domestic_victim_segment_2019$domestic <- nibrs_domestic_victim_segment_2019$spouse + nibrs_domestic_victim_segment_2019$boyfriend
nibrs_domestic_victim_segment_2019 <- nibrs_domestic_victim_segment_2019[nibrs_domestic_victim_segment_2019$domestic==1,]
data_subset <- nibrs_domestic_victim_segment_2019[ , c("ori")]
nibrs_domestic_victim_segment_2019 <- nibrs_domestic_victim_segment_2019[complete.cases(data_subset),]
nibrs_domestic_victim_segment_2019$simpleassault <- ifelse(nibrs_domestic_victim_segment_2019$ucr_offense_code_1 == "simple assault",1,0)
nibrs_domestic_victim_segment_2019$simpleassault[is.na(nibrs_domestic_victim_segment_2019$simpleassault)] <- 0
nibrs_domestic_victim_segment_2019$intimidation <- ifelse(nibrs_domestic_victim_segment_2019$ucr_offense_code_1 == "intimidation",1,0)
nibrs_domestic_victim_segment_2019$intimidation[is.na(nibrs_domestic_victim_segment_2019$intimidation)] <- 0
nibrs_domestic_victim_segment_2019$aggravatedassault <- ifelse(nibrs_domestic_victim_segment_2019$ucr_offense_code_1 == "aggravated assault",1,0)
nibrs_domestic_victim_segment_2019$aggravatedassault[is.na(nibrs_domestic_victim_segment_2019$aggravatedassault)] <- 0
drops <- c("ucr_offense_code_6","ucr_offense_code_7","ucr_offense_code_8","ucr_offense_code_9","ucr_offense_code_10","offender_number_to_be_related1","offender_number_to_be_related2","offender_number_to_be_related3","offender_number_to_be_related4","offender_number_to_be_related5","offender_number_to_be_related6","offender_number_to_be_related7","offender_number_to_be_related8","offender_number_to_be_related9","offender_number_to_be_related10","relation_of_vict_to_offender1","relation_of_vict_to_offender2","relation_of_vict_to_offender3","relation_of_vict_to_offender4","relation_of_vict_to_offender5","relation_of_vict_to_offender6","relation_of_vict_to_offender7","relation_of_vict_to_offender8","relation_of_vict_to_offender9","relation_of_vict_to_offender10")
nibrs_domestic_victim_segment_2019 <- nibrs_domestic_victim_segment_2019[ , !(names(nibrs_domestic_victim_segment_2019) %in% drops)]

##nibrs_domestic_victim_segment_2019$malevictim <- ifelse(nibrs_domestic_victim_segment_2019$sex_of_victim == "male", 1, 0)
nibrs_domestic_victim_segment_2019$femalevictim <- ifelse(nibrs_domestic_victim_segment_2019$sex_of_victim == "female", 1, 0)
##nibrs_domestic_victim_segment_2019$whitevictim <- ifelse(nibrs_domestic_victim_segment_2019$race_of_victim == "white",1,0)
##nibrs_domestic_victim_segment_2019$blackvictim <- ifelse(nibrs_domestic_victim_segment_2019$race_of_victim == "black",1,0)

nibrs_offender_segment_2019$maleoffender <- ifelse(nibrs_offender_segment_2019$sex_of_offender == "male",1,0)
##nibrs_offender_segment_2019$femaleoffender <- ifelse(nibrs_offender_segment_2019$sex_of_offender == "female",1,0)
##nibrs_offender_segment_2019$whiteoffender <- ifelse(nibrs_offender_segment_2019$race_of_offender == "white",1,0)
##nibrs_offender_segment_2019$blackoffender <- ifelse(nibrs_offender_segment_2019$race_of_offender == "black",1,0)


merge1_2019 <- merge(nibrs_domestic_victim_segment_2019,nibrs_administrative_segment_2019, by = c("unique_incident_id","ori","year","state","state_abb","incident_number","incident_date"))
merge1_2019 <- merge1_2019[merge1_2019$incident_date >= "2019-01-01" & merge1_2019$incident_date <= "2019-12-31",] 
merge2_2019 <- merge(merge1_2019,nibrs_offender_segment_2019, by = c("unique_incident_id","ori","year","state","state_abb","incident_number","incident_date"))
merge3_2019 <- merge2_2019[merge2_2019$domestic==1,]
summary(merge3_2019)
colnames(merge3_2019)

##female victim and male offenders only ##
workingdata_2019 <- merge3_2019[merge3_2019$maleoffender==1 & merge3_2019$femalevictim==1,]
summary(workingdata_2019)
table(workingdata_2019$incident_date_hour)

####shifting morning calls to the day before####

workingdata_2019$incident_date_shifted <- ifelse(workingdata_2019$incident_date_hour == "on or between midnight and 00:59" | workingdata_2019$incident_date_hour == "on or between 01:00 and 01:59" | workingdata_2019$incident_date_hour == "on or between 02:00 and 02:59" | workingdata_2019$incident_date_hour == "on or between 03:00 and 03:59" | workingdata_2019$incident_date_hour == "on or between 04:00 and 04:59" | workingdata_2019$incident_date_hour == "on or between 05:00 and 05:59",1,0)
workingdata_2019$incident_date <- as.Date(workingdata_2019$incident_date)
workingdata_2019$incident_date <- if_else(workingdata_2019$incident_date_shifted == 1, workingdata_2019$incident_date - 1, workingdata_2019$incident_date)
workingdata_2019$incident_date <- as.character(workingdata_2019$incident_date)
workingdata_2019$after6andbefore6 <- ifelse(workingdata_2019$incident_date_hour == "on or between 18:00 and 18:59" | workingdata_2019$incident_date_hour == "on or between 19:00 and 19:59" | workingdata_2019$incident_date_hour == "on or between 20:00 and 20:59" | workingdata_2019$incident_date_hour == "on or between 21:00 and 21:59" | workingdata_2019$incident_date_hour == "on or between 22:00 and 22:59" | workingdata_2019$incident_date_hour == "on or between 23:00 and 23:59" | workingdata_2019$incident_date_hour == "on or between midnight and 00:59" | workingdata_2019$incident_date_hour == "on or between 01:00 and 01:59" | workingdata_2019$incident_date_hour == "on or between 02:00 and 02:59" | workingdata_2019$incident_date_hour == "on or between 03:00 and 03:59" | workingdata_2019$incident_date_hour == "on or between 04:00 and 04:59" | workingdata_2019$incident_date_hour == "on or between 05:00 and 05:59",1,0)


dvcount2019 <- as.data.frame(table(ifelse(workingdata_2019$after6andbefore6==1,workingdata_2019$incident_date,0),workingdata_2019$ori))
colnames(dvcount2019)[1] = "incident_date"
colnames(dvcount2019)[2] = "ori"
colnames(dvcount2019)[3] = "oridvcount2019"
summary(dvcount2019)
dvcount2019lesszeros<-dvcount2019[!dvcount2019$incident_date==0,]
summary(dvcount2019lesszeros)
saveRDS(dvcount2019, file = "C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_domestic_2019.rds")
rm(list = ls())


dvcount2015 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_domestic_2015.rds")
colnames(dvcount2015)[1] = "incident_date"
colnames(dvcount2015)[2] = "ori"
colnames(dvcount2015)[3] = "dvcount"
dvcount2016 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_domestic_2016.rds")
colnames(dvcount2016)[1] = "incident_date"
colnames(dvcount2016)[2] = "ori"
colnames(dvcount2016)[3] = "dvcount"
dvcount2017 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_domestic_2017.rds")
colnames(dvcount2017)[1] = "incident_date"
colnames(dvcount2017)[2] = "ori"
colnames(dvcount2017)[3] = "dvcount"
dvcount2018 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_domestic_2018.rds")
colnames(dvcount2018)[1] = "incident_date"
colnames(dvcount2018)[2] = "ori"
colnames(dvcount2018)[3] = "dvcount"
dvcount2019 <- readRDS("C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/nibrs_domestic_2019.rds")
colnames(dvcount2019)[1] = "incident_date"
colnames(dvcount2019)[2] = "ori"
colnames(dvcount2019)[3] = "dvcount"

dvcount_main <- rbind(dvcount2015, dvcount2016, dvcount2017, dvcount2018, dvcount2019)
saveRDS(dvcount_main, file = "C:/Users/Kal/Desktop/Health/MLB_Domestic_Violence/Data/dvcount_main.rds")

