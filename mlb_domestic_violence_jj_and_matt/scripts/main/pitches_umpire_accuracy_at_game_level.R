rm(list = ls())
library(dplyr)
library(modelsummary)
library(baseballr)
library(lubridate)

all_years_combined <- data.frame()
game_data <- get_game_info_sup_petti()

for (year in 2009:2019) {
  pitch_data <- read.csv(paste0("C:\\Users\\Kal\\Desktop\\Health\\MLB_Domestic_Violence\\Data\\mlb\\ScrapedPitches\\pitch_data_", year, ".csv")) %>%
    select(c("game_date", "description", "game_type", "home_team", "away_team", "type", "balls", "strikes", "plate_x", "plate_z", "on_3b", "on_2b", "on_1b", "outs_when_up", "inning", "inning_topbot", "sz_top", "sz_bot", "game_pk", "at_bat_number", "pitch_number", "home_score", "away_score", "bat_score", "fld_score", "post_home_score", "post_away_score", "post_bat_score", "post_fld_score", "delta_home_win_exp", "delta_run_exp"))
  mlb_pitches <- pitch_data[!pitch_data$type == "X", ]
  mlb_pitches <- mlb_pitches[mlb_pitches$description %in% c("ball", "called_strike", "blocked_ball"), ]
  
  #mlb_pitches <- mlb_pitches[mlb_pitches$home_team  %in% c("CIN","CLE","COL","DET","KC","MIL","MIN","SEA","TEX") | mlb_pitches$away_team %in% c("CIN","CLE","COL","DET","KC","MIL","MIN","SEA","TEX"),]
  #mlb_pitches <- mlb_pitches[month(mlb_pitches$game_date)!=3,]
  
  mlb_pitches <- mlb_pitches %>%
    mutate(on1 = if_else(!is.na(on_1b), 1, 0),
           on2 = if_else(!is.na(on_2b), 1, 0),
           on3 = if_else(!is.na(on_3b), 1, 0))
  mlb_pitches <- mlb_pitches %>%
    select(-on_3b, -on_2b, -on_1b)
  
  situation_table <- aggregate(delta_run_exp ~ balls + strikes + type + on1 + on2 + on3 + outs_when_up, data = mlb_pitches, FUN = mean)
  
  # strike zone is 17 inches wide, ball is 3 inches wide. call is made from center of ball and 0 is center of plate. 8.5/12=.71 1.5/12 = .125 .71+.125 = .835
  x_width <- 0.835
  
  mlb_pitches$correctcall <- ifelse(
    mlb_pitches$plate_x > x_width | mlb_pitches$plate_x < -x_width, "B",
    ifelse(
      mlb_pitches$plate_x > -x_width & mlb_pitches$plate_x < x_width &
        mlb_pitches$plate_z > mlb_pitches$sz_bot &
        mlb_pitches$plate_z < mlb_pitches$sz_top, "S", "B"
    )
  )
  
  # renaming the situation table so I can merge it into the mlb_pitches data
  situation_table <- situation_table %>%
    rename(correctcall = type, correct_delta_run_exp = delta_run_exp)
  
  ## this removed anyone without a strike zone or a delta_run_expectancy
  merged_data <- merge(mlb_pitches, situation_table, by = c("balls", "strikes", "on1", "on2", "on3", "outs_when_up", "correctcall"), all.x = TRUE)
  merged_data_clean <- merged_data[complete.cases(merged_data$delta_run_exp, merged_data$correct_delta_run_exp), ]
  #share_clean <- nrow(merged_data_clean) / nrow(merged_data) * 100
  #print(paste("Share of pitches missing for year", year, ":", 100-round(share_clean, 2), "%"))
  merged_data_clean$homefavor <- merged_data_clean$delta_run_exp - merged_data_clean$correct_delta_run_exp
  
  ## creates a variable for whether a call favors the home or away team. if its positive, it favors home. if its negative, it favors away.
  merged_data_clean$homefavor <- ifelse(merged_data_clean$inning_topbot == "Bot", merged_data_clean$homefavor, -merged_data_clean$homefavor)
  
  # Aggregating the effects to the game level
  aggregated_data <- merged_data_clean %>%
    mutate(accurate = ifelse(correctcall == type,1,0)) %>%
    group_by(game_pk, game_date, game_type, home_team, away_team) %>%
    summarize(
      homefavor = sum(homefavor),
      accuracy = mean(accurate)
    )
  
  aggregated_game_data <- merge(aggregated_data, game_data, by = "game_pk")
  
  unique_dates <- unique(aggregated_game_data$game_date.x)  # Get unique dates from the test variable
  
  # Create an empty dataframe for the aggregated data
  game_pk_data <- data.frame()
  
  # Loop through each unique date
  for (date in unique_dates) {
    # Get the corresponding game_pks_date dataframe
    game_pks_date <- mlb_game_pks(date)
    
    # Remove columns from game_pks_date dataframe
    game_pks_date_filtered <- game_pks_date %>%
      select(game_pk, isTie, gameNumber, publicFacing, doubleHeader, gamedayType, tiebreaker, dayNight, scheduledInnings, reverseHomeAwayStatus, gamesInSeries, seriesGameNumber, seriesDescription, ifNecessaryDescription, teams.away.score, teams.away.leagueRecord.pct, teams.home.score, teams.home.leagueRecord.pct, status.codedGameState, status.detailedState)  # Specify the columns to be removed
    
    # Append the filtered data to the aggregated_data dataframe
    game_pk_data <- bind_rows(game_pk_data, game_pks_date_filtered)
  }
  
  broughttogether <- merge(aggregated_game_data, game_pk_data, by = "game_pk")
  broughttogether <- broughttogether[broughttogether$status.codedGameState == "F", ]
  
  unique_broughttogether <- broughttogether[!duplicated(broughttogether$game_pk), ]
  
  unique_broughttogether$homewin <- ifelse(unique_broughttogether$teams.home.score - unique_broughttogether$teams.away.score > 0 & unique_broughttogether$teams.home.score - unique_broughttogether$teams.away.score != 0, 1, 0)
  unique_broughttogether$awaywin <- ifelse(unique_broughttogether$teams.home.score - unique_broughttogether$teams.away.score < 0 & unique_broughttogether$teams.home.score - unique_broughttogether$teams.away.score != 0, 1, 0)
  
  unique_broughttogether$umpirechangedoutcome <- ifelse(unique_broughttogether$teams.home.score + unique_broughttogether$homefavor - unique_broughttogether$teams.away.score > 0 & unique_broughttogether$homewin == 0 | unique_broughttogether$teams.home.score + unique_broughttogether$homefavor - unique_broughttogether$teams.away.score < 0 & unique_broughttogether$homewin == 1, 1, 0)
  
  all_years_combined <- rbind(all_years_combined,unique_broughttogether)
  
  print(paste0("adding in ",year, " game data"))
}

all_years_combined_clean <- all_years_combined %>%
  select(-game_id, -home_sport_code, -official_scorer, -date, -gameday_sw, -game_date.x, -game_type.x) %>%
  rename(game_date = game_date.y,
         game_type = game_type.y)

saveRDS(all_years_combined_clean,"C:\\Users\\Kal\\Desktop\\Health\\MLB_Domestic_Violence\\Data\\mlb\\game_data_2009-2019.rds")


