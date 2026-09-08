
# Script Information
# ------------------------------------------------------------------------------
# This script file was written by Kalvin Mudrow on 11/7/2025
# This script executes the project's main results and robustness checks
# Email the author at: kalvin.mudrow@gcsu.edu


# Load Libraries
# ------------------------------------------------------------------------------
rm(list = ls())
library(dplyr)
library(tidyr)
library(modelsummary)
library(lubridate)
library(car)
library(ggplot2)
library(fixest)
options(modelsummary_factory_default = "kableExtra")
options(modelsummary_factory_latex = 'kableExtra')


# Load mlb game data
# ------------------------------------------------------------------------------
mlb_games <- readRDS("data\\processed\\game_data_2009-2019.rds")

mlb_games_home <- mlb_games[, !names(mlb_games) %in% "away_team"]
mlb_games_home$hometeam <- 1
mlb_games_home <- mlb_games_home %>%
  rename(team = home_team)

mlb_games_away <- mlb_games[, !names(mlb_games) %in% "home_team"]
mlb_games_away$hometeam <- 0
mlb_games_away <- mlb_games_away %>%
  rename(team = away_team)

mlb_games_stacked <- rbind(mlb_games_home, mlb_games_away)

mlb_games_stacked$ori <- ifelse(mlb_games_stacked$team == "ATL", "GAAPD0000",
                                ifelse(mlb_games_stacked$team == "AZ", "AZ0072300",
                                       ifelse(mlb_games_stacked$team == "BAL", "MDBPD0000",
                                              ifelse(mlb_games_stacked$team == "BOS", "MA0130100",
                                                     ifelse(mlb_games_stacked$team == "CHC", "ILCPD0000",
                                                            ifelse(mlb_games_stacked$team == "CIN", "OHCIP0000",
                                                                   ifelse(mlb_games_stacked$team == "CLE", "OHCLP0000",
                                                                          ifelse(mlb_games_stacked$team == "COL", "CODPD0000",
                                                                                 ifelse(mlb_games_stacked$team == "CWS", "ILCPD0000",
                                                                                        ifelse(mlb_games_stacked$team == "DET", "MI8234900",
                                                                                               ifelse(mlb_games_stacked$team == "HOU", "TXHPD0000",
                                                                                                      ifelse(mlb_games_stacked$team == "KC", "MOKPD0000",
                                                                                                             ifelse(mlb_games_stacked$team == "LAA", "CA0300100",
                                                                                                                    ifelse(mlb_games_stacked$team == "LAD", "CA0194200",
                                                                                                                           ifelse(mlb_games_stacked$team == "MIA", "FL0130600",
                                                                                                                                  ifelse(mlb_games_stacked$team == "MIL", "WIMPD0000",
                                                                                                                                         ifelse(mlb_games_stacked$team == "MIN", "MN0271100",
                                                                                                                                                ifelse(mlb_games_stacked$team == "NYM", "NY0303000",
                                                                                                                                                       ifelse(mlb_games_stacked$team == "NYY", "NY0303000",
                                                                                                                                                              ifelse(mlb_games_stacked$team == "OAK", "CA0010900",
                                                                                                                                                                     ifelse(mlb_games_stacked$team == "PHI", "PAPEP0000",
                                                                                                                                                                            ifelse(mlb_games_stacked$team == "PIT", "PAPPD0000",
                                                                                                                                                                                   ifelse(mlb_games_stacked$team == "SD", "CA0371100",
                                                                                                                                                                                          ifelse(mlb_games_stacked$team == "SEA", "WASPD0000",
                                                                                                                                                                                                 ifelse(mlb_games_stacked$team == "SF", "CA0380100",
                                                                                                                                                                                                        ifelse(mlb_games_stacked$team == "STL", "MOSPD0000",
                                                                                                                                                                                                               ifelse(mlb_games_stacked$team == "TB", "FL0521400",
                                                                                                                                                                                                                      ifelse(mlb_games_stacked$team == "TEX", "TX2200100",
                                                                                                                                                                                                                             ifelse(mlb_games_stacked$team == "TOR", "Canada",
                                                                                                                                                                                                                                    ifelse(mlb_games_stacked$team == "WSH", "DCMPD0000", 0))))))))))))))))))))))))))))))


##cut## team_abbreviations <- c("ATL", "AZ", "BAL", "BOS", "CHC", "CIN", "CLE", "COL", "CWS", "DET", "HOU", "KC", "LAA", "LAD", "MIA", "MIL", "MIN", "NYM", "NYY", "OAK", "PHI", "PIT", "SD", "SEA", "SF", "STL", "TB", "TEX", "TOR", "WSH")
##cut## mlb_games_subset <- mlb_games_stacked[mlb_games_stacked$team %in% team_abbreviations, ]
mlb_games_stacked$incident_date = mlb_games_stacked$game_date

# Excluding boston because it only starts reporting in september 2019 and houston because they report so many zeros
# ------------------------------------------------------------------------------
mlb_games_subset <- mlb_games_stacked[complete.cases(mlb_games_stacked$attendance), ]

# Loading DV data
# ------------------------------------------------------------------------------
dvcount_main <- readRDS("data\\processed\\nibrs_domestic_counts_2009-2020.rds")


# Merging MLB data to DV data
# ------------------------------------------------------------------------------
mlb_dv <- merge(mlb_games_subset, dvcount_main, by = c("incident_date", "ori"), all.x = TRUE) %>%
  filter(team %in% c("CIN", "CLE", "COL", "DET", "KC", "MIL", "MIN", "SEA", "TEX")) %>%
  filter(!(team == "TEX" & incident_date < as.Date("2017-01-01"))) %>%
  filter(!(team == "KC" & incident_date < as.Date("2010-01-01"))) %>%
  filter(!(team == "MIN" & incident_date < as.Date("2019-01-01")))%>%
  filter(!(team == "SEA" & incident_date < as.Date("2012-01-01")))%>%
  mutate(across(c(dv_hour_0:dv_hour_23), ~replace_na(.x, 0)))

# Extracting time-related features from MLB data
# ------------------------------------------------------------------------------
# Day of the week for each game (labelled as Mon, Tue, etc.)
mlb_dv$dayofweek <- wday(mlb_dv$incident_date, label = TRUE)
# Optional: convert to factor (commented out)
# mlb_dv$dayofweek <- as.factor(mlb_dv$dayofweek)

# Month and year of each game
mlb_dv$month <- month(mlb_dv$incident_date)
# Optional numeric month-year (commented out)
# mlb_dv$monthyear <- as.numeric(mlb_dv$month)

mlb_dv$year <- year(mlb_dv$incident_date)
# Optional numeric year conversion (commented out)
# mlb_dv$year <- as.numeric(mlb_dv$year)

# Cleaning elapsed time column by removing text in parentheses
mlb_dv$elapsed_time_clean <- gsub("\\s*\\(.*?\\)", "", mlb_dv$elapsed_time)

# Calculating actual end time of the game
mlb_dv$end_time <- hms(mlb_dv$start_time) + hm(mlb_dv$elapsed_time_clean)

# Converting end time to decimal hours for easier analysis
mlb_dv$game_end_time <- hour(mlb_dv$end_time) + minute(mlb_dv$end_time)/60

# Calculating actual start time of the game
mlb_dv$start_time_1 <- hms(mlb_dv$start_time)

# Converting start time to decimal hours for easier analysis
mlb_dv$game_start_time <- hour(mlb_dv$start_time_1) + minute(mlb_dv$start_time_1)/60


# Creating win/loss indicators and scaling accuracy
# ------------------------------------------------------------------------------
# Win indicator: 1 if team won, 0 otherwise
# Logic: if home team and homewin==1, or away team and homewin==0
mlb_dv$win <- ifelse(mlb_dv$hometeam == 1 & mlb_dv$homewin == 1 |
                       mlb_dv$hometeam == 0 & mlb_dv$homewin == 0, 1, 0)

# Lose indicator: 1 if team lost, 0 otherwise
# Logic: if home team and homewin==0, or away team and homewin==1
mlb_dv$lose <- ifelse(mlb_dv$hometeam == 1 & mlb_dv$homewin == 0 |
                        mlb_dv$hometeam == 0 & mlb_dv$homewin == 1, 1, 0)

# Scale accuracy from proportion to percentage
mlb_dv$accuracy <- mlb_dv$accuracy * 100

# Create inaccuracy measure as complement of accuracy
mlb_dv$inaccuracy <- 100 - mlb_dv$accuracy


# Aggregating DV counts into different time windows
# ------------------------------------------------------------------------------
# Sum domestic violence incidents across specific hourly ranges for analysis
mlb_dv <- mlb_dv %>%
  mutate(
    dvcount_10pmto6am = dv_hour_22 + dv_hour_23 + dv_hour_0 + dv_hour_1 + dv_hour_2 + dv_hour_3 + dv_hour_4 + dv_hour_5,
    dvcount_8pmto6am  = dv_hour_20 + dv_hour_21 + dv_hour_22 + dv_hour_23 + dv_hour_0 + dv_hour_1 + dv_hour_2 + dv_hour_3 + dv_hour_4 + dv_hour_5,
    dvcount_6pmto6am  = dv_hour_18 + dv_hour_19 + dv_hour_20 + dv_hour_21 + dv_hour_22 + dv_hour_23 + dv_hour_0 + dv_hour_1 + dv_hour_2 + dv_hour_3 + dv_hour_4 + dv_hour_5,
    dvcount_4pmto4am  = dv_hour_16 + dv_hour_17 + dv_hour_18 + dv_hour_19 + dv_hour_20 + dv_hour_21 + dv_hour_22 + dv_hour_23 + dv_hour_0 + dv_hour_1 + dv_hour_2 + dv_hour_3,
    dvcount_2pmto2am  = dv_hour_14 + dv_hour_15 + dv_hour_16 + dv_hour_17 + dv_hour_18 + dv_hour_19 + dv_hour_20 + dv_hour_21 + dv_hour_22 + dv_hour_23 + dv_hour_0 + dv_hour_1,
    dvcount_noontomidnight = dv_hour_12 + dv_hour_13 + dv_hour_14 + dv_hour_15 + dv_hour_16 + dv_hour_17 + dv_hour_18 + dv_hour_19 + dv_hour_20 + dv_hour_21 + dv_hour_22 + dv_hour_23,
    dvcount_noonto2pm = dv_hour_12 + dv_hour_13,
    dvcount_2pmto4pm  = dv_hour_14 + dv_hour_15,
    dvcount_4pmto6pm  = dv_hour_16 + dv_hour_17,
    dvcount_6pmto8pm  = dv_hour_17 + dv_hour_18,
    dvcount_8pmto10pm = dv_hour_19 + dv_hour_20,
    dvcount_10pmtomidnight = dv_hour_21 + dv_hour_22,
    dvcount_midnightto2am = dv_hour_0 + dv_hour_1,
    dvcount_2amto4am  = dv_hour_2 + dv_hour_3,
    dvcount_4amto6am  = dv_hour_4 + dv_hour_5
  )

# Converting numeric variables to appropriate types
# ------------------------------------------------------------------------------
mlb_dv$temperature <- as.numeric(mlb_dv$temperature)
mlb_dv$attendance  <- as.numeric(gsub(",", "", mlb_dv$attendance))
mlb_dv$teams.away.leagueRecord.pct <- as.numeric(mlb_dv$teams.away.leagueRecord.pct)
mlb_dv$teams.home.leagueRecord.pct <- as.numeric(mlb_dv$teams.home.leagueRecord.pct)

# Loading holiday data
# ------------------------------------------------------------------------------
holidays <- read.csv("data\\raw\\Holidays.csv")
holidays$holiday_date <- mdy(holidays$holiday_date)

# Creating a holiday indicator variable for games occurring on holidays
mlb_dv <- mlb_dv %>%
  mutate(
    holiday = as.numeric(game_date %in% holidays$holiday_date)
  )

# Loading air quality data
# ------------------------------------------------------------------------------
airquality <- readRDS("data\\raw\\airquality/dailyairqualitybycounty_2009_2020.rds")
airquality$incident_date <- airquality$Date

# Selecting relevant columns and renaming for clarity
airquality <- airquality %>%
  select(AQI, Category, incident_date, team) %>%
  rename("AQI_Category" = "Category")
airquality$AQI <- as.numeric(airquality$AQI)

# Merging air quality data with MLB + DV dataset
mlb_dv <- merge(mlb_dv, airquality, by = c("incident_date", "team"))

# Filtering out double headers not included in the analysis
mlb_dv <- mlb_dv[mlb_dv$doubleHeader == "N",]    # Keep only single-header games

# Selecting key variables for summary statistics
# ------------------------------------------------------------------------------
keeps <- c("team", "homefavor", "inaccuracy", "temperature", "teams.away.score",
           "teams.home.score", "AQI", "lose", "year", "month", "dvcount_6pmto6am")
mlb_dv_sumstats <- mlb_dv[, keeps]

# Generating overall summary statistics table (mean, sd, median, min, max)
datasummary(All(mlb_dv_sumstats) ~ mean + sd + median + min + max,
            data = mlb_dv_sumstats,
            title = "Summary Statistics",
            output = "tables\\summarystats.tex")

# Generating team-level summary statistics for DV counts 6pm-6am
datasummary(team ~ dvcount_6pmto6am * (mean + sd + median + min + max),
            data = mlb_dv_sumstats,
            title = "DV By Team",
            output = "tables\\dv_sumstats_by_team.tex")

# Scatterplot: inaccuracy vs DV counts (6pm-6am) by team
# ------------------------------------------------------------------------------
ggplot(mlb_dv, aes(x = inaccuracy, y = dvcount_6pmto6am)) +
  geom_point() +
  facet_wrap(~ team) +
  labs(
    x = "Inaccuracy",
    y = "DV Count (6pm to 6am)"
  ) +
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  )

# Histogram: distribution of DV counts (6pm-6am) by team
ggplot(mlb_dv, aes(x = dvcount_6pmto6am)) +
  geom_histogram(binwidth = 1, color = "black", fill = "#EAAA00", size = 0.5) +
  facet_wrap(~ team) +
  labs(
    x = "DV Count (6pm to 6am)",
    y = "Frequency"
  ) +
  theme(
    text = element_text(family = "sans", size = 12),
    axis.line = element_line(colour = "black", size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family = "sans", size = 12)
  )
ggsave("figures/dvcount_histogram.pdf", width = 6.7, height = 6, units = "in",device = cairo_pdf)

# Histogram: distribution of inaccuracy by team
ggplot(mlb_dv, aes(x = inaccuracy)) +
  geom_histogram(binwidth = 1, color = "black", fill = "#002855", alpha = 0.5, size = 0.5) +
  facet_wrap(~ team) +
  labs(
    x = "Inaccuracy",
    y = "Frequency"
  ) +
  theme(
    text = element_text(family = "sans", size = 12),
    axis.line = element_line(colour = "black", size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    strip.text = element_text(family = "sans", size = 12)
  )
ggsave("figures/inaccuracy_histogram.pdf", width = 6.7, height = 6, units = "in",device = cairo_pdf)

# Main results
# ------------------------------------------------------------------------------

# Specifying Poisson models for DV counts (6pm-6am)
# ------------------------------------------------------------------------------
models <- list(
  "(1)" = fepois(dvcount_6pmto6am ~ inaccuracy * lose, vcov = ~team, data = mlb_dv),
  "(2)" = fepois(dvcount_6pmto6am ~ inaccuracy * lose + hometeam + temperature + attendance +
                   teams.home.leagueRecord.pct + teams.away.leagueRecord.pct + game_end_time + AQI + holiday,
                 vcov = ~team, data = mlb_dv),
  "(3)" = fepois(dvcount_6pmto6am ~ inaccuracy * lose | team + year + month + dayofweek,
                 vcov = ~team, data = mlb_dv),
  "(4)" = fepois(dvcount_6pmto6am ~ inaccuracy * lose + hometeam + temperature + attendance +
                   teams.home.leagueRecord.pct + teams.away.leagueRecord.pct + game_end_time + AQI + holiday |
                   team + year + month + dayofweek,
                 vcov = ~team, data = mlb_dv),
  "(5)" = fepois(dvcount_6pmto6am ~ inaccuracy * lose + hometeam + temperature + attendance +
                   teams.home.leagueRecord.pct + teams.away.leagueRecord.pct + game_end_time + AQI + holiday |
                   team^year + month + dayofweek,
                 vcov = ~team, data = mlb_dv),
  "(6)" = fepois(dvcount_6pmto6am ~ inaccuracy * lose + hometeam + temperature + attendance +
                   teams.home.leagueRecord.pct + teams.away.leagueRecord.pct + game_end_time + AQI + holiday |
                   team^year^month + dayofweek,
                 vcov = ~team, data = mlb_dv)
)
gofmap <- tribble(
  ~raw, ~clean, ~fmt,
  "nobs", "N", 0,
  "r.squared", "R<sup>2</sup>", 2,
  "aic", "AIC", 0
)
modelsummary(
  models,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_omit = "^(?!.*lose|inaccuracy)",  # keep only inaccuracy and lose coefficients
  gof_map = gofmap,
  title = "Interaction model base",
  output = "tables\\base_interaction_model.tex"
)

# Specifying Poisson models main results
# ------------------------------------------------------------------------------
rows <- tribble(
  ~term,        ~"1",      ~"2",        ~"3",   ~"4",       ~"5", ~"6",
  'Controls',    'No',    'Yes',     'No',     'Yes',     'Yes','Yes',   
  'Day-of-Week Fixed Effects',    'No',    'No',  'Yes',   'Yes',       'Yes','—',    
  'City Fixed Effects',    'No',    'No',     'Yes',  'Yes',       '—','—',    
  'Year Fixed Effects',    'No',    'No',     'Yes',  'Yes',       '—','—',    
  'Month Fixed Effects',    'No',    'No',     'Yes',   'Yes',       'Yes','—',      
  'City-by-Year Fixed Effects',    'No',    'No',     'No',  'No',       'Yes','Yes',    
  'City-by-Month Fixed Effects',    'No',    'No',  'No',   'No',      'No','Yes',
  'City-by-Day-of-Week Fixed Effects',    'No',    'No',  'No',   'No',      'No','Yes'
)

#attr(rows, 'position') <- c()

gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "aic","AIC",0)

models <- list(
  "Panel A: Wins and Losses Combined" = list(
    "(1)" = fepois(dvcount_6pmto6am~inaccuracy,vcov = ~team,data=mlb_dv),
    "(2)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv),
    "(3)" = fepois(dvcount_6pmto6am~inaccuracy|team+year+month+dayofweek,vcov = ~team,data=mlb_dv),
    "(4)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv),
    "(5)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv),
    "(6)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv) 
  ),
  "Panel B: Losses" = list(
    "(1)" = fepois(dvcount_6pmto6am~inaccuracy,vcov = ~team,data=mlb_dv[mlb_dv$lose==1,]),
    "(2)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv[mlb_dv$lose==1,]),
    "(3)" = fepois(dvcount_6pmto6am~inaccuracy|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1,]),
    "(4)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1,]),
    "(5)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1,]),
    "(6)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1,]) 
  ),
  "Panel C: Wins" = list(
    "(1)" = fepois(dvcount_6pmto6am~inaccuracy,vcov = ~team,data=mlb_dv[mlb_dv$lose==0,]),
    "(2)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv[mlb_dv$lose==0,]),
    "(3)" = fepois(dvcount_6pmto6am~inaccuracy|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==0,]),
    "(4)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==0,]),
    "(5)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==0,]),
    "(6)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==0,])
  ))

modelsummary(models,
             coef_omit = "^(?!.*inaccuracy)",
             coef_rename = c("inaccuracy" = "Inaccuracy"),
             estimate = "{estimate}{stars}",
             stars = c('*' = .1,'**'=.05, '***' = .01),
             gof_map = gm,
             shape = "rbind",
             add_rows = rows,
             output = "tables\\separated_wins_losses_base.tex"
)


# Specifying Poisson models for day of week and month analysis
# ------------------------------------------------------------------------------
models <- list(
  "Panel A: Effects Across Days" = list(
  "Mon" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI|team^month+team^year,vcov = ~team,data=mlb_dv[mlb_dv$dayofweek=="Mon"&mlb_dv$lose==1,]),
  "Tue" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI|team^month+team^year,vcov = ~team,data=mlb_dv[mlb_dv$dayofweek=="Tue"&mlb_dv$lose==1,]),
  "Wed" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI|team^month+team^year,vcov = ~team,data=mlb_dv[mlb_dv$dayofweek=="Wed"&mlb_dv$lose==1,]),
  "Thu" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI|team^month+team^year,vcov = ~team,data=mlb_dv[mlb_dv$dayofweek=="Thu"&mlb_dv$lose==1,]),
  "Fri" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI|team^month+team^year,vcov = ~team,data=mlb_dv[mlb_dv$dayofweek=="Fri"&mlb_dv$lose==1,]),
  "Sat" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI|team^month+team^year,vcov = ~team,data=mlb_dv[mlb_dv$dayofweek=="Sat"&mlb_dv$lose==1,]),
  "Sun" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI|team^month+team^year,vcov = ~team,data=mlb_dv[mlb_dv$dayofweek=="Sun"&mlb_dv$lose==1,])),
  "Panel B: Effects Across Months" = list(
    "Apr" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$month=="4"&mlb_dv$lose==1,]),
    "May" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$month=="5"&mlb_dv$lose==1,]),
    "June" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$month=="6"&mlb_dv$lose==1,]),
    "July" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$month=="7"&mlb_dv$lose==1,]),
    "Aug" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$month=="8"&mlb_dv$lose==1,]),
    "Sept" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$month=="9"&mlb_dv$lose==1,]),
    "Oct" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$month=="10"&mlb_dv$lose==1,]))
)

modelsummary(models,
             stars = c("*"=.1,"**"=.05,"***"=.01),
             coef_omit = "^(?!.*lose|inaccuracy)",
             gof_map = gofmap,
             title = "Effects of Umpire Accuracy on Domestic Violence At Different Times",
             shape = "rbind",
             output="tables\\week_and_month_analysis.tex")

# Specifying Poisson models for higher stakes games
# ------------------------------------------------------------------------------
# defining close games and records
mlb_dv <- mlb_dv %>%
  mutate(closegame = ifelse(abs(teams.home.score-teams.away.score)<=1,1,0),
         closerecords = ifelse(abs(teams.home.leagueRecord.pct-teams.away.leagueRecord.pct)<=.05,1,0))

# running analysis
rows <- tribble(
  ~term,        ~"1",      ~"2",        ~"3",   ~"4",       ~"5", ~"6",
  'Controls',    'No',    'Yes',     'No',     'Yes',     'Yes','Yes',   
  'Day-of-Week Fixed Effects',    'No',    'No',  'Yes',   'Yes',       'Yes','Yes',    
  'City Fixed Effects',    'No',    'No',     'Yes',  'Yes',       '—','—',    
  'Year Fixed Effects',    'No',    'No',     'Yes',  'Yes',       '—','—',    
  'Month Fixed Effects',    'No',    'No',     'Yes',   'Yes',       'Yes','—',      
  'City-by-Year Fixed Effects',    'No',    'No',     'No',  'No',       'Yes','—',    
  'City-by-Year-by-Month Fixed Effects',    'No',    'No',  'No',   'No',      'No','Yes'
)

attr(rows, 'position') <- c()

gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "aic","AIC",0)

models <- list(
  "Panel A: Regular Season Losses" = list(
    "(1)" = fepois(dvcount_6pmto6am~inaccuracy,vcov = ~team,data=mlb_dv[mlb_dv$game_type == "R" & mlb_dv$lose == 1,]),
    "(2)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv[mlb_dv$game_type == "R" & mlb_dv$lose == 1,]),
    "(3)" = fepois(dvcount_6pmto6am~inaccuracy|team^year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$game_type == "R" & mlb_dv$lose == 1,]),
    "(4)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$game_type == "R" & mlb_dv$lose == 1,]),
    "(5)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$game_type == "R" & mlb_dv$lose == 1,]),
    "(6)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$game_type == "R" & mlb_dv$lose == 1,]) 
  ),
  "Panel B: Post Season Losses" = list(
    "(1)" = fepois(dvcount_6pmto6am~inaccuracy,vcov = ~team,data=mlb_dv[mlb_dv$game_type != "R" & mlb_dv$lose == 1,]),
    "(2)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv[mlb_dv$game_type != "R" & mlb_dv$lose == 1,]),
    "(3)" = fepois(dvcount_6pmto6am~inaccuracy|team^year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$game_type != "R" & mlb_dv$lose == 1,]),
    "(4)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$game_type != "R" & mlb_dv$lose == 1,]),
    "(5)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$game_type != "R" & mlb_dv$lose == 1,]),
    "(6)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$game_type != "R" & mlb_dv$lose == 1,]) 
  ),
  "Panel C: Close Games Losses" = list(
    "(1)" = fepois(dvcount_6pmto6am~inaccuracy,vcov = ~team,data=mlb_dv[mlb_dv$closegame == 1 & mlb_dv$lose == 1,]),
    "(2)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv[mlb_dv$closegame == 1 & mlb_dv$lose == 1,]),
    "(3)" = fepois(dvcount_6pmto6am~inaccuracy|dayofweek+year^team+month,vcov = ~team,data=mlb_dv[mlb_dv$closegame == 1 & mlb_dv$lose == 1,]),
    "(4)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$closegame == 1 & mlb_dv$lose == 1,]),
    "(5)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$closegame == 1 & mlb_dv$lose == 1,]),
    "(6)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$closegame == 1 & mlb_dv$lose == 1,])
  ),
  "Panel D: Close Records Losses" = list(
    "(1)" = fepois(dvcount_6pmto6am~inaccuracy,vcov = ~team,data=mlb_dv[mlb_dv$closerecords == 1 & mlb_dv$lose == 1,]),
    "(2)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv[mlb_dv$closerecords == 1 & mlb_dv$lose == 1,]),
    "(3)" = fepois(dvcount_6pmto6am~inaccuracy|dayofweek+year^team+month,vcov = ~team,data=mlb_dv[mlb_dv$closerecords == 1 & mlb_dv$lose == 1,]),
    "(4)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$closerecords == 1 & mlb_dv$lose == 1,]),
    "(5)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$closerecords == 1 & mlb_dv$lose == 1,]),
    "(6)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$closerecords == 1 & mlb_dv$lose == 1,])
  ))

# Output single LaTeX table with panels
modelsummary(
  models,
  stars = c('*' = 0.1, '**' = 0.05, '***' = 0.01),
  coef_omit = "^(?!.*inaccuracy)",        # only show inaccuracy
  coef_rename = c("inaccuracy" = "Inaccuracy"),
  estimate = "{estimate}{stars}",
  gof_map = gm,
  shape = "rbind",
  add_rows = rows,
  title = "Effects In Higher Stakes Games",
  output="tables\\important games.tex")

# ------------------------------------------------------------------------------
# Binned Inaccuracy Analysis
# ------------------------------------------------------------------------------
# Create year-specific inaccuracy bins (5%, 25%, 50%, 75%, 95%, Above 95%)
mlb_dv <- mlb_dv %>%
  mutate(year = year(game_date)) %>%  # extract year from game date
  group_by(year) %>%                  # calculate quantiles within each year
  mutate(
    p5  = quantile(inaccuracy, 0.05, na.rm = TRUE),
    p25 = quantile(inaccuracy, 0.25, na.rm = TRUE),
    p50 = quantile(inaccuracy, 0.50, na.rm = TRUE),
    p75 = quantile(inaccuracy, 0.75, na.rm = TRUE),
    p95 = quantile(inaccuracy, 0.95, na.rm = TRUE),
    # Assign each observation to a bin based on inaccuracy percentile
    inaccuracy_bin = case_when(
      inaccuracy <= p5  ~ "5%",
      inaccuracy <= p25 ~ "25%",
      inaccuracy <= p50 ~ "50%",
      inaccuracy <= p75 ~ "75%",
      inaccuracy <= p95 ~ "95%",
      TRUE              ~ "Above 95%"
    )
  ) %>%
  ungroup() %>%
  # Ensure factor levels are ordered logically for tables/plots
  mutate(inaccuracy_bin = factor(
    inaccuracy_bin,
    levels = c("5%", "25%", "50%", "75%", "95%", "Above 95%")
  )) %>%
  # Remove temporary quantile columns
  select(-p5, -p25, -p50, -p75, -p95)

# ------------------------------------------------------------------------------
# Poisson regression using binned inaccuracy (losses only)
# ------------------------------------------------------------------------------
models <- list(
  "Binned Poisson" = fepois(
    dvcount_6pmto6am ~ inaccuracy_bin + hometeam + temperature + attendance +
      game_end_time + teams.home.leagueRecord.pct + teams.away.leagueRecord.pct +
      AQI + holiday | team^month+team^year+team^dayofweek,
    vcov = ~team,
    data = mlb_dv[mlb_dv$lose == 1,]
  )
)

# ------------------------------------------------------------------------------
# Output LaTeX table for binned inaccuracy results
# ------------------------------------------------------------------------------
modelsummary(
  models,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_omit = "^(?!.*lose|above_avg_acc|inaccuracy_bin)", # only show bins
  gof_map = gofmap,
  title = "Inaccuracy Bins",
  output = "tables\\inaccuracy_bins.tex"
)

# ------------------------------------------------------------------------------
# Extract coefficients for plotting
# ------------------------------------------------------------------------------
coefs <- summary(models[["Binned Poisson"]])$coeftable %>%
  as.data.frame() %>%
  # Keep only the inaccuracy_bin terms
  filter(grepl("inaccuracy_bin", rownames(.))) %>%
  mutate(
    term  = rownames(.),
    lower = Estimate - 1.96 * `Std. Error`,
    upper = Estimate + 1.96 * `Std. Error`,
    # Clean up term names for the plot
    term  = gsub("inaccuracy_bin", "", term),
    term  = factor(term, levels = c("25%", "50%", "75%", "95%", "Above 95%"))
  )

# ------------------------------------------------------------------------------
# Plot coefficients with 95% confidence intervals
# ------------------------------------------------------------------------------
ggplot(coefs, aes(x = term, y = Estimate)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.1, size = 1) +
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
  labs(
    x = "Inaccuracy Bin",
    y = "Estimate"
  ) +
  theme(
    text = element_text(family = "sans", size = 12),
    axis.line = element_line(colour = "black", size = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  )
ggsave("figures/inaccuracy_bins.pdf", width = 6.7, height = 6, units = "in",device = cairo_pdf)


# Attendance-Based Poisson Models
# ------------------------------------------------------------------------------
# Calculate the yearly average attendance for each team
mlb_dv <- mlb_dv %>%
  group_by(year, team) %>%                          # group by both year and team
  mutate(yearly_avg_attendance = mean(attendance, na.rm = TRUE)) %>%  
  ungroup()

# Split dataset into games with below-average and above-average attendance
less_than_mean    <- mlb_dv %>% filter(attendance < yearly_avg_attendance)
greater_than_mean <- mlb_dv %>% filter(attendance > yearly_avg_attendance)

# Define Poisson regression models for losses based on attendance
models <- list(
  "Below Avg Attendance" = fepois(
    dvcount_6pmto6am ~ inaccuracy + temperature + attendance + 
      teams.home.leagueRecord.pct + teams.away.leagueRecord.pct + 
      game_end_time + AQI + holiday |
      month+dayofweek+year^team,
    vcov = ~team,
    data = less_than_mean[less_than_mean$lose == 1,]
  ),
  "Above Avg Attendance" = fepois(
    dvcount_6pmto6am ~ inaccuracy + temperature + attendance + 
      teams.home.leagueRecord.pct + teams.away.leagueRecord.pct + 
      game_end_time + AQI + holiday |
      month+dayofweek+year^team,
    vcov = ~team,
    data = greater_than_mean[greater_than_mean$lose == 1,]
  )
)

# Output model summaries to LaTeX table
modelsummary(
  models,
  stars = c("*" = 0.1, "**" = 0.05, "***" = 0.01),
  coef_omit = "^(?!.*lose|inaccuracy)",  # only show inaccuracy and related variables
  gof_map = gofmap,
  title = "Effects by Attendance",
  output = "tables\\attendance_subset.tex"
)

# Varying timing analysis
# ------------------------------------------------------------------------------
mlb_dv <- mlb_dv %>%
  mutate(game_end_hour = floor(game_end_time),
         game_start_hour = floor(game_start_time),
         dv_hour_24 = dv_hour_0,
         dv_hour_25 = dv_hour_1,
         dv_hour_26 = dv_hour_2,
         dv_hour_27 = dv_hour_3,
         dv_hour_28 = dv_hour_4,
         dv_hour_29 = dv_hour_5,
         dv_hour_30 = dv_hour_6)

mlb_dv <- mlb_dv %>%
  mutate(
    total_dv = rowSums(
      select(
        .,
        dv_hour_7,
        dv_hour_8,
        dv_hour_9,
        dv_hour_10,
        dv_hour_11,
        dv_hour_12,
        dv_hour_13,
        dv_hour_14,
        dv_hour_15,
        dv_hour_16,
        dv_hour_17,
        dv_hour_18,
        dv_hour_19,
        dv_hour_20,
        dv_hour_21,
        dv_hour_22,
        dv_hour_23,
        dv_hour_0,
        dv_hour_1,
        dv_hour_2,
        dv_hour_3,
        dv_hour_4,
        dv_hour_5,
        dv_hour_6
      ),
      na.rm = TRUE
    )
  )

mlb_dv <- mlb_dv %>%
  mutate(
    # Calculate dv_post_game based on game_end_hour using case_when
    dv_post_game = case_when(
      game_end_hour == 14 ~ rowSums(select(., dv_hour_14, dv_hour_15, dv_hour_16, dv_hour_17, dv_hour_18, dv_hour_19, dv_hour_20, dv_hour_21, dv_hour_22, dv_hour_23, dv_hour_0, dv_hour_1, dv_hour_2, dv_hour_3, dv_hour_4, dv_hour_5, dv_hour_6), na.rm = TRUE),
      game_end_hour == 15 ~ rowSums(select(., dv_hour_15, dv_hour_16, dv_hour_17, dv_hour_18, dv_hour_19, dv_hour_20, dv_hour_21, dv_hour_22, dv_hour_23, dv_hour_0, dv_hour_1, dv_hour_2, dv_hour_3, dv_hour_4, dv_hour_5, dv_hour_6), na.rm = TRUE),
      game_end_hour == 16 ~ rowSums(select(., dv_hour_16, dv_hour_17, dv_hour_18, dv_hour_19, dv_hour_20, dv_hour_21, dv_hour_22, dv_hour_23, dv_hour_0, dv_hour_1, dv_hour_2, dv_hour_3, dv_hour_4, dv_hour_5, dv_hour_6), na.rm = TRUE),
      game_end_hour == 17 ~ rowSums(select(., dv_hour_17, dv_hour_18, dv_hour_19, dv_hour_20, dv_hour_21, dv_hour_22, dv_hour_23, dv_hour_0, dv_hour_1, dv_hour_2, dv_hour_3, dv_hour_4, dv_hour_5, dv_hour_6), na.rm = TRUE),
      game_end_hour == 18 ~ rowSums(select(., dv_hour_18, dv_hour_19, dv_hour_20, dv_hour_21, dv_hour_22, dv_hour_23, dv_hour_0, dv_hour_1, dv_hour_2, dv_hour_3, dv_hour_4, dv_hour_5, dv_hour_6), na.rm = TRUE),
      game_end_hour == 19 ~ rowSums(select(., dv_hour_19, dv_hour_20, dv_hour_21, dv_hour_22, dv_hour_23, dv_hour_0, dv_hour_1, dv_hour_2, dv_hour_3, dv_hour_4, dv_hour_5, dv_hour_6), na.rm = TRUE),
      game_end_hour == 20 ~ rowSums(select(., dv_hour_20, dv_hour_21, dv_hour_22, dv_hour_23, dv_hour_0, dv_hour_1, dv_hour_2, dv_hour_3, dv_hour_4, dv_hour_5, dv_hour_6), na.rm = TRUE),
      game_end_hour == 21 ~ rowSums(select(., dv_hour_21, dv_hour_22, dv_hour_23, dv_hour_0, dv_hour_1, dv_hour_2, dv_hour_3, dv_hour_4, dv_hour_5, dv_hour_6), na.rm = TRUE),
      game_end_hour == 22 ~ rowSums(select(., dv_hour_22, dv_hour_23, dv_hour_0, dv_hour_1, dv_hour_2, dv_hour_3, dv_hour_4, dv_hour_5, dv_hour_6), na.rm = TRUE),
      game_end_hour == 23 ~ rowSums(select(., dv_hour_23, dv_hour_0, dv_hour_1, dv_hour_2, dv_hour_3, dv_hour_4, dv_hour_5, dv_hour_6), na.rm = TRUE),
      game_end_hour == 24 ~ rowSums(select(., dv_hour_0, dv_hour_1, dv_hour_2, dv_hour_3, dv_hour_4, dv_hour_5, dv_hour_6), na.rm = TRUE),
      game_end_hour == 25 ~ rowSums(select(., dv_hour_1, dv_hour_2, dv_hour_3, dv_hour_4, dv_hour_5, dv_hour_6), na.rm = TRUE),
      game_end_hour == 26 ~ rowSums(select(., dv_hour_2, dv_hour_3, dv_hour_4, dv_hour_5, dv_hour_6), na.rm = TRUE)
    ))

mlb_dv <- mlb_dv %>%
  mutate(
    dv_pre_game = case_when(
      game_start_hour == 11 ~ rowSums(select(., dv_hour_7, dv_hour_8, dv_hour_9, dv_hour_10), na.rm = TRUE),
      game_start_hour == 12 ~ rowSums(select(., dv_hour_7, dv_hour_8, dv_hour_9, dv_hour_10, dv_hour_11), na.rm = TRUE),
      game_start_hour == 13 ~ rowSums(select(., dv_hour_7, dv_hour_8, dv_hour_9, dv_hour_10, dv_hour_11, dv_hour_12), na.rm = TRUE),
      game_start_hour == 14 ~ rowSums(select(., dv_hour_7, dv_hour_8, dv_hour_9, dv_hour_10, dv_hour_11, dv_hour_12, dv_hour_13), na.rm = TRUE),
      game_start_hour == 15 ~ rowSums(select(., dv_hour_7, dv_hour_8, dv_hour_9, dv_hour_10, dv_hour_11, dv_hour_12, dv_hour_13, dv_hour_14), na.rm = TRUE),
      game_start_hour == 16 ~ rowSums(select(., dv_hour_7, dv_hour_8, dv_hour_9, dv_hour_10, dv_hour_11, dv_hour_12, dv_hour_13, dv_hour_14, dv_hour_15), na.rm = TRUE),
      game_start_hour == 17 ~ rowSums(select(., dv_hour_7, dv_hour_8, dv_hour_9, dv_hour_10, dv_hour_11, dv_hour_12, dv_hour_13, dv_hour_14, dv_hour_15, dv_hour_16), na.rm = TRUE),
      game_start_hour == 18 ~ rowSums(select(., dv_hour_7, dv_hour_8, dv_hour_9, dv_hour_10, dv_hour_11, dv_hour_12, dv_hour_13, dv_hour_14, dv_hour_15, dv_hour_16, dv_hour_17), na.rm = TRUE),
      game_start_hour == 19 ~ rowSums(select(., dv_hour_7, dv_hour_8, dv_hour_9, dv_hour_10, dv_hour_11, dv_hour_12, dv_hour_13, dv_hour_14, dv_hour_15, dv_hour_16, dv_hour_17, dv_hour_18), na.rm = TRUE),
      game_start_hour == 20 ~ rowSums(select(., dv_hour_7, dv_hour_8, dv_hour_9, dv_hour_10, dv_hour_11, dv_hour_12, dv_hour_13, dv_hour_14, dv_hour_15, dv_hour_16, dv_hour_17, dv_hour_18, dv_hour_19), na.rm = TRUE),
      game_start_hour == 21 ~ rowSums(select(., dv_hour_7, dv_hour_8, dv_hour_9, dv_hour_10, dv_hour_11, dv_hour_12, dv_hour_13, dv_hour_14, dv_hour_15, dv_hour_16, dv_hour_17, dv_hour_18, dv_hour_19, dv_hour_20), na.rm = TRUE),
      game_start_hour == 22 ~ rowSums(select(., dv_hour_7, dv_hour_8, dv_hour_9, dv_hour_10, dv_hour_11, dv_hour_12, dv_hour_13, dv_hour_14, dv_hour_15, dv_hour_16, dv_hour_17, dv_hour_18, dv_hour_19, dv_hour_20, dv_hour_21), na.rm = TRUE)
    )
  )

mlb_dv <- mlb_dv %>%
  mutate(
    dv_in_game = total_dv - dv_pre_game - dv_post_game
  )



models <- list(
  "post game" = fepois(dv_post_game~inaccuracy+hometeam+temperature+attendance+game_end_time+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+AQI+holiday|month+dayofweek+year^team,vcov = ~team,data=mlb_dv[mlb_dv$lose==1,]),
  "pre game" = fepois(dv_pre_game~inaccuracy+hometeam+temperature+attendance+game_end_time+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+AQI+holiday|month+dayofweek+year^team,vcov = ~team,data=mlb_dv[mlb_dv$lose==1,])
)

modelsummary(models,
             stars = c("*"=.1,"**"=.05,"***"=.01),
             coef_omit = "^(?!.*lose|inaccuracy)",
             gof_map = gofmap,
             title = "Pre-v-Post Game Reports"
             ,output = "tables\\pre-v-post-game.tex")

# Initialize an empty list for the models
models <- list()

# Loop through hours 1 to 23
for (hour in 1:23) {
  model_name <- as.character(hour)
  formula <- as.formula(paste0("dv_hour_", hour, "~inaccuracy + hometeam + temperature + attendance + teams.home.leagueRecord.pct + teams.away.leagueRecord.pct + AQI + holiday | month+dayofweek+year^team"))
  model <- fepois(formula, vcov = ~team, data = mlb_dv[mlb_dv$game_end_hour==22,])
  models[[model_name]] <- model
}

# Generate the summary with modelsummary
modelsummary(models,
             stars = c("*" = .1, "**" = .05, "***" = .01),
             coef_omit = "^(?!.*lose|inaccuracy)",
             gof_map = gofmap,
             title = "Time of Day")

# Loss aversion Poisson Models
# ------------------------------------------------------------------------------
# creating favor variable
mlb_dv$favor <- ifelse(mlb_dv$hometeam==1,mlb_dv$homefavor,-1*mlb_dv$homefavor)
mlb_dv$disfavor <- -1*mlb_dv$favor
mlb_dv$favored <- ifelse(mlb_dv$hometeam==1&mlb_dv$homefavor>0|mlb_dv$hometeam==0&mlb_dv$homefavor<0,1,0)
mlb_dv$disfavored <- ifelse(mlb_dv$favored==1,0,1)

# running models
rows <- tribble(
  ~term,        ~"1",      ~"2",        ~"3",   ~"4",       ~"5", ~"6",
  'Controls',    'Yes',    'Yes',     'Yes',     'Yes',     'Yes','Yes',   
  'Day-of-Week Fixed Effects',    'No',    'Yes',  'Yes',   'No',       'Yes','Yes',
  'Month Fixed Effects',    'No',    'Yes',  'Yes',   'No',       'Yes','Yes',     
  'City Fixed Effects',    'No',    'Yes',  '—',   'No',       'Yes','—',
  'Year Fixed Effects',    'No',    'Yes',  '—',   'No',       'Yes','—',    
  'City-by-Year Fixed Effects',    'No',    'No',     'Yes',  'No',       'No','Yes',
)

models <- list(
  "Panel A: Losses" = list(
    "Favored" = fepois(dvcount_6pmto6am~inaccuracy+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$favored==1,]),
    "Favored" = fepois(dvcount_6pmto6am~inaccuracy+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|month+dayofweek+year+team,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$favored==1,]),
    "Favored" = fepois(dvcount_6pmto6am~inaccuracy+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|month+dayofweek+year^team,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$favored==1,]),
    "Not Favored" = fepois(dvcount_6pmto6am~inaccuracy+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$favored==0,]),
    "Not Favored" = fepois(dvcount_6pmto6am~inaccuracy+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|month+dayofweek+year+team,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$favored==0,]),
    "Not Favored" = fepois(dvcount_6pmto6am~inaccuracy+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|month+dayofweek+year^team,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$favored==0,])
  ),
  "Panel B: Wins" = list(
    "Favored" = fepois(dvcount_6pmto6am~inaccuracy+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv[mlb_dv$lose==0&mlb_dv$favored==1,,]),
    "Favored" = fepois(dvcount_6pmto6am~inaccuracy+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|month+dayofweek+year+team,vcov = ~team,data=mlb_dv[mlb_dv$lose==0&mlb_dv$favored==1,,]),
    "Favored" = fepois(dvcount_6pmto6am~inaccuracy+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|month+dayofweek+year^team,vcov = ~team,data=mlb_dv[mlb_dv$lose==0&mlb_dv$favored==1,,]),
    "Not Favored" = fepois(dvcount_6pmto6am~inaccuracy+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv[mlb_dv$lose==0&mlb_dv$favored==0,,]),
    "Not Favored" = fepois(dvcount_6pmto6am~inaccuracy+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|month+dayofweek+year+team,vcov = ~team,data=mlb_dv[mlb_dv$lose==0&mlb_dv$favored==0,,]),
    "Not Favored" = fepois(dvcount_6pmto6am~inaccuracy+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|month+dayofweek+year^team,vcov = ~team,data=mlb_dv[mlb_dv$lose==0&mlb_dv$favored==0,,])
  ))


modelsummary(models,
             stars = c("*"=.1,"**"=.05,"***"=.01),
             coef_omit = "^(?!.*lose|inaccuracy)",
             gof_map = gofmap,
             shape = "rbind",
             add_rows = rows,
             title = "loss aversion",
             output = "tables\\loss_aversion.tex")

# Robustness Checks
# ------------------------------------------------------------------------------

# Varying timing
# ------------------------------------------------------------------------------
models <- list(
  "6pm - 6am" = fepois(dvcount_6pmto6am~inaccuracy+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|month+dayofweek+year^team,vcov = ~team,data=mlb_dv[mlb_dv$lose==1,]),
  "4pm - 4am" = fepois(dvcount_4pmto4am~inaccuracy+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|month+dayofweek+year^team,vcov = ~team,data=mlb_dv[mlb_dv$lose==1,]),
  "8pm - 6am" = fepois(dvcount_8pmto6am~inaccuracy+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|month+dayofweek+year^team,vcov = ~team,data=mlb_dv[mlb_dv$lose==1,]),
  "10pm - 6am" = fepois(dvcount_10pmto6am~inaccuracy+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|month+dayofweek+year^team,vcov = ~team,data=mlb_dv[mlb_dv$lose==1,]),
  "post game" = fepois(dv_post_game~inaccuracy+hometeam+temperature+attendance+game_end_time+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+AQI+holiday|month+dayofweek+year^team,vcov = ~team,data=mlb_dv[mlb_dv$lose==1,]),
  "pre game" = fepois(dv_pre_game~inaccuracy+hometeam+temperature+attendance+game_end_time+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+AQI+holiday|month+dayofweek+year^team,vcov = ~team,data=mlb_dv[mlb_dv$lose==1,]))


modelsummary(models,
             stars = c("*"=.1,"**"=.05,"***"=.01),
             coef_omit = "^(?!.*lose|inaccuracy)",
             gof_map = gofmap,
             title = "Varying Timeframe for Domestic Violence"
             ,output = "tables\\time_variation_robustness_check.tex")

# Female to Male DV analysis
# ------------------------------------------------------------------------------
dvcount_female_to_male <- readRDS("data/processed/nibrs_domestic_counts_2009-2020_female_to_male.rds")

mlb_games_subset$lose <- ifelse(mlb_games_subset$hometeam == 1 & mlb_games_subset$homewin == 0 |
                                  mlb_games_subset$hometeam == 0 & mlb_games_subset$homewin == 1, 1, 0)

# Scale accuracy from proportion to percentage
mlb_games_subset$accuracy <- mlb_games_subset$accuracy * 100

# Create inaccuracy measure as complement of accuracy
mlb_games_subset$inaccuracy <- 100 - mlb_games_subset$accuracy

mlb_games_subset <- mlb_games_subset %>%
  mutate(year = year(game_date)) %>%  # extract year from game date
  group_by(year) %>%                  # calculate quantiles within each year
  mutate(
    p5  = quantile(inaccuracy, 0.05, na.rm = TRUE),
    p25 = quantile(inaccuracy, 0.25, na.rm = TRUE),
    p50 = quantile(inaccuracy, 0.50, na.rm = TRUE),
    p75 = quantile(inaccuracy, 0.75, na.rm = TRUE),
    p95 = quantile(inaccuracy, 0.95, na.rm = TRUE),
    # Assign each observation to a bin based on disfavor percentile
    inaccuracy_bin = case_when(
      inaccuracy <= p5  ~ "5%",
      inaccuracy <= p25 ~ "25%",
      inaccuracy <= p50 ~ "50%",
      inaccuracy <= p75 ~ "75%",
      inaccuracy <= p95 ~ "95%",
      TRUE              ~ "Above 95%"
    )
  ) %>%
  ungroup() %>%
  # Ensure factor levels are ordered logically for tables/plots
  mutate(inaccuracy_bin = factor(
    inaccuracy_bin,
    levels = c("5%", "25%", "50%", "75%", "95%", "Above 95%")
  )) %>%
  # Remove temporary quantile columns
  select(-p5, -p25, -p50, -p75, -p95)

mlb_games_subset$top25inaccurate <- ifelse(mlb_games_subset$inaccuracy_bin=="Above 95%"|mlb_games_subset$inaccuracy_bin=="95%",1,0)

mlb_games_subset$favor <- ifelse(mlb_games_subset$hometeam==1,mlb_games_subset$homefavor,-1*mlb_games_subset$homefavor)
mlb_games_subset$disfavor <- -1*mlb_games_subset$favor
mlb_games_subset$favored <- ifelse(mlb_games_subset$hometeam==1&mlb_games_subset$homefavor>0|mlb_games_subset$hometeam==0&mlb_games_subset$homefavor<0,1,0)
mlb_games_subset$disfavored <- ifelse(mlb_games_subset$favored==1,0,1)

mlb_dv_female_to_male <- merge(mlb_games_subset, dvcount_female_to_male, by = c("incident_date", "ori"), all.x = TRUE) %>%
  filter(team %in% c("CIN", "CLE", "COL", "DET", "KC", "MIL", "MIN", "SEA", "TEX")) %>%
  filter(!(team == "TEX" & incident_date < as.Date("2017-01-01"))) %>%
  filter(!(team == "KC" & incident_date < as.Date("2010-01-01"))) %>%
  filter(!(team == "MIN" & incident_date < as.Date("2019-01-01")))%>%
  filter(!(team == "SEA" & incident_date < as.Date("2012-01-01")))%>%
  mutate(across(c(dv_hour_0:dv_hour_23), ~replace_na(.x, 0)))

mlb_dv_female_to_male$dayofweek <- wday(mlb_dv_female_to_male$incident_date, label = TRUE)
#mlb_dv_female_to_male$dayofweek <- as.factor(mlb_dv_female_to_male$dayofweek)
mlb_dv_female_to_male$month <- month(mlb_dv_female_to_male$incident_date)
#mlb_dv_female_to_male$monthyear <- as.numeric(mlb_dv_female_to_male$month)
mlb_dv_female_to_male$year <- year(mlb_dv_female_to_male$incident_date)
#mlb_dv_female_to_male$year <- as.numeric(mlb_dv_female_to_male$year)
mlb_dv_female_to_male$elapsed_time_clean <- gsub("\\s*\\(.*?\\)", "", mlb_dv_female_to_male$elapsed_time)
mlb_dv_female_to_male$end_time <- hms(mlb_dv_female_to_male$start_time) + hm(mlb_dv_female_to_male$elapsed_time_clean)
mlb_dv_female_to_male$game_end_time <- hour(mlb_dv_female_to_male$end_time) + minute(mlb_dv_female_to_male$end_time)/60

mlb_dv_female_to_male$win <- ifelse(mlb_dv_female_to_male$hometeam==1 & mlb_dv_female_to_male$homewin==1|mlb_dv_female_to_male$hometeam==0 & mlb_dv_female_to_male$homewin==0,1,0)
mlb_dv_female_to_male$lose <- ifelse(mlb_dv_female_to_male$hometeam==1 & mlb_dv_female_to_male$homewin==0|mlb_dv_female_to_male$hometeam==0 & mlb_dv_female_to_male$homewin==1,1,0)
mlb_dv_female_to_male$accuracy <- mlb_dv_female_to_male$accuracy*100
mlb_dv_female_to_male$inaccuracy <- 100-mlb_dv_female_to_male$accuracy

mlb_dv_female_to_male <- mlb_dv_female_to_male %>%
  mutate(dvcount_10pmto6am = dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_8pmto6am = dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_6pmto6am = dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_4pmto4am = dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3,
         dvcount_2pmto2am = dv_hour_14+dv_hour_15+dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1,
         dvcount_noontomidnight = dv_hour_12+dv_hour_13+dv_hour_14+dv_hour_15+dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23,
         dvcount_noonto2pm = dv_hour_12+dv_hour_13,
         dvcount_2pmto4pm = dv_hour_14+dv_hour_15,
         dvcount_4pmto6pm = dv_hour_16+dv_hour_17,
         dvcount_6pmto8pm = dv_hour_17+dv_hour_18,
         dvcount_8pmto10pm = dv_hour_19+dv_hour_20,
         dvcount_10pmtomidnight = dv_hour_21+dv_hour_22,
         dvcount_midnightto2am = dv_hour_0+dv_hour_1,
         dvcount_2amto4am = dv_hour_2+dv_hour_3,
         dvcount_4amto6am = dv_hour_4+dv_hour_5)


mlb_dv_female_to_male$temperature <- as.numeric(mlb_dv_female_to_male$temperature)
mlb_dv_female_to_male$attendance <- as.numeric(gsub(",", "", mlb_dv_female_to_male$attendance))
mlb_dv_female_to_male$teams.away.leagueRecord.pct <- as.numeric(mlb_dv_female_to_male$teams.away.leagueRecord.pct)
mlb_dv_female_to_male$teams.home.leagueRecord.pct <- as.numeric(mlb_dv_female_to_male$teams.home.leagueRecord.pct)

mlb_dv_female_to_male <- mlb_dv_female_to_male %>%
  mutate(
    holiday = as.numeric(game_date %in% holidays$holiday_date)
  )

mlb_dv_female_to_male <- merge(mlb_dv_female_to_male,airquality, by = c("incident_date", "team"))
mlb_dv_female_to_male <- mlb_dv_female_to_male[mlb_dv_female_to_male$doubleHeader=="N",]

rows <- tribble(
  ~term,        ~"1",      ~"2",        ~"3",   ~"4",       ~"5", ~"6",
  'Controls',    'No',    'Yes',     'No',     'Yes',     'Yes','Yes',   
  'Day-of-Week Fixed Effects',    'No',    'No',  'Yes',   'Yes',       'Yes','—',    
  'City Fixed Effects',    'No',    'No',     'Yes',  'Yes',       '—','—',    
  'Year Fixed Effects',    'No',    'No',     'Yes',  'Yes',       '—','—',    
  'Month Fixed Effects',    'No',    'No',     'Yes',   'Yes',       'Yes','—',      
  'City-by-Year Fixed Effects',    'No',    'No',     'No',  'No',       'Yes','Yes',    
  'City-by-Month Fixed Effects',    'No',    'No',  'No',   'No',      'No','Yes',
  'City-by-Day-of-Week Fixed Effects',    'No',    'No',  'No',   'No',      'No','Yes'
)

attr(rows, 'position') <- c()

gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "aic","AIC",0)

models <- list(
  "Panel A: Wins and Losses Combined" = list(
    "(1)" = fepois(dvcount_6pmto6am~inaccuracy,vcov = ~team,data=mlb_dv_female_to_male),
    "(2)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv_female_to_male),
    "(3)" = fepois(dvcount_6pmto6am~inaccuracy|team+year+month+dayofweek,vcov = ~team,data=mlb_dv_female_to_male),
    "(4)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv_female_to_male),
    "(5)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv_female_to_male),
    "(6)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv_female_to_male) 
  ),
  "Panel B: Losses" = list(
    "(1)" = fepois(dvcount_6pmto6am~inaccuracy,vcov = ~team,data=mlb_dv_female_to_male[mlb_dv_female_to_male$lose==1,]),
    "(2)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv_female_to_male[mlb_dv_female_to_male$lose==1,]),
    "(3)" = fepois(dvcount_6pmto6am~inaccuracy|team+year+month+dayofweek,vcov = ~team,data=mlb_dv_female_to_male[mlb_dv_female_to_male$lose==1,]),
    "(4)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv_female_to_male[mlb_dv_female_to_male$lose==1,]),
    "(5)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv_female_to_male[mlb_dv_female_to_male$lose==1,]),
    "(6)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv_female_to_male[mlb_dv_female_to_male$lose==1,]) 
  ),
  "Panel C: Wins" = list(
    "(1)" = fepois(dvcount_6pmto6am~inaccuracy,vcov = ~team,data=mlb_dv_female_to_male[mlb_dv_female_to_male$lose==0,]),
    "(2)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv_female_to_male[mlb_dv_female_to_male$lose==0,]),
    "(3)" = fepois(dvcount_6pmto6am~inaccuracy|team+year+month+dayofweek,vcov = ~team,data=mlb_dv_female_to_male[mlb_dv_female_to_male$lose==0,]),
    "(4)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv_female_to_male[mlb_dv_female_to_male$lose==0,]),
    "(5)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv_female_to_male[mlb_dv_female_to_male$lose==0,]),
    "(6)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv_female_to_male[mlb_dv_female_to_male$lose==0,])
  ))

modelsummary(models,
             coef_omit = "^(?!.*inaccuracy)",
             coef_rename = c("inaccuracy" = "Inaccuracy"),
             estimate = "{estimate}{stars}",
             stars = c('*' = .1,'**'=.05, '***' = .01),
             gof_map = gm,
             shape = "rbind",
             add_rows = rows,
             output = "tables\\Female-to-male_DV.tex"
)

# All other crime analysis
# ------------------------------------------------------------------------------


#rape
rape <- readRDS("data/processed/nibrs_domestic_counts_2009-2020_rape.rds")

mlb_rape <- merge(mlb_games_subset, rape, by = c("incident_date", "ori"), all.x = TRUE) %>%
  filter(team %in% c("CIN", "CLE", "COL", "DET", "KC", "MIL", "MIN", "SEA", "TEX")) %>%
  filter(!(team == "TEX" & incident_date < as.Date("2017-01-01"))) %>%
  filter(!(team == "KC" & incident_date < as.Date("2010-01-01"))) %>%
  filter(!(team == "MIN" & incident_date < as.Date("2019-01-01")))%>%
  filter(!(team == "SEA" & incident_date < as.Date("2012-01-01")))%>%
  mutate(across(c(dv_hour_0:dv_hour_23), ~replace_na(.x, 0)))

mlb_rape$dayofweek <- wday(mlb_rape$incident_date, label = TRUE)
#mlb_rape$dayofweek <- as.factor(mlb_rape$dayofweek)
mlb_rape$month <- month(mlb_rape$incident_date)
#mlb_rape$monthyear <- as.numeric(mlb_rape$month)
mlb_rape$year <- year(mlb_rape$incident_date)
#mlb_rape$year <- as.numeric(mlb_rape$year)
mlb_rape$elapsed_time_clean <- gsub("\\s*\\(.*?\\)", "", mlb_rape$elapsed_time)
mlb_rape$end_time <- hms(mlb_rape$start_time) + hm(mlb_rape$elapsed_time_clean)
mlb_rape$game_end_time <- hour(mlb_rape$end_time) + minute(mlb_rape$end_time)/60

mlb_rape$win <- ifelse(mlb_rape$hometeam==1 & mlb_rape$homewin==1|mlb_rape$hometeam==0 & mlb_rape$homewin==0,1,0)
mlb_rape$lose <- ifelse(mlb_rape$hometeam==1 & mlb_rape$homewin==0|mlb_rape$hometeam==0 & mlb_rape$homewin==1,1,0)
mlb_rape$accuracy <- mlb_rape$accuracy*100
mlb_rape$inaccuracy <- 100-mlb_rape$accuracy

mlb_rape <- mlb_rape %>%
  mutate(dvcount_10pmto6am = dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_8pmto6am = dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_6pmto6am = dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_4pmto4am = dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3,
         dvcount_2pmto2am = dv_hour_14+dv_hour_15+dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1,
         dvcount_noontomidnight = dv_hour_12+dv_hour_13+dv_hour_14+dv_hour_15+dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23,
         dvcount_noonto2pm = dv_hour_12+dv_hour_13,
         dvcount_2pmto4pm = dv_hour_14+dv_hour_15,
         dvcount_4pmto6pm = dv_hour_16+dv_hour_17,
         dvcount_6pmto8pm = dv_hour_17+dv_hour_18,
         dvcount_8pmto10pm = dv_hour_19+dv_hour_20,
         dvcount_10pmtomidnight = dv_hour_21+dv_hour_22,
         dvcount_midnightto2am = dv_hour_0+dv_hour_1,
         dvcount_2amto4am = dv_hour_2+dv_hour_3,
         dvcount_4amto6am = dv_hour_4+dv_hour_5)


mlb_rape$temperature <- as.numeric(mlb_rape$temperature)
mlb_rape$attendance <- as.numeric(gsub(",", "", mlb_rape$attendance))
mlb_rape$teams.away.leagueRecord.pct <- as.numeric(mlb_rape$teams.away.leagueRecord.pct)
mlb_rape$teams.home.leagueRecord.pct <- as.numeric(mlb_rape$teams.home.leagueRecord.pct)

mlb_rape <- mlb_rape %>%
  mutate(
    holiday = as.numeric(game_date %in% holidays$holiday_date)
  )

mlb_rape <- merge(mlb_rape,airquality, by = c("incident_date", "team"))
mlb_rape <- mlb_rape[mlb_rape$doubleHeader=="N",]

#intimidation
intimidation <- readRDS("data/processed/nibrs_domestic_counts_2009-2020_intimidation.rds")

mlb_intimidation <- merge(mlb_games_subset, intimidation, by = c("incident_date", "ori"), all.x = TRUE) %>%
  filter(team %in% c("CIN", "CLE", "COL", "DET", "KC", "MIL", "MIN", "SEA", "TEX")) %>%
  filter(!(team == "TEX" & incident_date < as.Date("2017-01-01"))) %>%
  filter(!(team == "KC" & incident_date < as.Date("2010-01-01"))) %>%
  filter(!(team == "MIN" & incident_date < as.Date("2019-01-01")))%>%
  filter(!(team == "SEA" & incident_date < as.Date("2012-01-01")))%>%
  mutate(across(c(dv_hour_0:dv_hour_23), ~replace_na(.x, 0)))

mlb_intimidation$dayofweek <- wday(mlb_intimidation$incident_date, label = TRUE)
#mlb_intimidation$dayofweek <- as.factor(mlb_intimidation$dayofweek)
mlb_intimidation$month <- month(mlb_intimidation$incident_date)
#mlb_intimidation$monthyear <- as.numeric(mlb_intimidation$month)
mlb_intimidation$year <- year(mlb_intimidation$incident_date)
#mlb_intimidation$year <- as.numeric(mlb_intimidation$year)
mlb_intimidation$elapsed_time_clean <- gsub("\\s*\\(.*?\\)", "", mlb_intimidation$elapsed_time)
mlb_intimidation$end_time <- hms(mlb_intimidation$start_time) + hm(mlb_intimidation$elapsed_time_clean)
mlb_intimidation$game_end_time <- hour(mlb_intimidation$end_time) + minute(mlb_intimidation$end_time)/60

mlb_intimidation$win <- ifelse(mlb_intimidation$hometeam==1 & mlb_intimidation$homewin==1|mlb_intimidation$hometeam==0 & mlb_intimidation$homewin==0,1,0)
mlb_intimidation$lose <- ifelse(mlb_intimidation$hometeam==1 & mlb_intimidation$homewin==0|mlb_intimidation$hometeam==0 & mlb_intimidation$homewin==1,1,0)
mlb_intimidation$accuracy <- mlb_intimidation$accuracy*100
mlb_intimidation$inaccuracy <- 100-mlb_intimidation$accuracy

mlb_intimidation <- mlb_intimidation %>%
  mutate(dvcount_10pmto6am = dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_8pmto6am = dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_6pmto6am = dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_4pmto4am = dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3,
         dvcount_2pmto2am = dv_hour_14+dv_hour_15+dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1,
         dvcount_noontomidnight = dv_hour_12+dv_hour_13+dv_hour_14+dv_hour_15+dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23,
         dvcount_noonto2pm = dv_hour_12+dv_hour_13,
         dvcount_2pmto4pm = dv_hour_14+dv_hour_15,
         dvcount_4pmto6pm = dv_hour_16+dv_hour_17,
         dvcount_6pmto8pm = dv_hour_17+dv_hour_18,
         dvcount_8pmto10pm = dv_hour_19+dv_hour_20,
         dvcount_10pmtomidnight = dv_hour_21+dv_hour_22,
         dvcount_midnightto2am = dv_hour_0+dv_hour_1,
         dvcount_2amto4am = dv_hour_2+dv_hour_3,
         dvcount_4amto6am = dv_hour_4+dv_hour_5)


mlb_intimidation$temperature <- as.numeric(mlb_intimidation$temperature)
mlb_intimidation$attendance <- as.numeric(gsub(",", "", mlb_intimidation$attendance))
mlb_intimidation$teams.away.leagueRecord.pct <- as.numeric(mlb_intimidation$teams.away.leagueRecord.pct)
mlb_intimidation$teams.home.leagueRecord.pct <- as.numeric(mlb_intimidation$teams.home.leagueRecord.pct)

mlb_intimidation <- mlb_intimidation %>%
  mutate(
    holiday = as.numeric(game_date %in% holidays$holiday_date)
  )

mlb_intimidation <- merge(mlb_intimidation,airquality, by = c("incident_date", "team"))
mlb_intimidation <- mlb_intimidation[mlb_intimidation$doubleHeader=="N",]

#robbery
robbery <- readRDS("data/processed/nibrs_domestic_counts_2009-2020_robbery.rds")

mlb_robbery <- merge(mlb_games_subset, robbery, by = c("incident_date", "ori"), all.x = TRUE) %>%
  filter(team %in% c("CIN", "CLE", "COL", "DET", "KC", "MIL", "MIN", "SEA", "TEX")) %>%
  filter(!(team == "TEX" & incident_date < as.Date("2017-01-01"))) %>%
  filter(!(team == "KC" & incident_date < as.Date("2010-01-01"))) %>%
  filter(!(team == "MIN" & incident_date < as.Date("2019-01-01")))%>%
  filter(!(team == "SEA" & incident_date < as.Date("2012-01-01")))%>%
  mutate(across(c(dv_hour_0:dv_hour_23), ~replace_na(.x, 0)))

mlb_robbery$dayofweek <- wday(mlb_robbery$incident_date, label = TRUE)
#mlb_robbery$dayofweek <- as.factor(mlb_robbery$dayofweek)
mlb_robbery$month <- month(mlb_robbery$incident_date)
#mlb_robbery$monthyear <- as.numeric(mlb_robbery$month)
mlb_robbery$year <- year(mlb_robbery$incident_date)
#mlb_robbery$year <- as.numeric(mlb_robbery$year)
mlb_robbery$elapsed_time_clean <- gsub("\\s*\\(.*?\\)", "", mlb_robbery$elapsed_time)
mlb_robbery$end_time <- hms(mlb_robbery$start_time) + hm(mlb_robbery$elapsed_time_clean)
mlb_robbery$game_end_time <- hour(mlb_robbery$end_time) + minute(mlb_robbery$end_time)/60

mlb_robbery$win <- ifelse(mlb_robbery$hometeam==1 & mlb_robbery$homewin==1|mlb_robbery$hometeam==0 & mlb_robbery$homewin==0,1,0)
mlb_robbery$lose <- ifelse(mlb_robbery$hometeam==1 & mlb_robbery$homewin==0|mlb_robbery$hometeam==0 & mlb_robbery$homewin==1,1,0)
mlb_robbery$accuracy <- mlb_robbery$accuracy*100
mlb_robbery$inaccuracy <- 100-mlb_robbery$accuracy

mlb_robbery <- mlb_robbery %>%
  mutate(dvcount_10pmto6am = dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_8pmto6am = dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_6pmto6am = dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_4pmto4am = dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3,
         dvcount_2pmto2am = dv_hour_14+dv_hour_15+dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1,
         dvcount_noontomidnight = dv_hour_12+dv_hour_13+dv_hour_14+dv_hour_15+dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23,
         dvcount_noonto2pm = dv_hour_12+dv_hour_13,
         dvcount_2pmto4pm = dv_hour_14+dv_hour_15,
         dvcount_4pmto6pm = dv_hour_16+dv_hour_17,
         dvcount_6pmto8pm = dv_hour_17+dv_hour_18,
         dvcount_8pmto10pm = dv_hour_19+dv_hour_20,
         dvcount_10pmtomidnight = dv_hour_21+dv_hour_22,
         dvcount_midnightto2am = dv_hour_0+dv_hour_1,
         dvcount_2amto4am = dv_hour_2+dv_hour_3,
         dvcount_4amto6am = dv_hour_4+dv_hour_5)


mlb_robbery$temperature <- as.numeric(mlb_robbery$temperature)
mlb_robbery$attendance <- as.numeric(gsub(",", "", mlb_robbery$attendance))
mlb_robbery$teams.away.leagueRecord.pct <- as.numeric(mlb_robbery$teams.away.leagueRecord.pct)
mlb_robbery$teams.home.leagueRecord.pct <- as.numeric(mlb_robbery$teams.home.leagueRecord.pct)

mlb_robbery <- mlb_robbery %>%
  mutate(
    holiday = as.numeric(game_date %in% holidays$holiday_date)
  )

mlb_robbery <- merge(mlb_robbery,airquality, by = c("incident_date", "team"))
mlb_robbery <- mlb_robbery[mlb_robbery$doubleHeader=="N",]

#pocketpicking
pocketpicking <- readRDS("data/processed/nibrs_domestic_counts_2009-2020_pocketpicking.rds")

mlb_pocketpicking <- merge(mlb_games_subset, pocketpicking, by = c("incident_date", "ori"), all.x = TRUE) %>%
  filter(team %in% c("CIN", "CLE", "COL", "DET", "KC", "MIL", "MIN", "SEA", "TEX")) %>%
  filter(!(team == "TEX" & incident_date < as.Date("2017-01-01"))) %>%
  filter(!(team == "KC" & incident_date < as.Date("2010-01-01"))) %>%
  filter(!(team == "MIN" & incident_date < as.Date("2019-01-01")))%>%
  filter(!(team == "SEA" & incident_date < as.Date("2012-01-01")))%>%
  mutate(across(c(dv_hour_0:dv_hour_23), ~replace_na(.x, 0)))

mlb_pocketpicking$dayofweek <- wday(mlb_pocketpicking$incident_date, label = TRUE)
#mlb_pocketpicking$dayofweek <- as.factor(mlb_pocketpicking$dayofweek)
mlb_pocketpicking$month <- month(mlb_pocketpicking$incident_date)
#mlb_pocketpicking$monthyear <- as.numeric(mlb_pocketpicking$month)
mlb_pocketpicking$year <- year(mlb_pocketpicking$incident_date)
#mlb_pocketpicking$year <- as.numeric(mlb_pocketpicking$year)
mlb_pocketpicking$elapsed_time_clean <- gsub("\\s*\\(.*?\\)", "", mlb_pocketpicking$elapsed_time)
mlb_pocketpicking$end_time <- hms(mlb_pocketpicking$start_time) + hm(mlb_pocketpicking$elapsed_time_clean)
mlb_pocketpicking$game_end_time <- hour(mlb_pocketpicking$end_time) + minute(mlb_pocketpicking$end_time)/60

mlb_pocketpicking$win <- ifelse(mlb_pocketpicking$hometeam==1 & mlb_pocketpicking$homewin==1|mlb_pocketpicking$hometeam==0 & mlb_pocketpicking$homewin==0,1,0)
mlb_pocketpicking$lose <- ifelse(mlb_pocketpicking$hometeam==1 & mlb_pocketpicking$homewin==0|mlb_pocketpicking$hometeam==0 & mlb_pocketpicking$homewin==1,1,0)
mlb_pocketpicking$accuracy <- mlb_pocketpicking$accuracy*100
mlb_pocketpicking$inaccuracy <- 100-mlb_pocketpicking$accuracy

mlb_pocketpicking <- mlb_pocketpicking %>%
  mutate(dvcount_10pmto6am = dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_8pmto6am = dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_6pmto6am = dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_4pmto4am = dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3,
         dvcount_2pmto2am = dv_hour_14+dv_hour_15+dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1,
         dvcount_noontomidnight = dv_hour_12+dv_hour_13+dv_hour_14+dv_hour_15+dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23,
         dvcount_noonto2pm = dv_hour_12+dv_hour_13,
         dvcount_2pmto4pm = dv_hour_14+dv_hour_15,
         dvcount_4pmto6pm = dv_hour_16+dv_hour_17,
         dvcount_6pmto8pm = dv_hour_17+dv_hour_18,
         dvcount_8pmto10pm = dv_hour_19+dv_hour_20,
         dvcount_10pmtomidnight = dv_hour_21+dv_hour_22,
         dvcount_midnightto2am = dv_hour_0+dv_hour_1,
         dvcount_2amto4am = dv_hour_2+dv_hour_3,
         dvcount_4amto6am = dv_hour_4+dv_hour_5)


mlb_pocketpicking$temperature <- as.numeric(mlb_pocketpicking$temperature)
mlb_pocketpicking$attendance <- as.numeric(gsub(",", "", mlb_pocketpicking$attendance))
mlb_pocketpicking$teams.away.leagueRecord.pct <- as.numeric(mlb_pocketpicking$teams.away.leagueRecord.pct)
mlb_pocketpicking$teams.home.leagueRecord.pct <- as.numeric(mlb_pocketpicking$teams.home.leagueRecord.pct)

mlb_pocketpicking <- mlb_pocketpicking %>%
  mutate(
    holiday = as.numeric(game_date %in% holidays$holiday_date)
  )

mlb_pocketpicking <- merge(mlb_pocketpicking,airquality, by = c("incident_date", "team"))
mlb_pocketpicking <- mlb_pocketpicking[mlb_pocketpicking$doubleHeader=="N",]

#simpleassault
simpleassault <- readRDS("data/processed/nibrs_domestic_counts_2009-2020_simpleassault.rds")

mlb_simpleassault <- merge(mlb_games_subset, simpleassault, by = c("incident_date", "ori"), all.x = TRUE) %>%
  filter(team %in% c("CIN", "CLE", "COL", "DET", "KC", "MIL", "MIN", "SEA", "TEX")) %>%
  filter(!(team == "TEX" & incident_date < as.Date("2017-01-01"))) %>%
  filter(!(team == "KC" & incident_date < as.Date("2010-01-01"))) %>%
  filter(!(team == "MIN" & incident_date < as.Date("2019-01-01")))%>%
  filter(!(team == "SEA" & incident_date < as.Date("2012-01-01")))%>%
  mutate(across(c(dv_hour_0:dv_hour_23), ~replace_na(.x, 0)))

mlb_simpleassault$dayofweek <- wday(mlb_simpleassault$incident_date, label = TRUE)
#mlb_simpleassault$dayofweek <- as.factor(mlb_simpleassault$dayofweek)
mlb_simpleassault$month <- month(mlb_simpleassault$incident_date)
#mlb_simpleassault$monthyear <- as.numeric(mlb_simpleassault$month)
mlb_simpleassault$year <- year(mlb_simpleassault$incident_date)
#mlb_simpleassault$year <- as.numeric(mlb_simpleassault$year)
mlb_simpleassault$elapsed_time_clean <- gsub("\\s*\\(.*?\\)", "", mlb_simpleassault$elapsed_time)
mlb_simpleassault$end_time <- hms(mlb_simpleassault$start_time) + hm(mlb_simpleassault$elapsed_time_clean)
mlb_simpleassault$game_end_time <- hour(mlb_simpleassault$end_time) + minute(mlb_simpleassault$end_time)/60

mlb_simpleassault$win <- ifelse(mlb_simpleassault$hometeam==1 & mlb_simpleassault$homewin==1|mlb_simpleassault$hometeam==0 & mlb_simpleassault$homewin==0,1,0)
mlb_simpleassault$lose <- ifelse(mlb_simpleassault$hometeam==1 & mlb_simpleassault$homewin==0|mlb_simpleassault$hometeam==0 & mlb_simpleassault$homewin==1,1,0)
mlb_simpleassault$accuracy <- mlb_simpleassault$accuracy*100
mlb_simpleassault$inaccuracy <- 100-mlb_simpleassault$accuracy

mlb_simpleassault <- mlb_simpleassault %>%
  mutate(dvcount_10pmto6am = dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_8pmto6am = dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_6pmto6am = dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_4pmto4am = dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3,
         dvcount_2pmto2am = dv_hour_14+dv_hour_15+dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1,
         dvcount_noontomidnight = dv_hour_12+dv_hour_13+dv_hour_14+dv_hour_15+dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23,
         dvcount_noonto2pm = dv_hour_12+dv_hour_13,
         dvcount_2pmto4pm = dv_hour_14+dv_hour_15,
         dvcount_4pmto6pm = dv_hour_16+dv_hour_17,
         dvcount_6pmto8pm = dv_hour_17+dv_hour_18,
         dvcount_8pmto10pm = dv_hour_19+dv_hour_20,
         dvcount_10pmtomidnight = dv_hour_21+dv_hour_22,
         dvcount_midnightto2am = dv_hour_0+dv_hour_1,
         dvcount_2amto4am = dv_hour_2+dv_hour_3,
         dvcount_4amto6am = dv_hour_4+dv_hour_5)


mlb_simpleassault$temperature <- as.numeric(mlb_simpleassault$temperature)
mlb_simpleassault$attendance <- as.numeric(gsub(",", "", mlb_simpleassault$attendance))
mlb_simpleassault$teams.away.leagueRecord.pct <- as.numeric(mlb_simpleassault$teams.away.leagueRecord.pct)
mlb_simpleassault$teams.home.leagueRecord.pct <- as.numeric(mlb_simpleassault$teams.home.leagueRecord.pct)

mlb_simpleassault <- mlb_simpleassault %>%
  mutate(
    holiday = as.numeric(game_date %in% holidays$holiday_date)
  )

mlb_simpleassault <- merge(mlb_simpleassault,airquality, by = c("incident_date", "team"))
mlb_simpleassault <- mlb_simpleassault[mlb_simpleassault$doubleHeader=="N",]

#aggravatedassault
aggravatedassault <- readRDS("data/processed/nibrs_domestic_counts_2009-2020_aggravatedassault.rds")

mlb_aggravatedassault <- merge(mlb_games_subset, aggravatedassault, by = c("incident_date", "ori"), all.x = TRUE) %>%
  filter(team %in% c("CIN", "CLE", "COL", "DET", "KC", "MIL", "MIN", "SEA", "TEX")) %>%
  filter(!(team == "TEX" & incident_date < as.Date("2017-01-01"))) %>%
  filter(!(team == "KC" & incident_date < as.Date("2010-01-01"))) %>%
  filter(!(team == "MIN" & incident_date < as.Date("2019-01-01")))%>%
  filter(!(team == "SEA" & incident_date < as.Date("2012-01-01")))%>%
  mutate(across(c(dv_hour_0:dv_hour_23), ~replace_na(.x, 0)))

mlb_aggravatedassault$dayofweek <- wday(mlb_aggravatedassault$incident_date, label = TRUE)
#mlb_aggravatedassault$dayofweek <- as.factor(mlb_aggravatedassault$dayofweek)
mlb_aggravatedassault$month <- month(mlb_aggravatedassault$incident_date)
#mlb_aggravatedassault$monthyear <- as.numeric(mlb_aggravatedassault$month)
mlb_aggravatedassault$year <- year(mlb_aggravatedassault$incident_date)
#mlb_aggravatedassault$year <- as.numeric(mlb_aggravatedassault$year)
mlb_aggravatedassault$elapsed_time_clean <- gsub("\\s*\\(.*?\\)", "", mlb_aggravatedassault$elapsed_time)
mlb_aggravatedassault$end_time <- hms(mlb_aggravatedassault$start_time) + hm(mlb_aggravatedassault$elapsed_time_clean)
mlb_aggravatedassault$game_end_time <- hour(mlb_aggravatedassault$end_time) + minute(mlb_aggravatedassault$end_time)/60

mlb_aggravatedassault$win <- ifelse(mlb_aggravatedassault$hometeam==1 & mlb_aggravatedassault$homewin==1|mlb_aggravatedassault$hometeam==0 & mlb_aggravatedassault$homewin==0,1,0)
mlb_aggravatedassault$lose <- ifelse(mlb_aggravatedassault$hometeam==1 & mlb_aggravatedassault$homewin==0|mlb_aggravatedassault$hometeam==0 & mlb_aggravatedassault$homewin==1,1,0)
mlb_aggravatedassault$accuracy <- mlb_aggravatedassault$accuracy*100
mlb_aggravatedassault$inaccuracy <- 100-mlb_aggravatedassault$accuracy

mlb_aggravatedassault <- mlb_aggravatedassault %>%
  mutate(dvcount_10pmto6am = dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_8pmto6am = dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_6pmto6am = dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_4pmto4am = dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3,
         dvcount_2pmto2am = dv_hour_14+dv_hour_15+dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1,
         dvcount_noontomidnight = dv_hour_12+dv_hour_13+dv_hour_14+dv_hour_15+dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23,
         dvcount_noonto2pm = dv_hour_12+dv_hour_13,
         dvcount_2pmto4pm = dv_hour_14+dv_hour_15,
         dvcount_4pmto6pm = dv_hour_16+dv_hour_17,
         dvcount_6pmto8pm = dv_hour_17+dv_hour_18,
         dvcount_8pmto10pm = dv_hour_19+dv_hour_20,
         dvcount_10pmtomidnight = dv_hour_21+dv_hour_22,
         dvcount_midnightto2am = dv_hour_0+dv_hour_1,
         dvcount_2amto4am = dv_hour_2+dv_hour_3,
         dvcount_4amto6am = dv_hour_4+dv_hour_5)


mlb_aggravatedassault$temperature <- as.numeric(mlb_aggravatedassault$temperature)
mlb_aggravatedassault$attendance <- as.numeric(gsub(",", "", mlb_aggravatedassault$attendance))
mlb_aggravatedassault$teams.away.leagueRecord.pct <- as.numeric(mlb_aggravatedassault$teams.away.leagueRecord.pct)
mlb_aggravatedassault$teams.home.leagueRecord.pct <- as.numeric(mlb_aggravatedassault$teams.home.leagueRecord.pct)

mlb_aggravatedassault <- mlb_aggravatedassault %>%
  mutate(
    holiday = as.numeric(game_date %in% holidays$holiday_date)
  )

mlb_aggravatedassault <- merge(mlb_aggravatedassault,airquality, by = c("incident_date", "team"))
mlb_aggravatedassault <- mlb_aggravatedassault[mlb_aggravatedassault$doubleHeader=="N",]

#allotherlarceny
allotherlarceny <- readRDS("data/processed/nibrs_domestic_counts_2009-2020_allotherlarceny.rds")

mlb_allotherlarceny <- merge(mlb_games_subset, allotherlarceny, by = c("incident_date", "ori"), all.x = TRUE) %>%
  filter(team %in% c("CIN", "CLE", "COL", "DET", "KC", "MIL", "MIN", "SEA", "TEX")) %>%
  filter(!(team == "TEX" & incident_date < as.Date("2017-01-01"))) %>%
  filter(!(team == "KC" & incident_date < as.Date("2010-01-01"))) %>%
  filter(!(team == "MIN" & incident_date < as.Date("2019-01-01")))%>%
  filter(!(team == "SEA" & incident_date < as.Date("2012-01-01")))%>%
  mutate(across(c(dv_hour_0:dv_hour_23), ~replace_na(.x, 0)))

mlb_allotherlarceny$dayofweek <- wday(mlb_allotherlarceny$incident_date, label = TRUE)
#mlb_allotherlarceny$dayofweek <- as.factor(mlb_allotherlarceny$dayofweek)
mlb_allotherlarceny$month <- month(mlb_allotherlarceny$incident_date)
#mlb_allotherlarceny$monthyear <- as.numeric(mlb_allotherlarceny$month)
mlb_allotherlarceny$year <- year(mlb_allotherlarceny$incident_date)
#mlb_allotherlarceny$year <- as.numeric(mlb_allotherlarceny$year)
mlb_allotherlarceny$elapsed_time_clean <- gsub("\\s*\\(.*?\\)", "", mlb_allotherlarceny$elapsed_time)
mlb_allotherlarceny$end_time <- hms(mlb_allotherlarceny$start_time) + hm(mlb_allotherlarceny$elapsed_time_clean)
mlb_allotherlarceny$game_end_time <- hour(mlb_allotherlarceny$end_time) + minute(mlb_allotherlarceny$end_time)/60

mlb_allotherlarceny$win <- ifelse(mlb_allotherlarceny$hometeam==1 & mlb_allotherlarceny$homewin==1|mlb_allotherlarceny$hometeam==0 & mlb_allotherlarceny$homewin==0,1,0)
mlb_allotherlarceny$lose <- ifelse(mlb_allotherlarceny$hometeam==1 & mlb_allotherlarceny$homewin==0|mlb_allotherlarceny$hometeam==0 & mlb_allotherlarceny$homewin==1,1,0)
mlb_allotherlarceny$accuracy <- mlb_allotherlarceny$accuracy*100
mlb_allotherlarceny$inaccuracy <- 100-mlb_allotherlarceny$accuracy

mlb_allotherlarceny <- mlb_allotherlarceny %>%
  mutate(dvcount_10pmto6am = dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_8pmto6am = dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_6pmto6am = dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_4pmto4am = dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3,
         dvcount_2pmto2am = dv_hour_14+dv_hour_15+dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1,
         dvcount_noontomidnight = dv_hour_12+dv_hour_13+dv_hour_14+dv_hour_15+dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23,
         dvcount_noonto2pm = dv_hour_12+dv_hour_13,
         dvcount_2pmto4pm = dv_hour_14+dv_hour_15,
         dvcount_4pmto6pm = dv_hour_16+dv_hour_17,
         dvcount_6pmto8pm = dv_hour_17+dv_hour_18,
         dvcount_8pmto10pm = dv_hour_19+dv_hour_20,
         dvcount_10pmtomidnight = dv_hour_21+dv_hour_22,
         dvcount_midnightto2am = dv_hour_0+dv_hour_1,
         dvcount_2amto4am = dv_hour_2+dv_hour_3,
         dvcount_4amto6am = dv_hour_4+dv_hour_5)


mlb_allotherlarceny$temperature <- as.numeric(mlb_allotherlarceny$temperature)
mlb_allotherlarceny$attendance <- as.numeric(gsub(",", "", mlb_allotherlarceny$attendance))
mlb_allotherlarceny$teams.away.leagueRecord.pct <- as.numeric(mlb_allotherlarceny$teams.away.leagueRecord.pct)
mlb_allotherlarceny$teams.home.leagueRecord.pct <- as.numeric(mlb_allotherlarceny$teams.home.leagueRecord.pct)

mlb_allotherlarceny <- mlb_allotherlarceny %>%
  mutate(
    holiday = as.numeric(game_date %in% holidays$holiday_date)
  )

mlb_allotherlarceny <- merge(mlb_allotherlarceny,airquality, by = c("incident_date", "team"))
mlb_allotherlarceny <- mlb_allotherlarceny[mlb_allotherlarceny$doubleHeader=="N",]

#drugnarcoticviolations
drugnarcoticviolations <- readRDS("data/processed/nibrs_domestic_counts_2009-2020_drugnarcoticviolations.rds")

mlb_drugnarcoticviolations <- merge(mlb_games_subset, drugnarcoticviolations, by = c("incident_date", "ori"), all.x = TRUE) %>%
  filter(team %in% c("CIN", "CLE", "COL", "DET", "KC", "MIL", "MIN", "SEA", "TEX")) %>%
  filter(!(team == "TEX" & incident_date < as.Date("2017-01-01"))) %>%
  filter(!(team == "KC" & incident_date < as.Date("2010-01-01"))) %>%
  filter(!(team == "MIN" & incident_date < as.Date("2019-01-01")))%>%
  filter(!(team == "SEA" & incident_date < as.Date("2012-01-01")))%>%
  mutate(across(c(dv_hour_0:dv_hour_23), ~replace_na(.x, 0)))

mlb_drugnarcoticviolations$dayofweek <- wday(mlb_drugnarcoticviolations$incident_date, label = TRUE)
#mlb_drugnarcoticviolations$dayofweek <- as.factor(mlb_drugnarcoticviolations$dayofweek)
mlb_drugnarcoticviolations$month <- month(mlb_drugnarcoticviolations$incident_date)
#mlb_drugnarcoticviolations$monthyear <- as.numeric(mlb_drugnarcoticviolations$month)
mlb_drugnarcoticviolations$year <- year(mlb_drugnarcoticviolations$incident_date)
#mlb_drugnarcoticviolations$year <- as.numeric(mlb_drugnarcoticviolations$year)
mlb_drugnarcoticviolations$elapsed_time_clean <- gsub("\\s*\\(.*?\\)", "", mlb_drugnarcoticviolations$elapsed_time)
mlb_drugnarcoticviolations$end_time <- hms(mlb_drugnarcoticviolations$start_time) + hm(mlb_drugnarcoticviolations$elapsed_time_clean)
mlb_drugnarcoticviolations$game_end_time <- hour(mlb_drugnarcoticviolations$end_time) + minute(mlb_drugnarcoticviolations$end_time)/60

mlb_drugnarcoticviolations$win <- ifelse(mlb_drugnarcoticviolations$hometeam==1 & mlb_drugnarcoticviolations$homewin==1|mlb_drugnarcoticviolations$hometeam==0 & mlb_drugnarcoticviolations$homewin==0,1,0)
mlb_drugnarcoticviolations$lose <- ifelse(mlb_drugnarcoticviolations$hometeam==1 & mlb_drugnarcoticviolations$homewin==0|mlb_drugnarcoticviolations$hometeam==0 & mlb_drugnarcoticviolations$homewin==1,1,0)
mlb_drugnarcoticviolations$accuracy <- mlb_drugnarcoticviolations$accuracy*100
mlb_drugnarcoticviolations$inaccuracy <- 100-mlb_drugnarcoticviolations$accuracy

mlb_drugnarcoticviolations <- mlb_drugnarcoticviolations %>%
  mutate(dvcount_10pmto6am = dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_8pmto6am = dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_6pmto6am = dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_4pmto4am = dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3,
         dvcount_2pmto2am = dv_hour_14+dv_hour_15+dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1,
         dvcount_noontomidnight = dv_hour_12+dv_hour_13+dv_hour_14+dv_hour_15+dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23,
         dvcount_noonto2pm = dv_hour_12+dv_hour_13,
         dvcount_2pmto4pm = dv_hour_14+dv_hour_15,
         dvcount_4pmto6pm = dv_hour_16+dv_hour_17,
         dvcount_6pmto8pm = dv_hour_17+dv_hour_18,
         dvcount_8pmto10pm = dv_hour_19+dv_hour_20,
         dvcount_10pmtomidnight = dv_hour_21+dv_hour_22,
         dvcount_midnightto2am = dv_hour_0+dv_hour_1,
         dvcount_2amto4am = dv_hour_2+dv_hour_3,
         dvcount_4amto6am = dv_hour_4+dv_hour_5)


mlb_drugnarcoticviolations$temperature <- as.numeric(mlb_drugnarcoticviolations$temperature)
mlb_drugnarcoticviolations$attendance <- as.numeric(gsub(",", "", mlb_drugnarcoticviolations$attendance))
mlb_drugnarcoticviolations$teams.away.leagueRecord.pct <- as.numeric(mlb_drugnarcoticviolations$teams.away.leagueRecord.pct)
mlb_drugnarcoticviolations$teams.home.leagueRecord.pct <- as.numeric(mlb_drugnarcoticviolations$teams.home.leagueRecord.pct)

mlb_drugnarcoticviolations <- mlb_drugnarcoticviolations %>%
  mutate(
    holiday = as.numeric(game_date %in% holidays$holiday_date)
  )

mlb_drugnarcoticviolations <- merge(mlb_drugnarcoticviolations,airquality, by = c("incident_date", "team"))
mlb_drugnarcoticviolations <- mlb_drugnarcoticviolations[mlb_drugnarcoticviolations$doubleHeader=="N",]

#destructiondamagevandalismofproperty
destructiondamagevandalismofproperty <- readRDS("data/processed/nibrs_domestic_counts_2009-2020_destructiondamagevandalismofproperty.rds")

mlb_destructiondamagevandalismofproperty <- merge(mlb_games_subset, destructiondamagevandalismofproperty, by = c("incident_date", "ori"), all.x = TRUE) %>%
  filter(team %in% c("CIN", "CLE", "COL", "DET", "KC", "MIL", "MIN", "SEA", "TEX")) %>%
  filter(!(team == "TEX" & incident_date < as.Date("2017-01-01"))) %>%
  filter(!(team == "KC" & incident_date < as.Date("2010-01-01"))) %>%
  filter(!(team == "MIN" & incident_date < as.Date("2019-01-01")))%>%
  filter(!(team == "SEA" & incident_date < as.Date("2012-01-01")))%>%
  mutate(across(c(dv_hour_0:dv_hour_23), ~replace_na(.x, 0)))

mlb_destructiondamagevandalismofproperty$dayofweek <- wday(mlb_destructiondamagevandalismofproperty$incident_date, label = TRUE)
#mlb_destructiondamagevandalismofproperty$dayofweek <- as.factor(mlb_destructiondamagevandalismofproperty$dayofweek)
mlb_destructiondamagevandalismofproperty$month <- month(mlb_destructiondamagevandalismofproperty$incident_date)
#mlb_destructiondamagevandalismofproperty$monthyear <- as.numeric(mlb_destructiondamagevandalismofproperty$month)
mlb_destructiondamagevandalismofproperty$year <- year(mlb_destructiondamagevandalismofproperty$incident_date)
#mlb_destructiondamagevandalismofproperty$year <- as.numeric(mlb_destructiondamagevandalismofproperty$year)
mlb_destructiondamagevandalismofproperty$elapsed_time_clean <- gsub("\\s*\\(.*?\\)", "", mlb_destructiondamagevandalismofproperty$elapsed_time)
mlb_destructiondamagevandalismofproperty$end_time <- hms(mlb_destructiondamagevandalismofproperty$start_time) + hm(mlb_destructiondamagevandalismofproperty$elapsed_time_clean)
mlb_destructiondamagevandalismofproperty$game_end_time <- hour(mlb_destructiondamagevandalismofproperty$end_time) + minute(mlb_destructiondamagevandalismofproperty$end_time)/60

mlb_destructiondamagevandalismofproperty$win <- ifelse(mlb_destructiondamagevandalismofproperty$hometeam==1 & mlb_destructiondamagevandalismofproperty$homewin==1|mlb_destructiondamagevandalismofproperty$hometeam==0 & mlb_destructiondamagevandalismofproperty$homewin==0,1,0)
mlb_destructiondamagevandalismofproperty$lose <- ifelse(mlb_destructiondamagevandalismofproperty$hometeam==1 & mlb_destructiondamagevandalismofproperty$homewin==0|mlb_destructiondamagevandalismofproperty$hometeam==0 & mlb_destructiondamagevandalismofproperty$homewin==1,1,0)
mlb_destructiondamagevandalismofproperty$accuracy <- mlb_destructiondamagevandalismofproperty$accuracy*100
mlb_destructiondamagevandalismofproperty$inaccuracy <- 100-mlb_destructiondamagevandalismofproperty$accuracy

mlb_destructiondamagevandalismofproperty <- mlb_destructiondamagevandalismofproperty %>%
  mutate(dvcount_10pmto6am = dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_8pmto6am = dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_6pmto6am = dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3+dv_hour_4+dv_hour_5,
         dvcount_4pmto4am = dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1+dv_hour_2+dv_hour_3,
         dvcount_2pmto2am = dv_hour_14+dv_hour_15+dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23+dv_hour_0+dv_hour_1,
         dvcount_noontomidnight = dv_hour_12+dv_hour_13+dv_hour_14+dv_hour_15+dv_hour_16+dv_hour_17+dv_hour_18+dv_hour_19+dv_hour_20+dv_hour_21+dv_hour_22+dv_hour_23,
         dvcount_noonto2pm = dv_hour_12+dv_hour_13,
         dvcount_2pmto4pm = dv_hour_14+dv_hour_15,
         dvcount_4pmto6pm = dv_hour_16+dv_hour_17,
         dvcount_6pmto8pm = dv_hour_17+dv_hour_18,
         dvcount_8pmto10pm = dv_hour_19+dv_hour_20,
         dvcount_10pmtomidnight = dv_hour_21+dv_hour_22,
         dvcount_midnightto2am = dv_hour_0+dv_hour_1,
         dvcount_2amto4am = dv_hour_2+dv_hour_3,
         dvcount_4amto6am = dv_hour_4+dv_hour_5)


mlb_destructiondamagevandalismofproperty$temperature <- as.numeric(mlb_destructiondamagevandalismofproperty$temperature)
mlb_destructiondamagevandalismofproperty$attendance <- as.numeric(gsub(",", "", mlb_destructiondamagevandalismofproperty$attendance))
mlb_destructiondamagevandalismofproperty$teams.away.leagueRecord.pct <- as.numeric(mlb_destructiondamagevandalismofproperty$teams.away.leagueRecord.pct)
mlb_destructiondamagevandalismofproperty$teams.home.leagueRecord.pct <- as.numeric(mlb_destructiondamagevandalismofproperty$teams.home.leagueRecord.pct)

mlb_destructiondamagevandalismofproperty <- mlb_destructiondamagevandalismofproperty %>%
  mutate(
    holiday = as.numeric(game_date %in% holidays$holiday_date)
  )

mlb_destructiondamagevandalismofproperty <- merge(mlb_destructiondamagevandalismofproperty,airquality, by = c("incident_date", "team"))
mlb_destructiondamagevandalismofproperty <- mlb_destructiondamagevandalismofproperty[mlb_destructiondamagevandalismofproperty$doubleHeader=="N",]


models <- list(
    "Simple Assault" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_simpleassault),
    "Intimidation" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_intimidation),
    "Aggravated Assault" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_aggravatedassault),
    "Rape" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_rape),
    "Robbery" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_robbery),
    "Female-to-Male" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv_female_to_male))

modelsummary(
  models,
  coef_omit = "^(?!top25inaccurate:lose:disfavored$)",
  stars = c("*"=.1,"**"=.05,"***"=.01),
  gof_map = gofmap,
  title = "Panel A: Violent Crime",
  output = "tables/all_crime_panelA.tex"
)


models <- list(
    "Larceny" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_allotherlarceny),
    "Pocket Picking" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_pocketpicking),
    "Drugs" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_drugnarcoticviolations),
    "Vandalism" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_destructiondamagevandalismofproperty))

modelsummary(
  models,
  coef_omit = "^(?!top25inaccurate:lose:disfavored$)",
  stars = c("*"=.1,"**"=.05,"***"=.01),
  gof_map = gofmap,
  title = "Panel A: Violent Crime",
  output = "tables/all_crime_panelB.tex"
)




# models <- list(
#   "Panel A: Rape" = list(
#     "(1)" = fepois(dvcount_6pmto6am~inaccuracy,vcov = ~team,data=mlb_rape[mlb_rape$lose==1,]),
#     "(2)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_rape[mlb_rape$lose==1,]),
#     "(3)" = fepois(dvcount_6pmto6am~inaccuracy|team+year+month+dayofweek,vcov = ~team,data=mlb_rape[mlb_rape$lose==1,]),
#     "(4)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_rape[mlb_rape$lose==1,]),
#     "(5)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_rape[mlb_rape$lose==1,]),
#     "(6)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_rape[mlb_rape$lose==1,])),
#   "Panel B: Intimidation" = list(
#     "(1)" = fepois(dvcount_6pmto6am~inaccuracy,vcov = ~team,data=mlb_intimidation[mlb_intimidation$lose==1,]),
#     "(2)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_intimidation[mlb_intimidation$lose==1,]),
#     "(3)" = fepois(dvcount_6pmto6am~inaccuracy|team+year+month+dayofweek,vcov = ~team,data=mlb_intimidation[mlb_intimidation$lose==1,]),
#     "(4)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_intimidation[mlb_intimidation$lose==1,]),
#     "(5)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_intimidation[mlb_intimidation$lose==1,]),
#     "(6)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_intimidation[mlb_intimidation$lose==1,])),
#   "Panel C: Robbery" = list(
#     "(1)" = fepois(dvcount_6pmto6am~inaccuracy,vcov = ~team,data=mlb_robbery[mlb_robbery$lose==1,]),
#     "(2)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_robbery[mlb_robbery$lose==1,]),
#     "(3)" = fepois(dvcount_6pmto6am~inaccuracy|team+year+month+dayofweek,vcov = ~team,data=mlb_robbery[mlb_robbery$lose==1,]),
#     "(4)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_robbery[mlb_robbery$lose==1,]),
#     "(5)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_robbery[mlb_robbery$lose==1,]),
#     "(6)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_robbery[mlb_robbery$lose==1,]))
# )
# 
# gofmap <- tribble(~raw,        ~clean,          ~fmt,
#                   "nobs",      "N",             0,
#                   "r.squared", "R<sup>2</sup>", 2,
#                   "aic","AIC",0)
# 
# modelsummary(models,
#              stars = c("*"=.1,"**"=.05,"***"=.01),
#              coef_omit = "^(?!.*lose|inaccuracy)",
#              gof_map = gofmap,
#              shape = "rbind",
#              title = "All Crime"
#              ,output="tables\\Female-to-male_DV.tex")

# ref report


# separating out bias vs inaccuracy
mlb_dv <- mlb_dv %>%
  mutate(year = year(game_date)) %>%  # extract year from game date
  group_by(year) %>%                  # calculate quantiles within each year
  mutate(
    p5  = quantile(disfavor, 0.05, na.rm = TRUE),
    p25 = quantile(disfavor, 0.25, na.rm = TRUE),
    p50 = quantile(disfavor, 0.50, na.rm = TRUE),
    p75 = quantile(disfavor, 0.75, na.rm = TRUE),
    p95 = quantile(disfavor, 0.95, na.rm = TRUE),
    # Assign each observation to a bin based on disfavor percentile
    disfavor_bin = case_when(
      disfavor <= p5  ~ "5%",
      disfavor <= p25 ~ "25%",
      disfavor <= p50 ~ "50%",
      disfavor <= p75 ~ "75%",
      disfavor <= p95 ~ "95%",
      TRUE              ~ "Above 95%"
    )
  ) %>%
  ungroup() %>%
  # Ensure factor levels are ordered logically for tables/plots
  mutate(disfavor_bin = factor(
    disfavor_bin,
    levels = c("5%", "25%", "50%", "75%", "95%", "Above 95%")
  )) %>%
  # Remove temporary quantile columns
  select(-p5, -p25, -p50, -p75, -p95)

mlb_dv$top25inaccurate <- ifelse(mlb_dv$inaccuracy_bin=="Above 95%"|mlb_dv$inaccuracy_bin=="95%",1,0)
mlb_dv$top25disfavor <- ifelse(mlb_dv$disfavor_bin=="Above 95%"|mlb_dv$disfavor_bin=="95%",1,0)

# Specifying Poisson models main results
# ------------------------------------------------------------------------------
rows <- tribble(
  ~term,        ~"1",      ~"2",        ~"3",   ~"4",       ~"5", ~"6",
  'Controls',    'No',    'Yes',     'No',     'Yes',     'Yes','Yes',   
  'Day-of-Week Fixed Effects',    'No',    'No',  'Yes',   'Yes',       'Yes','Yes',    
  'City Fixed Effects',    'No',    'No',     'Yes',  'Yes',       '—','—',    
  'Year Fixed Effects',    'No',    'No',     'Yes',  'Yes',       '—','—',    
  'Month Fixed Effects',    'No',    'No',     'Yes',   'Yes',       'Yes','—',      
  'City-by-Year Fixed Effects',    'No',    'No',     'No',  'No',       'Yes','—',    
  'City-by-Year-by-Month Fixed Effects',    'No',    'No',  'No',   'No',      'No','Yes'
)


attr(rows, 'position') <- c()

gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "aic","AIC",0)

models <- list(
  "(1)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored,vcov = ~team,data=mlb_dv),
  "(2)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv),
  "(3)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored|team+year+month+dayofweek,vcov = ~team,data=mlb_dv),
  "(4)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv),
  "(5)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv),
  "(6)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv)
)

modelsummary(
  models,
  coef_omit = "^(?!top25inaccurate|disfavored|lose)",
  estimate = "{estimate}{stars}",
  stars = c('*' = .1,'**'=.05, '***' = .01),
  gof_map = gm,
  add_rows = rows,
  output = "tables\\triple_interaction_referee.tex")

# Specifying Poisson models for day of week and month analysis
# ------------------------------------------------------------------------------
models <- list(
  "Panel A: Effects Across Days" = list(
    "Mon" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI|month+year^team,vcov = ~team,data=mlb_dv[mlb_dv$dayofweek=="Mon",]),
    "Tue" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI|month+year^team,vcov = ~team,data=mlb_dv[mlb_dv$dayofweek=="Tue",]),
    "Wed" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI|month+year^team,vcov = ~team,data=mlb_dv[mlb_dv$dayofweek=="Wed",]),
    "Thu" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI|month+year^team,vcov = ~team,data=mlb_dv[mlb_dv$dayofweek=="Thu",]),
    "Fri" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI|month+year^team,vcov = ~team,data=mlb_dv[mlb_dv$dayofweek=="Fri",]),
    "Sat" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI|month+year^team,vcov = ~team,data=mlb_dv[mlb_dv$dayofweek=="Sat",]),
    "Sun" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI|month+year^team,vcov = ~team,data=mlb_dv[mlb_dv$dayofweek=="Sun",])),
  "Panel B: Effects Across Months" = list(
    "Apr" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|dayofweek+year^team,vcov = ~team,data=mlb_dv[mlb_dv$month=="4",]),
    "May" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|dayofweek+year^team,vcov = ~team,data=mlb_dv[mlb_dv$month=="5",]),
    "June" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|dayofweek+year^team,vcov = ~team,data=mlb_dv[mlb_dv$month=="6",]),
    "July" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|dayofweek+year^team,vcov = ~team,data=mlb_dv[mlb_dv$month=="7",]),
    "Aug" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|dayofweek+year^team,vcov = ~team,data=mlb_dv[mlb_dv$month=="8",]),
    "Sept" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|dayofweek+year^team,vcov = ~team,data=mlb_dv[mlb_dv$month=="9",]),
    "Oct" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|dayofweek+year^team,vcov = ~team,data=mlb_dv[mlb_dv$month=="10",]))
)

modelsummary(models,
             stars = c("*"=.1,"**"=.05,"***"=.01),
             coef_omit = "^(?!top25inaccurate:lose:disfavored$)",
             gof_map = gofmap,
             title = "Effects of Umpire Accuracy on Domestic Violence At Different Times",
             shape = "rbind",
             output="tables\\week_and_month_analysis_referee.tex")

uppertempbound <- quantile(mlb_dv$temperature, probs = 0.90, na.rm = TRUE)

# Specifying Poisson models robustness checks
# ------------------------------------------------------------------------------
rows <- tribble(
  ~term,        ~"1",      ~"2",        ~"3",   ~"4",       ~"5", ~"6",
  'Controls',    'No',    'Yes',     'No',     'Yes',     'Yes','Yes',   
  'Day-of-Week Fixed Effects',    'No',    'No',  'Yes',   'Yes',       'Yes','Yes',    
  'City Fixed Effects',    'No',    'No',     'Yes',  'Yes',       '—','—',    
  'Year Fixed Effects',    'No',    'No',     'Yes',  'Yes',       '—','—',    
  'Month Fixed Effects',    'No',    'No',     'Yes',   'Yes',       'Yes','—',      
  'City-by-Year Fixed Effects',    'No',    'No',     'No',  'No',       'Yes','—',    
  'City-by-Year-by-Month Fixed Effects',    'No',    'No',  'No',   'No',      'No','Yes'
)


attr(rows, 'position') <- c()

gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "aic","AIC",0)

models <- list(
  "Panel A: home games" = list(
    "(1)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored,vcov = ~team,data=mlb_dv[mlb_dv$hometeam==1,]),
    "(2)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv[mlb_dv$hometeam==1,]),
    "(3)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$hometeam==1,]),
    "(4)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$hometeam==1,]),
    "(5)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$hometeam==1,]),
    "(6)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$hometeam==1,])),
  "Panel B: away games" = list(
    "(1)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored,vcov = ~team,data=mlb_dv[mlb_dv$hometeam==0,]),
    "(2)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv[mlb_dv$hometeam==0,]),
    "(3)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$hometeam==0,]),
    "(4)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$hometeam==0,]),
    "(5)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$hometeam==0,]),
    "(6)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$hometeam==0,])),
  "Panel C: AIR QUALITY GOOD OR MODERATE" = list(
  "(1)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored,vcov = ~team,data=mlb_dv[mlb_dv$AQI_Category=="Good"|mlb_dv$AQI_Category=="Moderate",]),
  "(2)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv[mlb_dv$AQI_Category=="Good"|mlb_dv$AQI_Category=="Moderate",]),
  "(3)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$AQI_Category=="Good"|mlb_dv$AQI_Category=="Moderate",]),
  "(4)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$AQI_Category=="Good"|mlb_dv$AQI_Category=="Moderate",]),
  "(5)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$AQI_Category=="Good"|mlb_dv$AQI_Category=="Moderate",]),
  "(6)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$AQI_Category=="Good"|mlb_dv$AQI_Category=="Moderate",])),
  "Panel D: Temperature NORMAL" = list(
    "(1)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored,vcov = ~team,data=mlb_dv[mlb_dv$temperature<uppertempbound,]),
    "(2)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv[mlb_dv$temperature<uppertempbound,]),
    "(3)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$temperature<uppertempbound,]),
    "(4)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$temperature<uppertempbound,]),
    "(5)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$temperature<uppertempbound,]),
    "(6)" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$temperature<uppertempbound,])))
    
    
modelsummary(
  models,
  coef_omit = "^(?!top25inaccurate:lose:disfavored)",
  estimate = "{estimate}{stars}",
  stars = c('*' = .1,'**'=.05, '***' = .01),
  gof_map = gm,
  add_rows = rows,
  shape = "rbind",
  output = "tables\\triple_interaction_robustness_referee.tex")


# Varying timing
# ------------------------------------------------------------------------------
models <- list(
  "6pm - 6am" = fepois(dvcount_6pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv),
  "4pm - 4am" = fepois(dvcount_4pmto4am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv),
  "8pm - 6am" = fepois(dvcount_8pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv),
  "10pm - 6am" = fepois(dvcount_10pmto6am~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv),
  "post game" = fepois(dv_post_game~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+game_end_time+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv),
  "pre game" = fepois(dv_pre_game~top25inaccurate*lose*disfavored+hometeam+temperature+attendance+game_end_time+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv))


modelsummary(models,
             stars = c("*"=.1,"**"=.05,"***"=.01),
             coef_omit = "^(?!top25inaccurate:lose:disfavored$)",
             gof_map = gofmap,
             title = "Varying Timeframe for Domestic Violence"
             ,output = "tables\\time_variation_robustness_check.tex")









attr(rows, 'position') <- c()

gm <- tibble::tribble(
  ~raw,        ~clean,          ~fmt,
  "nobs",      "N",             0,
  "aic","AIC",0)

models <- list(
  "Panel A: Wins and Losses Combined" = list(
    "(1)" = fepois(dvcount_6pmto6am~inaccuracy,vcov = ~team,data=mlb_dv[mlb_dv$AQI_Category=="Good",]),
    "(2)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv[mlb_dv$AQI_Category=="Good",]),
    "(3)" = fepois(dvcount_6pmto6am~inaccuracy|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$AQI_Category=="Good",]),
    "(4)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$AQI_Category=="Good",]),
    "(5)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$AQI_Category=="Good",]),
    "(6)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$AQI_Category=="Good",]) 
  ),
  "Panel B: Losses" = list(
    "(1)" = fepois(dvcount_6pmto6am~inaccuracy,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$AQI_Category=="Good",]),
    "(2)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$AQI_Category=="Good",]),
    "(3)" = fepois(dvcount_6pmto6am~inaccuracy|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$AQI_Category=="Good",]),
    "(4)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$AQI_Category=="Good",]),
    "(5)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$AQI_Category=="Good",]),
    "(6)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$AQI_Category=="Good",]) 
  ),
  "Panel C: Wins" = list(
    "(1)" = fepois(dvcount_6pmto6am~inaccuracy,vcov = ~team,data=mlb_dv[mlb_dv$lose==0&mlb_dv$AQI_Category=="Good",]),
    "(2)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv[mlb_dv$lose==0&mlb_dv$AQI_Category=="Good",]),
    "(3)" = fepois(dvcount_6pmto6am~inaccuracy|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==0&mlb_dv$AQI_Category=="Good",]),
    "(4)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==0&mlb_dv$AQI_Category=="Good",]),
    "(5)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==0&mlb_dv$AQI_Category=="Good",]),
    "(6)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==0&mlb_dv$AQI_Category=="Good",])
  ))

modelsummary(models,
             coef_omit = "^(?!.*inaccuracy)",
             coef_rename = c("inaccuracy" = "Inaccuracy"),
             estimate = "{estimate}{stars}",
             stars = c('*' = .1,'**'=.05, '***' = .01),
             gof_map = gm,
             shape = "rbind",
             add_rows = rows,
             output="tables\\airqualitygood_referee.tex")

lowertempbound <- quantile(mlb_dv$temperature, probs = 0.05, na.rm = TRUE)
uppertempbound <- quantile(mlb_dv$temperature, probs = 0.90, na.rm = TRUE)

models <- list(
  "Panel A: Wins and Losses Combined" = list(
    "(1)" = fepois(dvcount_6pmto6am~inaccuracy,vcov = ~team,data=mlb_dv[mlb_dv$temperature<uppertempbound,]),
    "(2)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv[mlb_dv$temperature<uppertempbound,]),
    "(3)" = fepois(dvcount_6pmto6am~inaccuracy|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$temperature<uppertempbound,]),
    "(4)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$temperature<uppertempbound,]),
    "(5)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$temperature<uppertempbound,]),
    "(6)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$temperature<uppertempbound,]) 
  ),
  "Panel B: Losses" = list(
    "(1)" = fepois(dvcount_6pmto6am~inaccuracy,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$temperature<uppertempbound,]),
    "(2)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$temperature<uppertempbound,]),
    "(3)" = fepois(dvcount_6pmto6am~inaccuracy|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$temperature<uppertempbound,]),
    "(4)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$temperature<uppertempbound,]),
    "(5)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$temperature<uppertempbound,]),
    "(6)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$temperature<uppertempbound,]) 
  ),
  "Panel C: Wins" = list(
    "(1)" = fepois(dvcount_6pmto6am~inaccuracy,vcov = ~team,data=mlb_dv[mlb_dv$lose==0&mlb_dv$temperature<uppertempbound,]),
    "(2)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv[mlb_dv$lose==0&mlb_dv$temperature<uppertempbound,]),
    "(3)" = fepois(dvcount_6pmto6am~inaccuracy|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==0&mlb_dv$temperature<uppertempbound,]),
    "(4)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==0&mlb_dv$temperature<uppertempbound,]),
    "(5)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==0&mlb_dv$temperature<uppertempbound,]),
    "(6)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==0&mlb_dv$temperature<uppertempbound,])
  ))

modelsummary(models,
             coef_omit = "^(?!.*inaccuracy)",
             coef_rename = c("inaccuracy" = "Inaccuracy"),
             estimate = "{estimate}{stars}",
             stars = c('*' = .1,'**'=.05, '***' = .01),
             gof_map = gm,
             shape = "rbind",
             add_rows = rows,
             output="tables\\temperaturebottom90_referee.tex")

# home vs away games
models <- list(
  "Panel A: Home and Away Losses Combined" = list(
    "(1)" = fepois(dvcount_6pmto6am~inaccuracy,vcov = ~team,data=mlb_dv[mlb_dv$lose==1,]),
    "(2)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv[mlb_dv$lose==1,]),
    "(3)" = fepois(dvcount_6pmto6am~inaccuracy|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1,]),
    "(4)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1,]),
    "(5)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1,]),
    "(6)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1,]) 
  ),
  "Panel B: Home Losses" = list(
    "(1)" = fepois(dvcount_6pmto6am~inaccuracy,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$hometeam==1,]),
    "(2)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$hometeam==1,]),
    "(3)" = fepois(dvcount_6pmto6am~inaccuracy|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$hometeam==1,]),
    "(4)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$hometeam==1,]),
    "(5)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$hometeam==1,]),
    "(6)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$hometeam==1,]) 
  ),
  "Panel C: Away Losses" = list(
    "(1)" = fepois(dvcount_6pmto6am~inaccuracy,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$hometeam==0,]),
    "(2)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$hometeam==0,]),
    "(3)" = fepois(dvcount_6pmto6am~inaccuracy|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$hometeam==0,]),
    "(4)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$hometeam==0,]),
    "(5)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$hometeam==0,]),
    "(6)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1&mlb_dv$hometeam==0,])
  ))

modelsummary(models,
             coef_omit = "^(?!.*inaccuracy)",
             coef_rename = c("inaccuracy" = "Inaccuracy"),
             estimate = "{estimate}{stars}",
             stars = c('*' = .1,'**'=.05, '***' = .01),
             gof_map = gm,
             shape = "rbind",
             add_rows = rows,
             output="tables\\homeandaway_referee.tex")

# changing FE
models <- list(
  "Panel A: Wins and Losses Combined" = list(
    "(1)" = fepois(dvcount_6pmto6am~inaccuracy,vcov = ~team,data=mlb_dv),
    "(2)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv),
    "(3)" = fepois(dvcount_6pmto6am~inaccuracy|team+year+month+dayofweek,vcov = ~team,data=mlb_dv),
    "(4)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv),
    "(5)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv),
    "(6)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv) 
  ),
  "Panel B: Losses" = list(
    "(1)" = fepois(dvcount_6pmto6am~inaccuracy,vcov = ~team,data=mlb_dv[mlb_dv$lose==1,]),
    "(2)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv[mlb_dv$lose==1,]),
    "(3)" = fepois(dvcount_6pmto6am~inaccuracy|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1,]),
    "(4)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1,]),
    "(5)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1,]),
    "(6)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==1,]) 
  ),
  "Panel C: Wins" = list(
    "(1)" = fepois(dvcount_6pmto6am~inaccuracy,vcov = ~team,data=mlb_dv[mlb_dv$lose==0,]),
    "(2)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday,vcov = ~team,data=mlb_dv[mlb_dv$lose==0,]),
    "(3)" = fepois(dvcount_6pmto6am~inaccuracy|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==0,]),
    "(4)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team+year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==0,]),
    "(5)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^year+month+dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==0,]),
    "(6)" = fepois(dvcount_6pmto6am~inaccuracy+hometeam+temperature+attendance+teams.home.leagueRecord.pct+teams.away.leagueRecord.pct+game_end_time+AQI+holiday|team^month+team^year+team^dayofweek,vcov = ~team,data=mlb_dv[mlb_dv$lose==0,])
  ))

rows <- tribble(
  ~term,        ~"1",      ~"2",        ~"3",   ~"4",       ~"5", ~"6",
  'Controls',    'No',    'Yes',     'No',     'Yes',     'Yes','Yes',   
  'Day-of-Week Fixed Effects',    'No',    'No',  'Yes',   'Yes',       'Yes','—',    
  'City Fixed Effects',    'No',    'No',     'Yes',  'Yes',       '—','—',    
  'Year Fixed Effects',    'No',    'No',     'Yes',  'Yes',       '—','—',    
  'Month Fixed Effects',    'No',    'No',     'Yes',   'Yes',       'Yes','—',      
  'City-by-Year Fixed Effects',    'No',    'No',     'No',  'No',       'Yes','Yes',    
  'City-by-Month Fixed Effects',    'No',    'No',  'No',   'No',      'No','Yes',
  'City-by-Day-of-Week Fixed Effects',    'No',    'No',  'No',   'No',      'No','Yes'
)

modelsummary(models,
             coef_omit = "^(?!.*inaccuracy)",
             coef_rename = c("inaccuracy" = "Inaccuracy"),
             estimate = "{estimate}{stars}",
             stars = c('*' = .1,'**'=.05, '***' = .01),
             gof_map = gm,
             shape = "rbind",
             add_rows = rows,
             output = "tables\\changingFE_referee.tex"
)

