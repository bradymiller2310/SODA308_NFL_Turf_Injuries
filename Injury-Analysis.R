library(nflreadr)
library(dplyr)
library(stringr)
library(tidyverse)

# Loading in 2024 play by play data
pbp_2024 <- load_pbp(2024)

# Loading in 2024 injury data
injuries_2024 <- load_injuries(2024)

pbp_2023 <- load_pbp(2023)
injuries_2023 <- load_injuries(2023)
pbp_2022 <- load_pbp(2022)
injuries_2022 <- load_injuries(2022)
pbp_2021 <- load_pbp(2021)
injuries_2021 <- load_injuries(2021)
pbp_2020 <- load_pbp(2020)
injuries_2020 <- load_injuries(2020)
pbp_2019 <- load_pbp(2019)
injuries_2019 <- load_injuries(2019)
pbp_2018 <- load_pbp(2018)
injuries_2018 <- load_injuries(2018)
pbp_2017 <- load_pbp(2017)
injuries_2017 <- load_injuries(2017)
pbp_2016 <- load_pbp(2016)
injuries_2016 <- load_injuries(2016)
pbp_2015 <- load_pbp(2015)
injuries_2015 <- load_injuries(2015)
pbp_2014 <- load_pbp(2014)
injuries_2014 <- load_injuries(2014)
pbp_2013 <- load_pbp(2013)
injuries_2013 <- load_injuries(2013)
pbp_2012 <- load_pbp(2012)
injuries_2012 <- load_injuries(2012)
pbp_2011 <- load_pbp(2011)
injuries_2011 <- load_injuries(2011)
pbp_2010 <- load_pbp(2010)
injuries_2010 <- load_injuries(2010)
# pbp_2009 <- load_pbp(2009)
# injuries_2009 <- load_injuries(2009)



pbp <- rbind(pbp_2010, pbp_2011,
             pbp_2012, pbp_2013, pbp_2014, pbp_2015, pbp_2016, pbp_2017,
             pbp_2018, pbp_2019, pbp_2020, pbp_2021, pbp_2022, pbp_2023, 
             pbp_2024)

injuries <- rbind(injuries_2010, injuries_2011,
                  injuries_2012, injuries_2013, injuries_2014, injuries_2015, 
                  injuries_2016, injuries_2017, injuries_2018, injuries_2019,
                  injuries_2020, injuries_2021, injuries_2022, injuries_2023, 
                  injuries_2024)


rm(pbp_2010, pbp_2011,
   pbp_2012, pbp_2013, pbp_2014, pbp_2015, pbp_2016, pbp_2017,
   pbp_2018, pbp_2019, pbp_2020, pbp_2021, pbp_2022, pbp_2023, 
   pbp_2024, 
   injuries_2010, injuries_2011,
   injuries_2012, injuries_2013, injuries_2014, injuries_2015, 
   injuries_2016, injuries_2017, injuries_2018, injuries_2019,
   injuries_2020, injuries_2021, injuries_2022, injuries_2023, 
   injuries_2024)

extract_injury_info <- function(desc) {
  # Regular expression to capture team abbreviation and player's abbreviated name
  match <- str_match(desc, "(\\b[A-Z]{2,3}\\b)-\\d+-([A-Z]{1,2}\\.[A-Za-z]+)\\swas")
  if (!is.na(match[1])) {
    team_abbr <- match[2]
    player_name <- match[3]
  } else {
    team_abbr <- NA
    player_name <- NA
  }
  return(c(player_name, team_abbr))
}


pbp <- pbp %>%
  rowwise() %>%
  mutate(
    injury_info = list(extract_injury_info(desc)),
    player_injured = injury_info[[1]],
    player_inj_team = injury_info[[2]]
  ) %>%
  select(-injury_info) 


# Getting abbreviated name from injury data set
injuries <- injuries %>%
  mutate(
    abv_name = paste0(str_sub(full_name, 1, 1), ".", word(full_name, 2)),
    date_only = substr(date_modified, 1, 10)
  )



# Joining injury and play by play data
# ONLY DOING 2024 FOR NOW - NOT ALL INJURY HAS ALL/ANY MODIFIED DATES
pbp$game_date <- as.Date(pbp$game_date)
injuries$date_only <- as.Date(injuries$date_only)

combined_data <- pbp %>%
  left_join(injuries, by = c("player_injured" = "abv_name", "week" = "week", "player_inj_team" = "team")) 


# adding column to designate an injury occured in game
# player_injured is based on the column created in original pbp that shows wheteher play was injured on the play
# for injury locations, will only consider in game injuries
combined_data <- combined_data %>%
  mutate(in_game_injury = ifelse(!is.na(player_injured), 1, 0),
         knee_injury_in_game = ifelse(in_game_injury == 1, ifelse(report_primary_injury == "Knee", 1, 0), 0), #creating binary variable for in game knee injury
         lower_body_injury_in_game = ifelse(report_primary_injury %in% c("Achilles", "Ankle", "Calf", "Feet", 
                                                                         "Fibula", "Foot", "Glute", "Groin", 
                                                                         "Hamstring", "Heel", "Hip", "Knee",
                                                                         "Knees", "left Foot", "Pelvis", "Quad",
                                                                         "Quadricep", "Right Ankle", "Right Knee",
                                                                         "Shin", "Tailbone", "Thigh", "Tibia",
                                                                         "Toe", "Toes"), 1, 0) #creating binary variable for in game lower body injury
         )

# selecting columns that we want to include in the model 
new_data <- combined_data %>% 
  select(game_id, home_team, away_team, season_type, week, game_date, quarter_seconds_remaining,
         half_seconds_remaining, game_seconds_remaining, drive, qtr, down, time, play_type, 
         yards_gained, pass_length, desc, pass_location, air_yards, yards_after_catch, run_location,
         total_home_score, total_away_score, wp, home_wp, away_wp, interception, solo_tackle, 
         penalty, fumble_lost, rusher, receiver, pass, rush, special, player_injured, player_inj_team,
         season.x, position, report_primary_injury, report_secondary_injury,  report_status, 
         date_modified, in_game_injury, knee_injury_in_game, lower_body_injury_in_game, stadium, weather,
         away_score, home_score, roof, surface, temp, wind, touchdown, sack, rush_attempt, pass_attempt, 
         qb_hit, fumble, complete_pass, passing_yards, rushing_yards, receiving_yards, 
         tackle_for_loss_1_player_name,tackle_for_loss_2_player_name, qb_hit_1_player_name, 
         qb_hit_2_player_name, forced_fumble_player_1_player_name, forced_fumble_player_2_player_name,
         solo_tackle_1_player_name, solo_tackle_2_player_name, assist_tackle_1_player_name, 
         assist_tackle_2_player_name, assist_tackle_3_player_name, assist_tackle_4_player_name, 
         tackle_with_assist_1_player_name, tackle_with_assist_2_player_name, pass_defense_1_player_name, 
         pass_defense_2_player_name, fumbled_1_player_name, fumble_recovery_1_player_name, sack_player_name,
         half_sack_1_player_name, half_sack_2_player_name
         )


########## Other things to add ##########

# 1) Correlation, initial feature importance, EDA of variables (plays w/ injuries vs plays w/o injuries)
# 2) Create model
# 3) Evaluate using accuracy, precision, recall, ...

