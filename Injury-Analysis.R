library(nflreadr)
library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(logistf)
library(corrplot)
library(psych)

nflreadr::.clear_cache()
# Loading in 2024 play by play data
pbp_2024 <- load_pbp(2024)
# Loading in 2024 injury data
injuries_2024 <- load_injuries(2024)
pbp_2023 <- load_pbp(2023)
injuries_2023 <- load_injuries(2023)
nflreadr::.clear_cache()
pbp_2022 <- load_pbp(2022)
injuries_2022 <- load_injuries(2022)
pbp_2021 <- load_pbp(2021)
injuries_2021 <- load_injuries(2021)
nflreadr::.clear_cache()
pbp_2020 <- load_pbp(2020)
injuries_2020 <- load_injuries(2020)
pbp_2019 <- load_pbp(2019)
injuries_2019 <- load_injuries(2019)
nflreadr::.clear_cache()
pbp_2018 <- load_pbp(2018)
injuries_2018 <- load_injuries(2018)
pbp_2017 <- load_pbp(2017)
injuries_2017 <- load_injuries(2017)
nflreadr::.clear_cache()
pbp_2016 <- load_pbp(2016)
injuries_2016 <- load_injuries(2016)
pbp_2015 <- load_pbp(2015)
injuries_2015 <- load_injuries(2015)
nflreadr::.clear_cache()
pbp_2014 <- load_pbp(2014)
injuries_2014 <- load_injuries(2014)
pbp_2013 <- load_pbp(2013)
injuries_2013 <- load_injuries(2013)
nflreadr::.clear_cache()
pbp_2012 <- load_pbp(2012)
injuries_2012 <- load_injuries(2012)
pbp_2011 <- load_pbp(2011)
nflreadr::.clear_cache()
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


new_data <- new_data %>%
  filter(rush == 1| pass == 1|special == 1)

########## Exploratory Data Analysis ##########
count_data <- as.data.frame(table(new_data$in_game_injury))
colnames(count_data) <- c("Injuries", "count")

ggplot(count_data, aes(x = factor(Injuries), y = count)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = count), vjust = -0.5) +
  labs(title = "Count of NFL Plays 2010-2024 With and Without Injuries",
       x = "", 
       y = "Count") +
  scale_x_discrete(labels = c("No Injury", "Injury")) +
  theme_minimal()

####
injury_counts <- data.frame(
  variable = c("Knee", "Lower Body"),
  count = c(sum(new_data$knee_injury_in_game == 1, na.rm = TRUE), sum(new_data$lower_body_injury_in_game == 1))
)
ggplot(injury_counts, aes(x = variable, y = count)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5) + # Add count labels above bars
  labs(x = "", y = "Count",
       title = "Count of Plays with Lower Body and Knee Injuries") +
  scale_x_discrete(labels = c("Knee Injuries", "Lower Body Injuries")) +
  theme_minimal()

####
new_data %>%
  group_by(player_inj_team) %>%
  summarize(count = sum(in_game_injury == 1, na.rm = TRUE)) %>%
  filter(count > 0) %>%
  ggplot() + 
  geom_bar(stat = "identity", aes(x = reorder(player_inj_team, -count), y = count)) +
  labs(y = "In Game Injury Count", 
       x = "NFL Teams",
       title = "Number of In Game Injuries for Each NFL Team (2010-2024)") + 
  theme_minimal()

####

new_data$surface <- trimws(new_data$surface, which = "right")

new_data$surface <- ifelse(new_data$surface == "a_turf", "astroturf", new_data$surface)

surface_type_inj <- data.frame(
  variable = c("Knee", "Lower Body"),
  count = c(sum(new_data$knee_injury_in_game == 1, na.rm = TRUE), sum(new_data$lower_body_injury_in_game == 1))
  
)

####
summary_data <- new_data %>%
  filter(surface != "") %>%
  group_by(surface) %>%
  summarise(
    total_plays = n(),
    injury_count = sum(in_game_injury, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = c(total_plays, injury_count),
    names_to = "category",
    values_to = "count"
  )

# Create the bar chart
ggplot(summary_data, aes(x = surface, y = count, fill = category)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(
    values = c("total_plays" = "green", "injury_count" = "red"),
    labels = c("Injuries", "Total Plays")
  ) +
  labs(
    x = "Field Type",
    y = "Count",
    fill = "Category",
    title = "Total Plays vs Injuries by Field Type"
  ) +
  theme_minimal() +
  geom_text(aes(label = count), 
            position = position_dodge(width = 0.9),
            vjust = -0.5)
  
### CREATING MORE VARIABLES
new_data <- new_data %>%
  mutate(field_type = ifelse(surface == "grass", 0, 1))


data <- new_data %>%
  select(
    game_id,                   # To identify unique plays
    field_type,                   # Hypothesis 1 & 2: Turf vs. grass impact
    roof,
    in_game_injury,
    knee_injury_in_game,       # Hypothesis 1: Focus on knee injuries
    lower_body_injury_in_game, # Hypothesis 2: Lower body injuries
    temp,                      # Hypothesis 3: Weather conditions
    wind,                      # Hypothesis 3: Weather conditions
    position,                  # Hypothesis 4: Skill position players
    qtr,                       # Game dynamics (early vs. late injury patterns)
    rush,              # Offensive plays that could impact injuries
    pass,              # Offensive plays that could impact injuries
    special,
    quarter_seconds_remaining, # Time remaining in the current quarter
    weather
  )



newnew_data <- data %>%
  mutate(
    # Extract weather condition (e.g., "Light Rain")
    weather_condition = str_extract(weather, "^[^:]+"),
    
    # Extract temperature (numeric value in Fahrenheit)
    temperature = as.numeric(str_extract(weather, "(?<=Temp: )\\d+")),
    
    # Extract humidity (numeric value as a percentage)
    humidity = as.numeric(str_extract(weather, "(?<=Humidity: )\\d+")),
    
    # Extract wind (direction and speed)
    wind = str_extract(weather, "(?<=Wind: ).+")
  )


newnew_data <- newnew_data %>%
  mutate(
    rain = if_else(str_detect(weather_condition, regex("rain", ignore_case = TRUE)), 1, 0), # 1 if 'rain' is present
    snow = if_else(str_detect(weather_condition, regex("snow", ignore_case = TRUE)), 1, 0),  # 1 if 'snow' is present
    roof_binary = if_else(roof %in% c("outdoors", "open"), 0, 1)
  )


data <- newnew_data %>%
  select(
    game_id, 
    field_type, # Hypothesis 1 & 2: Turf vs. grass impact
    roof_binary,
    knee_injury_in_game,       # Hypothesis 1: Focus on knee injuries
    lower_body_injury_in_game, # Hypothesis 2: Lower body injuries
    in_game_injury,
    position,                  # Hypothesis 4: Skill position players
    qtr,                       # Game dynamics (early vs. late injury patterns)
    rush,              # Offensive plays that could impact injuries
    pass,              # Offensive plays that could impact injuries
    special,
    quarter_seconds_remaining, # Time remaining in the current quarter
    temperature, 
    wind, 
    humidity,
    rain, 
    snow
  )
data$position <- as.character(data$position)

data <- data %>% 
  filter(position %in% c("C", "CB", "DE", "DT", "FB", "G", "K", "LB", "LS", "P", "QB", "RB", "S", "T", "TE", "WR"))

data$position <- as.factor(data$position)


data <- data %>%
  mutate(new_wind = ifelse(
    grepl("[0-9]", wind), # Check if there are any numeric characters
    as.numeric(gsub("[^0-9.-]", "", wind)), # Extract numeric value
    NA # Assign NA for rows without numeric content
  ))

# Lower Body Injury Model

lb_model_reg <- lm(lower_body_injury_in_game ~ field_type + roof_binary + position + qtr + rush + pass + special +
            quarter_seconds_remaining + temperature + new_wind + humidity + rain + snow, data = data)

summary(lb_model_reg)



lb_model_log <- logistf(lower_body_injury_in_game ~ field_type + roof_binary + position + qtr + rush + pass + special +
                     quarter_seconds_remaining + temperature + new_wind + humidity + rain + snow, data = data)

summary(lb_model_log)


# Knee Injury model
knee_model_reg <- lm(knee_injury_in_game ~ field_type + roof_binary + position + qtr + rush + pass + special +
                     quarter_seconds_remaining + temperature + new_wind + humidity + rain + snow, data = data)

summary(knee_model_reg)



knee_model_log <- logistf(knee_injury_in_game ~ field_type + roof_binary + position + qtr + rush + pass + special +
                          quarter_seconds_remaining + temperature + new_wind + humidity + rain + snow, data = data)

summary(knee_model_log)


# All injury model
model_reg <- lm(in_game_injury ~ field_type + roof_binary + position + qtr + rush + pass + special +
                       quarter_seconds_remaining + temperature + new_wind + humidity + rain + snow, data = data)

summary(model_reg)



model_log <- logistf(in_game_injury ~ field_type + roof_binary + position + qtr + rush + pass + special +
                            quarter_seconds_remaining + temperature + new_wind + humidity + rain + snow, data = data)

summary(model_log)




########## Other things to add ##########

# 1) Correlation, initial feature importance, EDA of variables (plays w/ injuries vs plays w/o injuries)
# 2) Create model
# 3) Evaluate using accuracy, precision, recall, ...

# Correlation (get rid of anything below 0.2)
# Base model to get rid of any features that don't have any impact
 
 

# CREATE VARIABLE FOR WHETHER PLAY WAS INVOLVED IN THE PLAY (Touched ball or made tackle)
# MAKE variables 0, 1, 2 for some of injury variables
# Ex) Knee injury in game --> 0 = no injury, 1 = injury but not knee, 2 = knee injury on play 

