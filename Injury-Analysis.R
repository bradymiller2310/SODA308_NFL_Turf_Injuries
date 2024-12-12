### Loading in all necessary libraries ###
library(nflreadr)
library(dplyr)
library(stringr)
library(tidyverse)
library(ggplot2)
library(logistf)
library(corrplot)
library(psych)
library(caret)
library(reshape2)


########## Data collection ##########

# Reading in play by play & injury data from nflreader (2010-2024)
nflreadr::.clear_cache()
pbp_2024 <- load_pbp(2024)
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



### Combining play by play data ###
pbp <- rbind(pbp_2010, pbp_2011, pbp_2012, pbp_2013, pbp_2014, pbp_2015, 
             pbp_2016, pbp_2017, pbp_2018, pbp_2019, pbp_2020, pbp_2021,
             pbp_2022, pbp_2023, pbp_2024)

### Combining injury data ###
injuries <- rbind(injuries_2010, injuries_2011, injuries_2012, injuries_2013,
                  injuries_2014, injuries_2015, injuries_2016, injuries_2017,
                  injuries_2018, injuries_2019, injuries_2020, injuries_2021, 
                  injuries_2022, injuries_2023, injuries_2024)


### Removing all individual data sets to clear space ###
rm(pbp_2010, pbp_2011, pbp_2012, pbp_2013, pbp_2014, pbp_2015, pbp_2016, pbp_2017,
   pbp_2018, pbp_2019, pbp_2020, pbp_2021, pbp_2022, pbp_2023, pbp_2024, 
   injuries_2010, injuries_2011, injuries_2012, injuries_2013, injuries_2014,
   injuries_2015, injuries_2016, injuries_2017, injuries_2018, injuries_2019,
   injuries_2020, injuries_2021, injuries_2022, injuries_2023, injuries_2024)


########## Data Cleaning and Preprocessing ##########

### Function to extract player & team name of who was injured on the play ###
extract_injury_info <- function(desc) {
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

### Adding new columns to play by play of who was injured ###
pbp <- pbp %>%
  rowwise() %>%
  mutate(
    # Using function above to get and extract the information
    injury_info = list(extract_injury_info(desc)),
    player_injured = injury_info[[1]],
    player_inj_team = injury_info[[2]]
  ) %>%
  select(-injury_info) 


### Getting abbreviated name from injury data set for joining purposes ###
injuries <- injuries %>%
  mutate(
    abv_name = paste0(str_sub(full_name, 1, 1), ".", word(full_name, 2)),
    date_only = substr(date_modified, 1, 10)
  )



### Joining injury and play by play data ###

# Setting data type of data column for joining
pbp$game_date <- as.Date(pbp$game_date)
injuries$date_only <- as.Date(injuries$date_only)

# Joining on the name of player injured, their team, and the week it occured
combined_data <- pbp %>%
  left_join(injuries, by = c("player_injured" = "abv_name", "week" = "week", "player_inj_team" = "team")) 


# Adding 3 binary variables to designate injury on play and type
combined_data <- combined_data %>%
  mutate(
         # whether or not injury occured on play: use player injured (to check if anyone) & the primary injury as this will indicate if it was game (if not NA) or in practice (is NA)  
         # ensuring report says it is an injury (not personal matter or other)
         in_game_injury = ifelse(!is.na(player_injured) & !is.na(report_primary_injury) &
                                   report_primary_injury != "Not Injury Related" &
                                   report_primary_injury != "Not injury related - personal matter",
                                 1, 0),
         # in game knee injury: injury occured on play and the primary injury is reported as 'knee'
         knee_injury_in_game = ifelse(in_game_injury == 1, ifelse(report_primary_injury == "Knee", 1, 0), 0), 
         # lower body injury: must occur in game and injury must be designated as one some lower body location
         lower_body_injury_in_game = ifelse(in_game_injury == 1 & report_primary_injury %in% c("Achilles", "Ankle", "Calf", "Feet", 
                                                                         "Fibula", "Foot", "Glute", "Groin", 
                                                                         "Hamstring", "Heel", "Hip", "Knee",
                                                                         "Knees", "left Foot", "Pelvis", "Quad",
                                                                         "Quadricep", "Right Ankle", "Right Knee",
                                                                         "Shin", "Tailbone", "Thigh", "Tibia",
                                                                         "Toe", "Toes"), 1, 0)
         )

### extracting  weather data ###
combined_data <- combined_data %>%
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


### creating binary variables for whether there was snow or rain, and if stadium is open/closed based on roof, and wind value
combined_data <- combined_data %>%
  mutate(
    rain = if_else(str_detect(weather_condition, regex("rain", ignore_case = TRUE)), 1, 0), # 1 if 'rain' is present
    snow = if_else(str_detect(weather_condition, regex("snow", ignore_case = TRUE)), 1, 0),  # 1 if 'snow' is present
    roof_binary = if_else(roof %in% c("outdoors", "open"), 0, 1),
    new_wind = ifelse(grepl("[0-9]", wind),
                      as.numeric(gsub("[^0-9.-]", "", wind)), NA)
  )

### Creating binary variable for field type (turf or grass)
combined_data <- combined_data %>% 
  mutate(turf = ifelse(surface == "grass", 0, 1))


# removing plays considered "qb kneel" or "qb spike" as injuries would not happen on these plays
combined_data <- combined_data %>%
  filter(!(play_type %in% c("qb_kneel", "qb_spike")))


# filtering out plays that are listed as timeouts
# filtering out plays with pre-snap penalties so no play happened --> kept plays that have penalties later as someone could have gotten injured during the play
combined_data <- combined_data %>% 
  filter(
    !str_detect(desc, regex("(?i)timeout")) & # Remove rows containing "timeout" or "Timeout"
      !str_detect(desc, regex("^\\(\\d{1,2}:\\d{2}\\) PENALTY")) & # Remove rows starting with "(time) PENALTY"
      !str_detect(desc, regex("^\\(\\d{1,2}:\\d{2}\\) \\(.*\\) PENALTY")) & # Remove rows with "(time) (something) PENALTY"
      !str_detect(desc, regex("^\\(\\s*:\\d{2}\\) \\(.*\\) PENALTY")) & # Check for malformed time with something like (Shotgun) before PENALTY
      !str_detect(desc, regex("^\\(\\s*:\\d{2}\\) PENALTY")) & # Check for malformed time where PENALTY directly follows
      !str_detect(desc, regex("^\\(.*\\) PENALTY")) & 
      desc != "*** play under review ***"
  )

# creating categorical play type variable (0 = rush, 1 = pass, 2 = special teams)
combined_data <- combined_data %>%
  mutate(
    PlayType = case_when(
      rush == 1 ~ 0, # Rush play
      pass == 1 ~ 1, # Pass play
      rush == 0 & pass == 0 & special == 0 & str_detect(desc, regex("punts|kicks|field goal", ignore_case = TRUE)) ~ 2, # Special teams play
      TRUE ~ 3 # Other plays
    )
  )

test3 <- combined_data %>%
  filter(PlayType == 3)

# removing any plays not listed as special teams, run or pass (likely presnap penalty that was not able to be filtered out)
combined_data <- combined_data %>%
  filter(PlayType %in% c(0,1,2))

combined_data$position <- as.character(combined_data$position)
# removing any instances were the position is not a valid position, or not NA (has odd entry) - NA position would mean there was no one injured on play which is why those are fine to keep
combined_data <- combined_data %>%
  filter(position %in% c("C", "CB", "DE", "DT", "FB", "G", "K", "LB", "LS", "P", "QB", "RB", "S", "T", "TE", "WR") | is.na(position))



########## Exploratory Data Analysis ##########

##### 1) Play count of injury vs non-injury plays #####
# Getting count of in game injuries using in_game_injury variable
count_data <- as.data.frame(table(combined_data$in_game_injury))
colnames(count_data) <- c("Injuries", "count")

ggplot(count_data, aes(x = factor(Injuries), y = count)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_text(aes(label = count), vjust = -0.5) +
  labs(title = "Count of NFL Plays 2010-2024 With and Without Injuries",
       x = "", 
       y = "Count") +
  scale_x_discrete(labels = c("No Injury", "Injury")) +
  theme_minimal()

##### 2) Knee vs Lower Body Injuries of Plays with Injuries #####
# Getting count of knee injuries & lower body injuries
injury_counts <- data.frame(
  variable = c("Knee", "Lower Body"),
  count = c(sum(combined_data$knee_injury_in_game == 1, na.rm = TRUE), sum(combined_data$lower_body_injury_in_game == 1))
)
ggplot(injury_counts, aes(x = variable, y = count)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5) + # Add count labels above bars
  labs(x = "", y = "Count",
       title = "Count of Plays with Lower Body and Knee Injuries") +
  scale_x_discrete(labels = c("Knee Injuries", "Lower Body Injuries")) +
  theme_minimal()

##### 3) In game injuries for each team #####
combined_data %>%
  group_by(player_inj_team) %>%
  summarize(count = sum(in_game_injury == 1, na.rm = TRUE)) %>%
  filter(count > 0) %>%
  ggplot() + 
  geom_bar(stat = "identity", aes(x = reorder(player_inj_team, -count), y = count)) +
  labs(y = "In Game Injury Count", 
       x = "NFL Teams",
       title = "Number of In Game Injuries for Each NFL Team (2010-2024)") + 
  theme_minimal()

##### 4) Injuries by field type ####
# cleaning up field types
combined_data$surface <- trimws(combined_data$surface, which = "right")
combined_data$surface <- ifelse(combined_data$surface == "a_turf", "astroturf", combined_data$surface)

# creating summary data of the amount of plays and injuries on each surface
summary_data <- combined_data %>%
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

# Creating the bar chart
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


########## Variable Selection ##########
data <- combined_data %>%
  select(
    game_id, 
    turf, # Hypothesis 1 & 2: Turf vs. grass impact (turf = 1, grass = 0)
    roof_binary,
    knee_injury_in_game,       # Hypothesis 1: Focus on knee injuries
    lower_body_injury_in_game, # Hypothesis 2: Lower body injuries
    in_game_injury,
    position,                  # Hypothesis 4: Skill position players
    qtr,                       # Game dynamics (early vs. late injury patterns)
    PlayType,
    quarter_seconds_remaining, # Time remaining in the current quarter
    temperature, 
    new_wind, 
    humidity,
    rain, 
    snow,
    down,
    week
  )


########## CORRELATION ##########
numeric_columns <- data[, sapply(data, is.numeric)]

# Compute the correlation matrix
cor_matrix <- cor(numeric_columns, use = "pairwise.complete.obs")

# Melt the correlation matrix for ggplot
cor_data <- melt(cor_matrix)
color_scale <- colorRampPalette(c("blue", "white", "red"))

# Create the correlation plot
corrplot(cor_matrix, method = "color", type = "lower", 
         tl.col = "black", tl.srt = 45, 
         addCoef.col = "black", # Add correlation coefficients
         number.cex = 0.7,      # Adjust text size
         col = color_scale(200), # Apply flipped color scale
         cl.pos = "r",          # Position of the color legend
         cl.cex = 0.8) 

########## Preparing for modeling ##########
data$turf <- as.factor(data$turf)
data$knee_injury_in_game <- as.factor(data$knee_injury_in_game)
data$lower_body_injury_in_game <- as.factor(data$lower_body_injury_in_game)
data$in_game_injury <- as.factor(data$in_game_injury)
data$rain <- as.factor(data$rain)
data$snow <- as.factor(data$snow)
data$PlayType <- as.factor(data$PlayType)
data$qtr <- as.factor(data$qtr)
data$roof_binary <- as.factor(data$roof_binary)
data$down <- as.factor(data$down)
data$week <- as.factor(data$week)
data$position <- as.factor(data$position)



########## TRAIN AND TEST SETS ##########
set.seed(123)

### LOWER BODY INJURIES ###
# Perform stratified sampling
lb_train_index <- createDataPartition(data$lower_body_injury_in_game, p = 0.75, list = FALSE)  # 70% training set

# Split the data
lb_train_data <- data[lb_train_index, ]
lb_test_data <- data[-lb_train_index, ]

# Check proportions
table(data$lower_body_injury_in_game)   
table(lb_train_data$lower_body_injury_in_game) 
table(lb_test_data$lower_body_injury_in_game)


### KNEE INJURIES ###
knee_train_index <- createDataPartition(data$knee_injury_in_game, p = 0.75, list = FALSE)  # 70% training set

# Split the data
knee_train_data <- data[knee_train_index, ]
knee_test_data <- data[-knee_train_index, ]

# Check proportions
table(data$knee_injury_in_game)       
table(knee_train_data$knee_injury_in_game) 
table(knee_test_data$knee_injury_in_game)


### ALL INJURIES ###
data$in_game_injury <- as.numeric(data$in_game_injury)
train_index <- createDataPartition(data$in_game_injury, p = 0.75, list = FALSE)  # 70% training set

# Split the data
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Check proportions
table(data$in_game_injury)       # Full dataset
table(train_data$in_game_injury) # Training set
table(test_data$in_game_injury)


########## Model Creation ##########

### Lower Body Injury Model ###
lb_model_reg <- glm(lower_body_injury_in_game ~ turf + roof_binary + position + 
                    qtr + PlayType + quarter_seconds_remaining + temperature +
                    new_wind + humidity + rain + snow + down + week, 
                    data = lb_train_data,
                    family = binomial)

summary(lb_model_reg)

probabilities <- predict(lb_model_reg, newdata = lb_test_data, type = "response")
predictions <- ifelse(probabilities > 0.5, 1, 0)
confusion_matrix <- table(Predicted = predictions, Actual = lb_test_data$lower_body_injury_in_game)
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))




lb_model_log <- logistf(lower_body_injury_in_game ~ turf + roof_binary + position + 
                          qtr + PlayType + quarter_seconds_remaining + temperature +
                          new_wind + humidity + rain + snow + down + week, 
                        data = lb_train_data)

summary(lb_model_log)
probabilities <- predict(lb_model_log, newdata = lb_test_data, type = "response")
predictions <- ifelse(probabilities > 0.5, 1, 0)
confusion_matrix <- table(Predicted = predictions, Actual = lb_test_data$lower_body_injury_in_game)
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))


### Knee Injury Model ###
knee_model_reg <- glm(knee_injury_in_game ~ turf + roof_binary + position + qtr +
                      PlayType + quarter_seconds_remaining + temperature +
                      new_wind + humidity + rain + snow + down + week,
                      data = knee_train_data, 
                      family = binomial)

summary(knee_model_reg)

probabilities <- predict(knee_model_reg, newdata = knee_test_data)
predictions <- ifelse(probabilities > 0.5, 1, 0)
confusion_matrix <- table(Predicted = predictions, Actual = knee_test_data$knee_injury_in_game)
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))




knee_model_log <- logistf(knee_injury_in_game ~ turf + roof_binary + position + qtr +
                            PlayType + quarter_seconds_remaining + temperature +
                            new_wind + humidity + rain + snow + down + week,
                          data = knee_train_data)

summary(knee_model_log)
probabilities <- predict(knee_model_log, newdata = knee_test_data)
predictions <- ifelse(probabilities > 0.5, 1, 0)
confusion_matrix <- table(Predicted = predictions, Actual = knee_test_data$knee_injury_in_game)
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))


### All injury model ###
#data$in_game_injury <- as.numeric(data$in_game_injury)
data$in_game_injury <- ifelse(data$in_game_injury == 1, 0, 1)
train_index <- createDataPartition(data$in_game_injury, p = 0.75, list = FALSE)  # 70% training set

# Split the data
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Check proportions
table(data$in_game_injury)       # Full dataset
table(train_data$in_game_injury) # Training set
table(test_data$in_game_injury)

#data$in_game_injury <- as.numeric(data$in_game_injury)
model_reg <- glm(in_game_injury ~ turf + roof_binary + position + qtr + PlayType +
                       quarter_seconds_remaining + temperature + new_wind 
                 + humidity + rain + snow + down + week, data = train_data,
                 family = binomial)

summary(model_reg)
probabilities <- predict(model_reg, newdata = test_data)
predictions <- ifelse(probabilities > 0.5, 1, 0)
confusion_matrix <- table(Predicted = predictions, Actual = test_data$in_game_injury)
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))



model_log <- logistf(in_game_injury ~ turf + roof_binary + position + qtr + PlayType +
                       quarter_seconds_remaining + temperature + new_wind +
                       humidity + rain + snow + down + week, data = train_data)

#####
#library(car)
#vif_values <- vif(lm(in_game_injury ~ field_type + roof_binary + position + qtr + #rush + pass +
#                       quarter_seconds_remaining + temperature + new_wind + humidity + rain + snow + down + week, data = train_data))
#print(vif_values)
#####
#####
summary(model_log)
probabilities <- predict(model_log, newdata = test_data)
predictions <- ifelse(probabilities > 0.5, 1, 0)
confusion_matrix <- table(Predicted = predictions, Actual = test_data$injury_in_game)
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))




