save.image(file = "my_environment.RData")
setwd("C:/Users/brady/Downloads")
load("my_environment.RData")

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

########## More Data Cleaning ##########
# removing rows with snow, rain or temp NA values because tougher to impute those
new_data <- data %>% 
  select(-new_wind) %>% 
  filter(!is.na(rain) & !is.na(snow) & !is.na(temperature))

# creating humidity predictions (using temp and whether there was rain/snow)
# want to keep this variable as higher humidity could lead to less field traction --> more potential injuries
humidity_model <- lm(humidity ~ temperature + rain + snow, data = new_data, na.action = na.exclude)
new_data$humidity[is.na(new_data$humidity)] <- predict(humidity_model, newdata = new_data[is.na(new_data$humidity), ])


# checking NA count of all columns
colSums(is.na(new_data))

# adding new level to positions so it allows for predictions in modeling (if position is NA it won't be able to predict)
new_data$position <- ifelse(is.na(new_data$position), "No Injury", new_data$position)

# removing all other NA values in the dataset
new_data <- new_data %>% na.omit()

########## Preparing for modeling ##########
new_data$turf <- as.factor(new_data$turf)
new_data$knee_injury_in_game <- as.factor(new_data$knee_injury_in_game)
new_data$lower_body_injury_in_game <- as.factor(new_data$lower_body_injury_in_game)
new_data$in_game_injury <- as.factor(new_data$in_game_injury)
new_data$rain <- as.factor(new_data$rain)
new_data$snow <- as.factor(new_data$snow)
new_data$PlayType <- as.factor(new_data$PlayType)
new_data$qtr <- as.factor(new_data$qtr)
new_data$roof_binary <- as.factor(new_data$roof_binary)
new_data$down <- as.factor(new_data$down)
new_data$week <- as.factor(new_data$week)
new_data$position <- as.factor(new_data$position)






########## TRAIN AND TEST SETS ##########
########## 
########## 
########## 
########## DONT TOUCH/RESET TEST-TRAIN SETS ##########
########## 
########## 
########## 
set.seed(123)

### LOWER BODY INJURIES ###
# Perform stratified sampling
lb_train_index <- createDataPartition(new_data$lower_body_injury_in_game, p = 0.75, list = FALSE)  # 70% training set

# Split the data
lb_train_data <- new_data[lb_train_index, ]
lb_test_data <- new_data[-lb_train_index, ]

# Check proportions
table(new_data$lower_body_injury_in_game)   
table(lb_train_data$lower_body_injury_in_game) 
table(lb_test_data$lower_body_injury_in_game)


### KNEE INJURIES ###
knee_train_index <- createDataPartition(new_data$knee_injury_in_game, p = 0.75, list = FALSE)  # 70% training set

# Split the data
knee_train_data <- new_data[knee_train_index, ]
knee_test_data <- new_data[-knee_train_index, ]

# Check proportions
table(new_data$knee_injury_in_game)       
table(knee_train_data$knee_injury_in_game) 
table(knee_test_data$knee_injury_in_game)


### ALL INJURIES ###
#data$in_game_injury <- as.numeric(data$in_game_injury)
train_index <- createDataPartition(new_data$in_game_injury, p = 0.75, list = FALSE)  # 70% training set

# Split the data
train_data <- new_data[train_index, ]
test_data <- new_data[-train_index, ]

train_data$in_game_injury <- as.factor(train_data$in_game_injury)
test_data$in_game_injury <- as.factor(test_data$in_game_injury)

# Check proportions
table(new_data$in_game_injury)       # Full dataset
table(train_data$in_game_injury) # Training set
table(test_data$in_game_injury)


### Function for calculating model evaluation metrics
compute_metrics <- function(conf_matrix) {
  # Extract dimensions of the confusion matrix
  classes <- rownames(conf_matrix)
  
  # Initialize variables for confusion matrix values
  TP <- 0  # True Positives
  FP <- 0  # False Positives
  FN <- 0  # False Negatives
  TN <- 0  # True Negatives
  
  # Check for the presence of both predicted classes
  if ("1" %in% classes) {
    TP <- conf_matrix["1", "1"]
    FP <- conf_matrix["1", "0"]
  }
  if ("0" %in% classes) {
    TN <- conf_matrix["0", "0"]
    FN <- conf_matrix["0", "1"]
  }
  
  # Calculate metrics for Class 1
  precision_1 <- ifelse((TP + FP) > 0, TP / (TP + FP), NA)
  recall_1 <- ifelse((TP + FN) > 0, TP / (TP + FN), NA)
  f1_score_1 <- ifelse(!is.na(precision_1) & !is.na(recall_1) & (precision_1 + recall_1) > 0,
                       2 * (precision_1 * recall_1) / (precision_1 + recall_1), NA)
  
  # Calculate metrics for Class 0
  precision_0 <- ifelse((TN + FN) > 0, TN / (TN + FN), NA)
  recall_0 <- ifelse((TN + FP) > 0, TN / (TN + FP), NA)
  f1_score_0 <- ifelse(!is.na(precision_0) & !is.na(recall_0) & (precision_0 + recall_0) > 0,
                       2 * (precision_0 * recall_0) / (precision_0 + recall_0), NA)
  
  # Overall metrics
  accuracy <- (TP + TN) / sum(conf_matrix)
  specificity <- ifelse((TN + FP) > 0, TN / (TN + FP), NA)
  
  # Print metrics in a clean format
  cat("\n### Metrics for Each Class ###\n")
  cat("Class 1 (Injury Class):\n")
  cat("  Precision:  ", ifelse(!is.na(precision_1), round(precision_1, 4), "NA"), "\n")
  cat("  Recall:     ", ifelse(!is.na(recall_1), round(recall_1, 4), "NA"), "\n")
  cat("  F1 Score:   ", ifelse(!is.na(f1_score_1), round(f1_score_1, 4), "NA"), "\n")
  
  cat("\nClass 0 (No Injury Class):\n")
  cat("  Precision:  ", ifelse(!is.na(precision_0), round(precision_0, 4), "NA"), "\n")
  cat("  Recall:     ", ifelse(!is.na(recall_0), round(recall_0, 4), "NA"), "\n")
  cat("  F1 Score:   ", ifelse(!is.na(f1_score_0), round(f1_score_0, 4), "NA"), "\n")
  
  cat("\n### Overall Metrics ###\n")
  cat("  Accuracy:    ", round(accuracy, 4), "\n")
  cat("  Specificity: ", ifelse(!is.na(specificity), round(specificity, 4), "NA"), "\n")

}

######################### Model Creation #########################

############### Lower Body Injury Model ###############

####### Logistic Regression #######
lb_train_data$position <- as.factor(trimws(lb_train_data$position))
lb_test_data$position <- as.factor(trimws(lb_test_data$position))

# dropping "P" and "K" position level - model struggled to learn from it
lb_train_data <- subset(lb_train_data, (position %in% c("C", "CB", "DE", "DT", "FB", "G", "LB", "LS", "QB", "RB", "S", "T", "TE", "WR", "No Injury")))
lb_train_data <- droplevels(lb_train_data)
lb_test_data <- subset(lb_test_data, (position %in% c("C", "CB", "DE", "DT", "FB", "G", "LB", "LS", "QB", "RB", "S", "T", "TE", "WR", "No Injury")))
lb_test_data <- droplevels(lb_test_data)

# removing week 22 since there is none in that level in train & only 1 in test
lb_train_data <- subset(lb_train_data, week != 22)
lb_train_data <- droplevels(lb_train_data)
lb_test_data <- subset(lb_test_data, week != 22)
lb_test_data <- subset(lb_test_data)

lb_model_reg <- glm(lower_body_injury_in_game ~ turf + roof_binary + position + 
                    qtr + PlayType + quarter_seconds_remaining + temperature +
                    humidity + rain + snow + down + week, 
                    data = lb_train_data,
                    family = binomial)

summary(lb_model_reg)

probabilities <- predict(lb_model_reg, newdata = lb_test_data, type = "response")
predictions <- ifelse(probabilities > 0.5, 1, 0)
confusion_matrix <- table(Predicted = predictions, Actual = lb_test_data$lower_body_injury_in_game)
print(confusion_matrix)
compute_metrics(confusion_matrix)



####### Firth's Logistic Regression #######
lb_train_new <- lb_train_data %>%
  select(turf, roof_binary, lower_body_injury_in_game, position, qtr, PlayType, 
         quarter_seconds_remaining, temperature, humidity, rain, snow,
         down, week)

lb_test_new <- lb_test_data %>%
  select(turf, roof_binary, lower_body_injury_in_game, position, qtr, PlayType, 
         quarter_seconds_remaining, temperature, humidity, rain, snow,
         down, week)


lb_model_log <- logistf(lower_body_injury_in_game ~ ., data = lb_train_new)

summary(lb_model_log)

design_matrix_train <- model.matrix(lb_model_log$formula, data = lb_train_new)
design_matrix_test <- model.matrix(lb_model_log$formula, data = lb_test_new)

cat("Training matrix dimensions:", dim(design_matrix_train), "\n")
cat("Training matrix dimensions:", dim(design_matrix_test), "\n")

# Need to remove the week 22 variable from the test set
lb_test_new <- subset(lb_test_new, week != 22)
lb_test_new <- droplevels(lb_test_new)


probabilities <- predict(lb_model_log, newdata = lb_test_new)
predictions <- ifelse(probabilities > 0.5, 1, 0)
confusion_matrix <- table(Predicted = predictions, Actual = lb_test_new$lower_body_injury_in_game)
print(confusion_matrix)
compute_metrics(confusion_matrix)
#accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
#print(paste("Accuracy:", accuracy))


############## Knee Injury Model ###############

####### Logistic Regression #######

# dropping the 6th qtr because of rare event issues --> model isn't learning from it
knee_train_data <- subset(knee_train_data, qtr != 6)
knee_test_data <- subset(knee_test_data, qtr != 6)
knee_train_data <- droplevels(knee_train_data)
knee_test_data <- droplevels(knee_test_data)

# need to drop 'K' from position in training data
knee_train_data <- subset(knee_train_data, (position %in% c("C", "CB", "DE", "DT", "FB", "G", "LB", "LS", "QB", "RB", "P", "S", "T", "TE", "WR", "No Injury")))
knee_train_data <- droplevels(knee_train_data)


knee_model_reg <- glm(knee_injury_in_game ~ turf + roof_binary + position + qtr +
                      PlayType + quarter_seconds_remaining + temperature +
                      humidity + rain + snow + down + week,
                      data = knee_train_data, 
                      family = binomial)

summary(knee_model_reg)

probabilities <- predict(knee_model_reg, newdata = knee_test_data)
predictions <- ifelse(probabilities > 0.5, 1, 0)
confusion_matrix <- table(Predicted = predictions, Actual = knee_test_data$knee_injury_in_game)
print(confusion_matrix)
compute_metrics(confusion_matrix)

####### Firth's Logistic Regression #######
knee_train_new <- knee_train_data %>%
  select(turf, roof_binary, knee_injury_in_game, position, qtr, PlayType, 
         quarter_seconds_remaining, temperature, humidity, rain, snow,
         down, week)

knee_test_new <- knee_test_data %>%
  select(turf, roof_binary, knee_injury_in_game, position, qtr, PlayType, 
         quarter_seconds_remaining, temperature, humidity, rain, snow,
         down, week)

knee_train_new <- subset(knee_train_new, 
                         (position %in% c("C", "CB", "DE", "DT", "FB", "G", "LB", "LS", "QB", "RB", "S", "T", "TE", "WR", "No Injury")))
knee_train_new <- droplevels(knee_train_new)


knee_model_log <- logistf(knee_injury_in_game ~ ., data = knee_train_new)

summary(knee_model_log)

# need to fix data because of errors with making predictions
# creating design matricies to debug the issue
#design_matrix_train <- model.matrix(knee_model_log$formula, data = knee_train_new)
#design_matrix_test <- model.matrix(knee_model_log$formula, data = knee_test_new)

#cat("Training matrix dimensions:", dim(design_matrix_train), "\n")
#cat("Training matrix dimensions:", dim(design_matrix_test), "\n")

# need to drop 'K' from position in training data
#knee_train_new <- subset(knee_train_new, (position %in% c("C", "CB", "DE", "DT", "FB", "G", "LB", "LS", "QB", "RB", "P", "S", "T", "TE", "WR", "No Injury")))
#knee_train_new <- droplevels(knee_train_new)

#knee_model_log <- logistf(knee_injury_in_game ~ ., data = knee_train_new)
#summary(knee_model_log)

probabilities <- predict(knee_model_log, newdata = knee_test_new)
predictions <- ifelse(probabilities > 0.5, 1, 0)
confusion_matrix <- table(Predicted = predictions, Actual = knee_test_new$knee_injury_in_game)
print(confusion_matrix)
compute_metrics(confusion_matrix)


############### All injury model ###############

####### Logistic Regression #######

# dropping the 6th qtr because of rare event issues --> model isn't learning from it
# dropping the unused levels
train_data <- subset(train_data, qtr != 6)
train_data <- droplevels(train_data)
test_data <- subset(test_data, qtr != 6)
test_data <- droplevels(test_data)

# dropping the week 22 rows (model was struggling to learn with it)
train_data <- subset(train_data, week != 22)
train_data <- droplevels(train_data)
test_data <- subset(test_data, week != 22)
test_data <- droplevels(test_data)

# Dropping kicker & punter position because train set doesn't have it while test set does
# can't learn off levels it didn't intially know
train_data <- subset(train_data, (position %in% c("C", "CB", "DE", "DT", "FB", "G", "LB", "LS", "QB", "RB", "S", "T", "TE", "WR", "No Injury")))
test_data <- subset(test_data, (position %in% c("C", "CB", "DE", "DT", "FB", "G", "LB", "LS", "QB", "RB", "S", "T", "TE", "WR", "No Injury")))

# Setting week factor levels to match
test_data$week <- factor(test_data$week, levels = levels(train_data$week))
test_data$position <- factor(test_data$position, levels = levels(train_data$position))

model_reg <- glm(in_game_injury ~ turf + roof_binary + position + qtr + PlayType +
                       quarter_seconds_remaining + temperature  
                 + humidity + rain + snow + down + week, data = train_data,
                 family = binomial)

summary(model_reg)
probabilities <- predict(model_reg, newdata = test_data)
predictions <- ifelse(probabilities > 0.5, 1, 0)
confusion_matrix <- table(Predicted = predictions, Actual = test_data$in_game_injury)
print(confusion_matrix)
compute_metrics(confusion_matrix)


####### Firth's Logistic Regression #######

# getting rid of unneeded columns in training & testing data
train_data_new <- train_data %>%
  select(turf, roof_binary, in_game_injury, position, qtr, PlayType, 
         quarter_seconds_remaining, temperature, humidity, rain, snow,
         down, week)

test_data_new <- test_data %>%
  select(turf, roof_binary, in_game_injury, position, qtr, PlayType, 
         quarter_seconds_remaining, temperature, humidity, rain, snow,
         down, week)

# check that factors align
test_data_new$turf <- factor(test_data_new$turf, levels = levels(train_data_new$turf))
test_data_new$roof_binary <- factor(test_data_new$roof_binary, levels = levels(train_data_new$roof_binary))
test_data_new$position <- factor(test_data_new$position, levels = levels(train_data_new$position))
test_data_new$qtr <- factor(test_data_new$qtr, levels = levels(train_data_new$qtr))
test_data_new$PlayType <- factor(test_data_new$PlayType, levels = levels(train_data_new$PlayType))
test_data_new$rain <- factor(test_data_new$rain, levels = levels(train_data_new$rain))
test_data_new$snow <- factor(test_data_new$snow, levels = levels(train_data_new$snow))
test_data_new$down <- factor(test_data_new$down, levels = levels(train_data_new$down))
test_data_new$week <- factor(test_data_new$week, levels = levels(train_data_new$week))

# dropping 'P' position level - already got rid of it but the level is still showing up
test_data_new <- droplevels(test_data_new)
train_data_new <- droplevels(train_data_new)

# creating model
model_log <- logistf(in_game_injury ~ turf + roof_binary + position + qtr + PlayType +
                       quarter_seconds_remaining + temperature + humidity +
                       rain + snow + down + week, data = train_data_new)

summary(model_log)

probabilities <- predict(model_log, newdata = test_data_new)
predictions <- ifelse(probabilities > 0.5, 1, 0)
confusion_matrix <- table(Predicted = predictions, Actual = test_data_new$in_game_injury)
print(confusion_matrix)
compute_metrics(confusion_matrix)




