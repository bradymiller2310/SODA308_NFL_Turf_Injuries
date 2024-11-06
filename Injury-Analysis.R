library(nflreadr)
library(dplyr)
library(stringr)
library(tidyverse)

# Loading in 2024 play by play data
pbp_2024 <- load_pbp(2024)

# Loading in 2024 injury data
injuries_2024 <- load_injuries(2024)


# function for extracting the name & team of player who was injured on the play 
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


pbp_2024 <- pbp_2024 %>%
  rowwise() %>%
  mutate(
    injury_info = list(extract_injury_info(desc)),
    player_injured = injury_info[[1]],
    player_inj_team = injury_info[[2]]
  ) %>%
  select(-injury_info) 


# Getting abbreviated name from injury data set
injuries_2024 <- injuries_2024 %>%
  mutate(
    abv_name = paste0(str_sub(full_name, 1, 1), ".", word(full_name, 2)),
    date_only = substr(date_modified, 1, 10)
  )


# Joining injury and play by play data
combined_data <- pbp_2024 %>%
  left_join(injuries_2024, by = c("player_injured" = "abv_name", "week" = "week", "player_inj_team" = "team", "game_date" = "date_only"))



