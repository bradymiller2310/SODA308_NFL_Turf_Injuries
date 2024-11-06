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
pbp_2009 <- load_pbp(2009)
injuries_2009 <- load_injuries(2009)



pbp <- rbind(
             pbp_2009, pbp_2010, pbp_2011,
             pbp_2012, pbp_2013, pbp_2014, pbp_2015, pbp_2016, pbp_2017,
             pbp_2018, pbp_2019, pbp_2020, pbp_2021, pbp_2022, pbp_2023, 
             pbp_2024)

injuries <- rbind(
                  injuries_2009, injuries_2010, injuries_2011,
                  injuries_2012, injuries_2013, injuries_2014, injuries_2015, 
                  injuries_2016, injuries_2017, injuries_2018, injuries_2019,
                  injuries_2020, injuries_2021, injuries_2022, injuries_2023, 
                  injuries_2024)


rm(pbp_2009, pbp_2010, pbp_2011,
   pbp_2012, pbp_2013, pbp_2014, pbp_2015, pbp_2016, pbp_2017,
   pbp_2018, pbp_2019, pbp_2020, pbp_2021, pbp_2022, pbp_2023, 
   #pbp_2024, 
   injuries_2009, injuries_2010, injuries_2011,
   injuries_2012, injuries_2013, injuries_2014, injuries_2015, 
   injuries_2016, injuries_2017, injuries_2018, injuries_2019,
   injuries_2020, injuries_2021, injuries_2022, injuries_2023) 
   #injuries_2024)

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
# ONLY DOING 2024 FOR NOW - NOT ALL INJURY HAS ALL/ANY MODIFIED DATES
combined_data <- pbp_2024 %>%
  left_join(injuries_2024, by = c("player_injured" = "abv_name", "week" = "week", "player_inj_team" = "team")) #, "game_date" = "date_only"))



