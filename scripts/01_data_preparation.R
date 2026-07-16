# Question 1: Data inspection, cleaning and penalty distribution

library(tidyverse)

# Read the four datasets.
matches <- read.csv("../data/Matches.csv")
stadiums <- read.csv("../data/Stadiums.csv")
teams <- read.csv("../data/Teams.csv")
tournaments <- read.csv("../data/Tournaments.csv")

# Change "?" values to NA.
matches[matches == "?"] <- NA
stadiums[stadiums == "?"] <- NA
teams[teams == "?"] <- NA
tournaments[tournaments == "?"] <- NA

# Join all datasets.
df <- matches %>%
  left_join(
    teams %>%
      rename(
        HomeTeamID = TeamID,
        HomeTeamName = TeamName,
        HomeTeamCode = TeamCode
      ),
    by = "HomeTeamID"
  ) %>%
  left_join(
    teams %>%
      rename(
        AwayTeamID = TeamID,
        AwayTeamName = TeamName,
        AwayTeamCode = TeamCode
      ),
    by = "AwayTeamID"
  ) %>%
  left_join(stadiums, by = "StadiumID") %>%
  left_join(tournaments, by = "TournamentID")

# Inspect the data.
head(df)
dim(df)
str(df)
summary(df)

# Convert the required variables.
df$Result <- as.factor(df$Result)
df$Stage <- as.factor(df$Stage)
df$Country <- as.factor(df$Country)
df$ExtraTime <- as.factor(df$ExtraTime)

df$HomePenalty <- as.numeric(df$HomePenalty)
df$AwayPenalty <- as.numeric(df$AwayPenalty)

df$HomePenalty[is.na(df$HomePenalty)] <- 0
df$AwayPenalty[is.na(df$AwayPenalty)] <- 0

# Test the cleaned variables.
nrow(matches)
nrow(df)

class(df$Result)
class(df$Stage)
class(df$Country)
class(df$ExtraTime)
class(df$HomePenalty)
class(df$AwayPenalty)

sum(is.na(df$HomePenalty))
sum(is.na(df$AwayPenalty))

# Analyse penalty shootout matches.
penalty_matches <- df %>%
  filter(PenaltyShootout == 1)

penalty_table <- table(penalty_matches$HomePenalty)
print(penalty_table)

ggplot(penalty_matches, aes(x = factor(HomePenalty))) +
  geom_bar() +
  labs(
    title = "Distribution of Home-Team Penalty Goals",
    x = "Home-team penalty goals",
    y = "Number of matches"
  ) +
  theme_minimal()
