# Question 1: Data inspection, cleaning and home penalty distribution

library(tidyverse)

# Allow the script to work from either the project root or the scripts folder.
project_root <- if (
  file.exists("COMP1013-FIFA-WorldCup-Analysis.Rproj")
) "." else ".."

# Import datasets and convert "?" into NA.
matches <- read.csv(
  file.path(project_root, "data", "Matches.csv"),
  na.strings = "?",
  stringsAsFactors = FALSE
)

stadiums <- read.csv(
  file.path(project_root, "data", "Stadiums.csv"),
  na.strings = "?",
  stringsAsFactors = FALSE
)

teams <- read.csv(
  file.path(project_root, "data", "Teams.csv"),
  na.strings = "?",
  stringsAsFactors = FALSE
)

tournaments <- read.csv(
  file.path(project_root, "data", "Tournaments.csv"),
  na.strings = "?",
  stringsAsFactors = FALSE
)

# Prepare home-team and away-team lookup tables.
home_teams <- teams %>%
  transmute(
    HomeTeamID = TeamID,
    HomeTeamName = TeamName,
    HomeTeamCode = TeamCode
  )

away_teams <- teams %>%
  transmute(
    AwayTeamID = TeamID,
    AwayTeamName = TeamName,
    AwayTeamCode = TeamCode
  )

# Join all datasets.
df <- matches %>%
  left_join(home_teams, by = "HomeTeamID") %>%
  left_join(away_teams, by = "AwayTeamID") %>%
  left_join(stadiums, by = "StadiumID") %>%
  left_join(tournaments, by = "TournamentID")

# Inspect the combined data.
dim(df)
names(df)
str(df)

# Convert the required variables.
df <- df %>%
  mutate(
    Result = as.factor(Result),
    Stage = as.factor(Stage),
    Country = as.factor(Country),
    ExtraTime = as.factor(ExtraTime),
    HomePenalty = replace_na(as.numeric(HomePenalty), 0),
    AwayPenalty = replace_na(as.numeric(AwayPenalty), 0)
  )

# Test the join and conversions.
stopifnot(
  nrow(df) == nrow(matches),
  is.factor(df$Result),
  is.factor(df$Stage),
  is.factor(df$Country),
  is.factor(df$ExtraTime),
  is.numeric(df$HomePenalty),
  is.numeric(df$AwayPenalty),
  !anyNA(df$HomePenalty),
  !anyNA(df$AwayPenalty)
)

# Filter penalty shootout matches and summarise HomePenalty.
penalty_matches <- df %>%
  filter(PenaltyShootout == 1)

stopifnot(nrow(penalty_matches) > 0)

penalty_distribution <- penalty_matches %>%
  count(HomePenalty, name = "NumberOfMatches") %>%
  arrange(HomePenalty)

print(penalty_distribution)

# Plot the distribution.
ggplot(
  penalty_matches,
  aes(x = factor(HomePenalty))
) +
  geom_bar() +
  labs(
    title = "Distribution of Home-Team Penalty Goals",
    subtitle = "FIFA World Cup matches involving a penalty shootout",
    x = "Home-team penalty goals",
    y = "Number of matches"
  ) +
  theme_minimal()
