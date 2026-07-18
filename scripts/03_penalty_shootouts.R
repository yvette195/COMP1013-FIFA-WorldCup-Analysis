# Question 3: Penalty shootout participation
# Run scripts/01_data_preparation.R first so that the data frame df exists.

library(tidyverse)

# Keep only matches that involved a penalty shootout.
penalty_matches <- df %>%
  filter(PenaltyShootout == 1)

# Check the filtered data.
head(penalty_matches)
nrow(penalty_matches)
table(penalty_matches$PenaltyShootout)

# Count penalty-shootout matches for each home team.
home_penalty_count <- penalty_matches %>%
  group_by(HomeTeamName, HomeTeamCode) %>%
  summarise(NumberOfMatches = n())

# Sort from the highest number of matches to the lowest.
home_penalty_count <- home_penalty_count %>%
  arrange(desc(NumberOfMatches))

# Select the top five home teams.
top5_home <- head(home_penalty_count, 5)

# Rename the columns to match the assignment requirement.
names(top5_home) <- c("TeamName", "TeamCode", "NumberOfMatches")

# Display the top five home teams.
print(top5_home)

# Count penalty-shootout matches for each away team.
away_penalty_count <- penalty_matches %>%
  group_by(AwayTeamName, AwayTeamCode) %>%
  summarise(NumberOfMatches = n())

# Sort from the highest number of matches to the lowest.
away_penalty_count <- away_penalty_count %>%
  arrange(desc(NumberOfMatches))

# Select the top five away teams.
top5_away <- head(away_penalty_count, 5)

# Rename the columns to match the assignment requirement.
names(top5_away) <- c("TeamName", "TeamCode", "NumberOfMatches")

# Display the top five away teams.
print(top5_away)

# Code testing.
table(penalty_matches$PenaltyShootout)

nrow(top5_home)
nrow(top5_away)

names(top5_home)
names(top5_away)

sum(is.na(top5_home$TeamName))
sum(is.na(top5_home$TeamCode))

sum(is.na(top5_away$TeamName))
sum(is.na(top5_away$TeamCode))

top5_home$NumberOfMatches
top5_away$NumberOfMatches

# Compare the two top-five lists.
common_teams <- top5_home$TeamName[
  top5_home$TeamName %in% top5_away$TeamName
]

print(common_teams)
length(common_teams)

identical(
  top5_home$TeamName,
  top5_away$TeamName
)
