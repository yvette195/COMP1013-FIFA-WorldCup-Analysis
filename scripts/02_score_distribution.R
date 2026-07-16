# Question 2: Distribution of home-team scores
# Run this script after 01_data_preparation.R so that df exists.

library(tidyverse)

# Mean and median HomeTeamScore by Stage.
stage_mean <- aggregate(
  HomeTeamScore ~ Stage,
  mean,
  data = df
)

stage_median <- aggregate(
  HomeTeamScore ~ Stage,
  median,
  data = df
)

print(stage_mean)
print(stage_median)

# Histogram by Stage.
ggplot(df, aes(x = HomeTeamScore)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~ Stage) +
  labs(
    title = "Home-Team Scores by Competition Stage",
    x = "Home-team goals",
    y = "Number of matches"
  ) +
  theme_minimal()

# Mean and median HomeTeamScore by TournamentName.
tournament_mean <- aggregate(
  HomeTeamScore ~ TournamentName,
  mean,
  data = df
)

tournament_median <- aggregate(
  HomeTeamScore ~ TournamentName,
  median,
  data = df
)

print(tournament_mean)
print(tournament_median)

# Histogram by TournamentName.
ggplot(df, aes(x = HomeTeamScore)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~ TournamentName) +
  labs(
    title = "Home-Team Scores by Tournament",
    x = "Home-team goals",
    y = "Number of matches"
  ) +
  theme_minimal()

# Create the three required AwayTeamScore groups.
df$AwayScoreGroup <- NA

df$AwayScoreGroup[df$AwayTeamScore <= 1] <- "0-1 goals"

df$AwayScoreGroup[
  df$AwayTeamScore >= 2 &
    df$AwayTeamScore <= 3
] <- "2-3 goals"

df$AwayScoreGroup[df$AwayTeamScore >= 4] <- "4 or more goals"

df$AwayScoreGroup <- as.factor(df$AwayScoreGroup)

# Check group sizes.
print(table(df$AwayScoreGroup))

# Mean and median HomeTeamScore by AwayScoreGroup.
away_group_mean <- aggregate(
  HomeTeamScore ~ AwayScoreGroup,
  mean,
  data = df
)

away_group_median <- aggregate(
  HomeTeamScore ~ AwayScoreGroup,
  median,
  data = df
)

print(away_group_mean)
print(away_group_median)

# Histogram by AwayScoreGroup.
ggplot(df, aes(x = HomeTeamScore)) +
  geom_histogram(binwidth = 1) +
  facet_wrap(~ AwayScoreGroup) +
  labs(
    title = "Home-Team Scores by Away-Team Score Group",
    x = "Home-team goals",
    y = "Number of matches"
  ) +
  theme_minimal()

# Simple code testing.
class(df$HomeTeamScore)
class(df$AwayTeamScore)
sum(is.na(df$HomeTeamScore))
sum(is.na(df$AwayTeamScore))
class(df$AwayScoreGroup)
table(df$AwayScoreGroup)
sum(is.na(df$AwayScoreGroup))
sum(table(df$AwayScoreGroup))
nrow(df)
