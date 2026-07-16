# Question 2: Distribution of home-team scores

library(tidyverse)

# Allow the script to work from either the project root or the scripts folder.
project_root <- if (
  file.exists("COMP1013-FIFA-WorldCup-Analysis.Rproj")
) "." else ".."

# Run the Question 1 preparation script to create the cleaned data frame 'df'.
source(
  file.path(
    project_root,
    "scripts",
    "01_data_preparation.R"
  )
)

# Summarise HomeTeamScore by competition stage.
stage_score_summary <- df %>%
  group_by(Stage) %>%
  summarise(
    NumberOfMatches = n(),
    MeanHomeScore = mean(HomeTeamScore),
    MedianHomeScore = median(HomeTeamScore),
    SDHomeScore = sd(HomeTeamScore),
    MaximumHomeScore = max(HomeTeamScore),
    .groups = "drop"
  ) %>%
  arrange(desc(MeanHomeScore))

print(stage_score_summary)

# Plot HomeTeamScore by stage.
ggplot(df, aes(x = HomeTeamScore)) +
  geom_histogram(
    binwidth = 1,
    boundary = -0.5
  ) +
  facet_wrap(
    ~ Stage,
    ncol = 3,
    scales = "free_y"
  ) +
  scale_x_continuous(
    breaks = seq(
      0,
      max(df$HomeTeamScore),
      by = 1
    )
  ) +
  labs(
    title = "Distribution of Home-Team Scores by Competition Stage",
    x = "Home-team goals",
    y = "Number of matches"
  ) +
  theme_minimal()

# Summarise HomeTeamScore by tournament.
tournament_score_summary <- df %>%
  group_by(TournamentName) %>%
  summarise(
    NumberOfMatches = n(),
    MeanHomeScore = mean(HomeTeamScore),
    MedianHomeScore = median(HomeTeamScore),
    SDHomeScore = sd(HomeTeamScore),
    MaximumHomeScore = max(HomeTeamScore),
    .groups = "drop"
  ) %>%
  arrange(TournamentName)

print(tournament_score_summary)

# Plot HomeTeamScore by tournament.
ggplot(df, aes(x = HomeTeamScore)) +
  geom_histogram(
    binwidth = 1,
    boundary = -0.5
  ) +
  facet_wrap(
    ~ TournamentName,
    ncol = 4,
    scales = "free_y"
  ) +
  scale_x_continuous(
    breaks = seq(
      0,
      max(df$HomeTeamScore),
      by = 1
    )
  ) +
  labs(
    title = "Distribution of Home-Team Scores by Tournament",
    x = "Home-team goals",
    y = "Number of matches"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 7)
  )

# Create the required AwayTeamScore groups.
df <- df %>%
  mutate(
    AwayScoreGroup = case_when(
      AwayTeamScore <= 1 ~ "0-1 goals",
      AwayTeamScore <= 3 ~ "2-3 goals",
      AwayTeamScore >= 4 ~ "4 or more goals"
    ),
    AwayScoreGroup = factor(
      AwayScoreGroup,
      levels = c(
        "0-1 goals",
        "2-3 goals",
        "4 or more goals"
      )
    )
  )

# Summarise HomeTeamScore by away-team score group.
away_group_summary <- df %>%
  group_by(AwayScoreGroup) %>%
  summarise(
    NumberOfMatches = n(),
    MeanHomeScore = mean(HomeTeamScore),
    MedianHomeScore = median(HomeTeamScore),
    SDHomeScore = sd(HomeTeamScore),
    MaximumHomeScore = max(HomeTeamScore),
    .groups = "drop"
  )

print(away_group_summary)

# Plot HomeTeamScore by away-team score group.
ggplot(df, aes(x = HomeTeamScore)) +
  geom_histogram(
    binwidth = 1,
    boundary = -0.5
  ) +
  facet_wrap(
    ~ AwayScoreGroup,
    nrow = 1,
    scales = "free_y"
  ) +
  scale_x_continuous(
    breaks = seq(
      0,
      max(df$HomeTeamScore),
      by = 1
    )
  ) +
  labs(
    title = "Distribution of Home-Team Scores by Away-Team Score Group",
    x = "Home-team goals",
    y = "Number of matches"
  ) +
  theme_minimal()

# Test data types, score ranges and grouping completeness.
away_group_counts <- table(
  df$AwayScoreGroup,
  useNA = "ifany"
)

print(away_group_counts)

stopifnot(
  is.numeric(df$HomeTeamScore),
  is.numeric(df$AwayTeamScore),
  !anyNA(df$HomeTeamScore),
  !anyNA(df$AwayTeamScore),
  all(df$HomeTeamScore >= 0),
  all(df$AwayTeamScore >= 0),
  !anyNA(df$AwayScoreGroup),
  sum(away_group_counts) == nrow(df),
  sum(stage_score_summary$NumberOfMatches) == nrow(df),
  sum(tournament_score_summary$NumberOfMatches) == nrow(df),
  sum(away_group_summary$NumberOfMatches) == nrow(df)
)
