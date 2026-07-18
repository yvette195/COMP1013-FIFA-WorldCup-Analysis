# Question 4: Factors influencing match outcomes
# Run scripts/01_data_preparation.R first so that the data frame df exists.

library(tidyverse)

# Factor 1: Competition Stage
# Factor 2: ExtraTime

# Count all match results.
overall_result_table <- table(df$Result)
print(overall_result_table)

# Find the most common result.
most_common_result <- names(overall_result_table)[
  which.max(overall_result_table)
]

most_common_result_count <- max(overall_result_table)

print(most_common_result)
print(most_common_result_count)

# Create a count table of Stage and Result.
stage_result_count <- table(df$Stage, df$Result)
print(stage_result_count)

# Convert Stage counts to row percentages.
stage_result_percent <- prop.table(
  stage_result_count,
  margin = 1
) * 100

stage_result_percent <- round(stage_result_percent, 1)
print(stage_result_percent)

# Plot match results by Stage.
ggplot(df, aes(x = Stage, fill = Result)) +
  geom_bar(position = "fill") +
  labs(
    title = "Match Results by Competition Stage",
    x = "Competition stage",
    y = "Proportion of matches",
    fill = "Match result"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )

# Create a count table of ExtraTime and Result.
extra_time_result_count <- table(df$ExtraTime, df$Result)
print(extra_time_result_count)

# Convert ExtraTime counts to row percentages.
extra_time_result_percent <- prop.table(
  extra_time_result_count,
  margin = 1
) * 100

extra_time_result_percent <- round(extra_time_result_percent, 1)
print(extra_time_result_percent)

# Plot match results by ExtraTime.
ggplot(df, aes(x = ExtraTime, fill = Result)) +
  geom_bar(position = "fill") +
  labs(
    title = "Match Results by Extra-Time Category",
    x = "Extra-time category",
    y = "Proportion of matches",
    fill = "Match result"
  ) +
  theme_minimal()

# Code testing.
class(df$Result)
class(df$Stage)
class(df$ExtraTime)

levels(df$Result)
levels(df$Stage)
levels(df$ExtraTime)

sum(is.na(df$Result))
sum(is.na(df$Stage))
sum(is.na(df$ExtraTime))

sum(stage_result_count)
nrow(df)

sum(extra_time_result_count)
nrow(df)

rowSums(stage_result_percent)
rowSums(extra_time_result_percent)

# Find the Stage with the highest percentage of home-team wins.
home_result_column <- which(
  tolower(colnames(stage_result_percent)) ==
    "home team win"
)

highest_home_stage_row <- which.max(
  stage_result_percent[, home_result_column]
)

highest_home_stage <- rownames(
  stage_result_percent
)[highest_home_stage_row]

highest_home_stage_percent <- stage_result_percent[
  highest_home_stage_row,
  home_result_column
]

print(highest_home_stage)
print(highest_home_stage_percent)

# Find the ExtraTime category with the highest percentage of home-team wins.
highest_home_extra_row <- which.max(
  extra_time_result_percent[, home_result_column]
)

highest_home_extra <- rownames(
  extra_time_result_percent
)[highest_home_extra_row]

highest_home_extra_percent <- extra_time_result_percent[
  highest_home_extra_row,
  home_result_column
]

print(highest_home_extra)
print(highest_home_extra_percent)
