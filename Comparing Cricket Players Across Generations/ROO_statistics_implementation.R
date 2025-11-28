data <- read.csv("project_1_ds3.csv")
## Calculating ROO
data <- data %>%
  mutate(
    # Ensure numeric conversion; handle cases where conversion might fail
    BattingAverage = as.numeric(as.character(BattingAverage)),
    others_avg = as.numeric(as.character(others_avg))
  ) %>%
  mutate(
    # Check for NA values that might have been introduced by failed conversions
    BattingAverage = ifelse(is.na(BattingAverage), 0, BattingAverage),
    others_avg = ifelse(is.na(others_avg), 0, others_avg),
    # Calculate ROO
    ROO = (BattingAverage - others_avg) * (Inns - Not_out)
  )

# View the updated data to check for errors
print(head(data))

# Optionally, you can rank players based on ROO
data <- data %>%
  arrange(desc(ROO)) %>%
  mutate(Rank = row_number())

# View the ranked data
print(select(data, Name, Country, BattingAverage, run_against, total_out, others_avg, ROO, Rank))

# Save the ranked data to a CSV file
write.csv(data, "cricketers_ranked_by_roo.csv", row.names = FALSE)
