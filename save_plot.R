library(tidyverse)

# Read dataset for Digital Economy
df <- read_csv("data/UNCTAD_DE.csv", show_col_types = FALSE)

# Filter only imports/exports rows
df_trade <- df |>
  filter(
    INDICATOR %in%
      c("UNCTAD_DE_DIG_SERVTRADE_ANN_IMP", "UNCTAD_DE_DIG_SERVTRADE_ANN_EXP"),
    !is.na(OBS_VALUE), # Remove rows of empty export/import values
    !is.na(UNIT_MULT),
    REF_AREA != "WLD", # Include only countries
  ) |>
  mutate(
    amount_value = (OBS_VALUE * 10^UNIT_MULT / 1e12) # Convert to Trillions USD
  ) |>
  select(
    amount_value,
    time_period = TIME_PERIOD,
    indicator = INDICATOR
  ) |>
  mutate(
    indicator = case_when(
      indicator == "UNCTAD_DE_DIG_SERVTRADE_ANN_EXP" ~ "Exports",
      indicator == "UNCTAD_DE_DIG_SERVTRADE_ANN_IMP" ~ "Imports",
      TRUE ~ indicator
    ) # Rename default indicators to descriptive names
  ) |>
  distinct() |> # Remove duplicate rows
  group_by(time_period, indicator) |>
  summarise(
    amount_value = sum(amount_value, na.rm = TRUE),
    .groups = "drop"
  ) |> # Get total exports/imports value
  mutate(time_period = as.factor(time_period))

# Plot imports and export of digital services from 2010 to 2023.
plot <- df_trade |>
  ggplot(aes(x = time_period, y = amount_value)) +
  geom_smooth(
    aes(group = indicator, color = indicator),
    method = "gam",
    se = FALSE,
    linetype = "dashed",
    formula = y ~ s(x, bs = "cs"),
    linewidth = 1
  ) +
  scale_y_continuous(labels = scales::label_dollar(suffix = "T")) + # Show prices in USD
  labs(
    title = "Digitally-deliverable Services Trade (2010–2023)",
    x = "Year",
    y = "Value (Trillions USD)",
    color = NULL
  ) +
  theme_minimal(base_size = 14) + # Larger font size
  theme(axis.text.x = element_text(angle = 45))

saveRDS(plot, file = "data/main_plot.rds")