# Load necessary packages
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)


# Read and process DM01 data
DM01_data <- list.files(path = 'raw_data/', pattern = '^(?!.*CDC).*202[0-4].*\\.csv$', full.names = TRUE) %>%
  lapply(read_csv) %>%
  bind_rows() %>%
  mutate(Period = gsub('DM01-|-', ' ', Period) %>% my())

# Aggregate Total Activity for DM01 data
DM01_data_Period_TA_agg <- DM01_data %>%
  select(Period, `Total Activity`) %>%
  group_by(Period) %>%
  summarise(`Total Activity` = sum(`Total Activity`), .groups = 'drop')

# Process CDC data
CDC_dataFinalQ <- CDC_data %>%
  select(Period, `Total Activity`) %>%
  group_by(Period) %>%
  summarise(`Total Activity` = sum(`Total Activity`), .groups = 'drop')

# Merge data and reshape
merged_long2 <- full_join(DM01_data_Period_TA_agg, CDC_dataFinalQ, by = "Period") %>%
  pivot_longer(cols = starts_with("Total Activity"), names_to = "Source", values_to = "Total.Activity") %>%
  mutate(Source = recode(Source, `Total Activity.x` = "Total Diagnostics", `Total Activity.y` = "CDC Diagnostics")) %>%
  mutate(Total.Activity = replace_na(Total.Activity, 0)) %>%
  pivot_wider(names_from = Source, values_from = Total.Activity) %>%
  mutate(`Total Diagnostics - CDC Diagnostics` = `Total Diagnostics` - `CDC Diagnostics`) %>%
  pivot_longer(cols = c(`Total Diagnostics`, `Total Diagnostics - CDC Diagnostics`), 
               names_to = "Source", 
               values_to = "Total.Activity") %>%
  mutate(Source = factor(Source, levels = c("Total Diagnostics", "Total Diagnostics - CDC Diagnostics")))

# Plot
ggplot(merged_long2, aes(x = Period, y = Total.Activity, fill = Source)) +
  geom_area(position = "identity", alpha = 1) +
  labs(title = "Graph of Changes in Diagnostic Activity, Total and CDC (2019-2024)",
       x = "Period", y = "Total Activity") +
  scale_fill_manual(values = c("Total Diagnostics" = "#1f77b4", 
                               "Total Diagnostics - CDC Diagnostics" = "lightblue"), 
                    labels = c("CDC Diagnostics", "Total Diagnostics")) +
  theme_minimal()

        