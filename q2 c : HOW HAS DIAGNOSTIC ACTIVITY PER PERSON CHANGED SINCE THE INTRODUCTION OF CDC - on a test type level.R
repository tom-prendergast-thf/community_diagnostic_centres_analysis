### QUESTION 2 -  2c: HOW HAS DIAGNOSTIC ACTIVITY PER PERSON CHANGED SINCE THE INTRODUCTION OF CDC - on a test type leve

## grouping by Diagnostic Test ###
# on most basis level - regional (via ICB) DA pp over time
DM01_ICBPopulationMerge1c <- subset(DM01_ICBPopulationMerge1, select = -c(`NSHER.2023.Name`, `ICB.2023.Name`, `Commissioner Org Code`))
DM01_ICBPopulationMerge1c <- DM01_ICBPopulationMerge1b %>%
  group_by(Period, `Diagnostic Tests`) %>%
  summarize(
    Total_Activity = sum(`Total_Activity`, na.rm = TRUE),
    Population_Total = sum(`Population_Total`, na.rm = TRUE),
    .groups = 'drop'
  )

# calculate DA/PP 
DM01_ICBPopulationMerge1c <- DM01_ICBPopulationMerge1c %>%
  mutate(`DA/PP` = `Total_Activity` / `Population_Total`)

DM01_ICBPopulationMerge1c <- DM01_ICBPopulationMerge1c %>%
  filter(!grepl("TOTAL", `Diagnostic Tests`, ignore.case = TRUE)) #remove "total" values


# Convert Period to Date if it's not already
DM01_ICBPopulationMerge1c$Period <- as.Date(DM01_ICBPopulationMerge1c$Period)

# Aggregate data by month and Diagnostic Tests
DM01_ICBPopulationMerge1c_aggregated <- DM01_ICBPopulationMerge1c %>%
  group_by(Period = floor_date(Period, "month"), `Diagnostic Tests`) %>%
  summarise(`DA/PP` = mean(`DA/PP`, na.rm = TRUE), .groups = 'drop')

# Plot with smoothing
ggplot(DM01_ICBPopulationMerge1c_aggregated, aes(x = Period, y = `DA/PP`, colour = `Diagnostic Tests`)) +
  geom_line(alpha = 0.5) +
  geom_smooth(se = FALSE) +
  ggtitle("How Diagnostic Activity Per Person Has Changed between 2021 and 2024 For Each Diagnostic Test") +
  theme_minimal() +
  labs(x = "Period", y = "DA/PP")