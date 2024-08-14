### Q3 - HOW DO DIAGNOSTIC WAITING TIMES COMPARE BETWEEN AREAS WITH AND WITHOUT CDCs?

CDC_Join_Trusts <- full_join(CDC_data, cdc_trust_lookup, by = c("Provider Org Code" = "Code"))
View(CDC_Join_Trusts)
#ensure period column is in date format 
DM01_data$Period <- as.Date(DM01_data$Period)
#filter to get only dates after 2023.01.01
recent_DM01_data <- DM01_data %>% filter(Period >= as.Date("2023-03-01"))
#display filtered df
View(recent_DM01_data)


# aggregate CDC_data
library(data.table)

# Convert data frame to data.table
DT <- as.data.table(CDC_Join_Trusts)

# Grouping and summing columns using data.table
summarized_DT <- DT[, .(
  `00 < 01 Week` = sum(`00 < 01 Week`, na.rm = TRUE),
  `01 < 02 Weeks` = sum(`01 < 02 Weeks`, na.rm = TRUE),
  `02 < 03 Weeks` = sum(`02 < 03 Weeks`, na.rm = TRUE),
  `03 < 04 Weeks` = sum(`03 < 04 Weeks`, na.rm = TRUE),
  `04 < 05 Weeks` = sum(`04 < 05 Weeks`, na.rm = TRUE),
  `05 < 06 Weeks` = sum(`05 < 06 Weeks`, na.rm = TRUE),
  `06 < 07 Weeks` = sum(`06 < 07 Weeks`, na.rm = TRUE),
  `07 < 08 Weeks` = sum(`07 < 08 Weeks`, na.rm = TRUE),
  `08 < 09 Weeks` = sum(`08 < 09 Weeks`, na.rm = TRUE),
  `09 < 10 Weeks` = sum(`09 < 10 Weeks`, na.rm = TRUE),
  `10 < 11 Weeks` = sum(`10 < 11 Weeks`, na.rm = TRUE),
  `11 < 12 Weeks` = sum(`11 < 12 Weeks`, na.rm = TRUE),
  `12 < 13 Weeks` = sum(`12 < 13 Weeks`, na.rm = TRUE),
  `13+ Weeks` = sum(`13+ Weeks`, na.rm = TRUE),
  `Total WL` = sum(`Total WL`, na.rm = TRUE),
  `Waiting List Activity` = sum(`Waiting List Activity`, na.rm = TRUE),
  `Planned Activity` = sum(`Planned Activity`, na.rm = TRUE),
  `Unscheduled Activity` = sum(`Unscheduled Activity`, na.rm = TRUE),
  `Total Activity` = sum(`Total Activity`, na.rm = TRUE)
), by = .(Period, `Diagnostic Tests`, 
          `Provider Org Code`, 
          `Is.Operated.By...Code`, Name)]

# Convert back to data frame (optional)
summarized_CDC_Join_Trusts <- as.data.frame(summarized_DT)

#aggregate recent_DM01 data
library(data.table)
# Load the data.table package
library(data.table)

# Convert data frame to data.table
DT <- as.data.table(recent_DM01_data)

# Grouping and summing columns using data.table
summarized_DT <- DT[, .(
  `00 < 01 Week` = sum(`00 < 01 Week`, na.rm = TRUE),
  `01 < 02 Weeks` = sum(`01 < 02 Weeks`, na.rm = TRUE),
  `02 < 03 Weeks` = sum(`02 < 03 Weeks`, na.rm = TRUE),
  `03 < 04 Weeks` = sum(`03 < 04 Weeks`, na.rm = TRUE),
  `04 < 05 Weeks` = sum(`04 < 05 Weeks`, na.rm = TRUE),
  `05 < 06 Weeks` = sum(`05 < 06 Weeks`, na.rm = TRUE),
  `06 < 07 Weeks` = sum(`06 < 07 Weeks`, na.rm = TRUE),
  `07 < 08 Weeks` = sum(`07 < 08 Weeks`, na.rm = TRUE),
  `08 < 09 Weeks` = sum(`08 < 09 Weeks`, na.rm = TRUE),
  `09 < 10 Weeks` = sum(`09 < 10 Weeks`, na.rm = TRUE),
  `10 < 11 Weeks` = sum(`10 < 11 Weeks`, na.rm = TRUE),
  `11 < 12 Weeks` = sum(`11 < 12 Weeks`, na.rm = TRUE),
  `12 < 13 Weeks` = sum(`12 < 13 Weeks`, na.rm = TRUE),
  `13+ Weeks` = sum(`13+ Weeks`, na.rm = TRUE),
  `Total WL` = sum(`Total WL`, na.rm = TRUE),
  `Waiting List Activity` = sum(`Waiting List Activity`, na.rm = TRUE),
  `Planned Activity` = sum(`Planned Activity`, na.rm = TRUE),
  `Unscheduled Activity` = sum(`Unscheduled Activity`, na.rm = TRUE),
  `Total Activity` = sum(`Total Activity`, na.rm = TRUE)
), by = .(Period, `Provider Org Code`, `Diagnostic Tests`)]


# Convert back to data frame (optional)
summarized_recent_DM01_data <- as.data.frame(summarized_DT)

# join the two data frames : 
library(data.table)
# Convert data frames to data tables
DT1 <- as.data.table(summarized_recent_DM01_data)
DT2 <- as.data.table(summarized_CDC_Join_Trusts)
# Perform a left join, keeping all rows from DT1 and retaining NA values where there is no match in DT2
Q3CDC_DM01_Trusts <- DT2[DT1, on = .(`Is.Operated.By...Code` = `Provider Org Code`), allow.cartesian = TRUE]

#create flag wherever name = NA - this trust has no CDC associatded 
library(dplyr)

# Create the new data frame Q3flagged with a flag column
Q3flagged <- Q3CDC_DM01_Trusts %>%
  mutate(flag = ifelse(is.na(Name), 0, 1))
# note - 74400 columns produce 0 flag (trusts without CDC), 4917456 produce 1 flag

##aggregating by test type
# Create a new data frame with the specified columns
Q3flagged2 <- Q3flagged[, c("i.Period", "Provider Org Code", "Is.Operated.By...Code", 
                        "Name", "i.Diagnostic Tests", 
                        "i.00 < 01 Week", "i.01 < 02 Weeks", "i.02 < 03 Weeks", 
                        "i.03 < 04 Weeks", "i.04 < 05 Weeks", "i.05 < 06 Weeks", 
                        "i.06 < 07 Weeks", "i.07 < 08 Weeks", "i.08 < 09 Weeks", 
                        "i.09 < 10 Weeks", "i.10 < 11 Weeks", "i.11 < 12 Weeks", 
                        "i.12 < 13 Weeks", "i.13+ Weeks", "i.Total WL", 
                        "i.Waiting List Activity", "i.Planned Activity", 
                        "i.Unscheduled Activity", "i.Total Activity", "flag")]
Q3flagged2agg <- Q3flagged2 %>%
  group_by(`i.Diagnostic Tests`, flag) %>%
  summarise(across(
    where(is.numeric),
    ~sum(.x, na.rm = TRUE),
    .names = "sum_{col}"
  ))

# Load necessary libraries
library(dplyr)
library(tidyr)

# Reshape data from wide to long format
Q3flagged2agg_long <- Q3flagged2agg %>%
  pivot_longer(
    cols = starts_with("sum_i."),
    names_to = "Week_Range",
    values_to = "Value"
  )

# Define the unwanted values
unwanted_values <- c("sum_i.Total WL", "sum_i.Waiting List Activity", 
                     "sum_i.Planned Activity", "sum_i.Unscheduled Activity", 
                     "sum_i.Total Activity")

# Filter out rows where Week_Range contains unwanted values
Q3flagged2agg_filtered <- Q3flagged2agg_long %>%
  filter(!Week_Range %in% unwanted_values)

# View the filtered data frame
print(Q3flagged2agg_filtered)


