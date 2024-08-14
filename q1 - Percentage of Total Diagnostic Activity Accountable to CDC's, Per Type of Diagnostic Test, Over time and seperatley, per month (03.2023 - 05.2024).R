### "Percentage of Total Diagnostic Activity Accountable to CDC's, Per Type of Diagnostic Test, Over time and seperatley, per month (03.2023 - 05.2024) ###


## grouping by Diagnostic Test and producing visualisation ##
## the below produces a table which computes the percentage of total diagnostic activity accountable to CDC's  per DT ##
DM01_CDC_Merged_ByDT <- DM01_CDC_Merged %>%
  select(`Diagnostic Tests`, `Total Activity.x`, `Total Activity.y`)
DM01_CDC_Merged_ByDT1 <- aggregate(cbind(`Total Activity.x`,`Total Activity.y`) ~ `Diagnostic Tests`, DM01_CDC_Merged_ByDT, FUN=sum)
DM01_CDC_Merged_ByDT1$Percentage_CDC <- (DM01_CDC_Merged_ByDT1$`Total Activity.y`/ DM01_CDC_Merged_ByDT1$`Total Activity.x`) * 100
DM01_CDC_Merged_ByDT1 <- DM01_CDC_Merged_ByDT1[order(-DM01_CDC_Merged_NoMonthsJustICBS$Percentage_CDC), ]
View(DM01_CDC_Merged_ByDT1)


DM01_CDC_Merged_ByDT1 <- DM01_CDC_Merged_ByDT1 %>%
  drop_na()   # drop missing values
# Remove rows that contain 'Total' (case-insensitive) in any column
DM01_CDC_Merged_ByDT1 <- DM01_CDC_Merged_ByDT1 %>%
  filter(!if_any(everything(), ~ grepl("Total", .x, ignore.case = TRUE)))

## create visuals
ggp4 <- ggplot(DM01_CDC_Merged_ByDT1, 
               aes(x = reorder(`Diagnostic Tests`, Percentage_CDC), 
                   y = Percentage_CDC)) +    
  geom_bar(stat = "identity", fill = "darkblue") +  # Set bars to dark blue
  coord_flip() +  # Flip the coordinates for a horizontal bar graph
  labs(title = "Percentage of Total Diagnostic Activity Accountable to CDC's, Per Type Of Diagnostic Test (03.2023 - 05.2024)", x = "Diagnostic Test", y = "Percentage of Total Diagnostic Activity Accountable to CDC's (%)") +
  scale_y_continuous(breaks = seq(0, max(DM01_CDC_Merged_ByDT1$Percentage_CDC, na.rm = TRUE), by = 2)) +
  theme_minimal()
print(ggp4)



### "Percentage of Total Diagnostic Activity Accountable to CDC's, Per Type of Diagnostic Test, Per Month  (03.2023 - 05.2024) ###
DM01_CDC_Merged_ByDT_Month <- DM01_CDC_Merged %>%
  select(`Diagnostic Tests`, `Period`, `Total Activity.x`, `Total Activity.y`)

DM01_CDC_Merged_ByDT_Month <- DM01_CDC_Merged_ByDT_Month %>%
  group_by(Period, `Diagnostic Tests`) %>%
  summarise(
    `Total Activity.x` = sum(`Total Activity.x`, na.rm = TRUE),
    `Total Activity.y` = sum(`Total Activity.y`, na.rm = TRUE),
    .groups = 'drop'
  )
DM01_CDC_Merged_ByDT_Month <- na.omit(DM01_CDC_Merged_ByDT_Month)

# Remove rows that contain 'Total' (case-insensitive) in any column
DM01_CDC_Merged_ByDT_Month <- DM01_CDC_Merged_ByDT_Month %>%
  filter(!if_any(everything(), ~ grepl("Total", .x, ignore.case = TRUE)))

View(DM01_CDC_Merged_ByDT_Month)


### "Percentage of Total Diagnostic Activity Accountable to CDC's, Per Type of Diagnostic Test, Per Month  (03.2023 - 05.2024) ###
DM01_CDC_Merged_ByDT_Month <- DM01_CDC_Merged %>%
  select(`Diagnostic Tests`, `Period`, `Total Activity.x`, `Total Activity.y`)

DM01_CDC_Merged_ByDT_Month <- DM01_CDC_Merged_ByDT_Month %>%
  group_by(Period, `Diagnostic Tests`) %>%
  summarise(
    `Total Activity.x` = sum(`Total Activity.x`, na.rm = TRUE),
    `Total Activity.y` = sum(`Total Activity.y`, na.rm = TRUE),
    .groups = 'drop'
  )
DM01_CDC_Merged_ByDT_Month <- na.omit(DM01_CDC_Merged_ByDT_Month)

# Remove rows that contain 'Total' (case-insensitive) in any column
DM01_CDC_Merged_ByDT_Month <- DM01_CDC_Merged_ByDT_Month %>%
  filter(!if_any(everything(), ~ grepl("Total", .x, ignore.case = TRUE)))

#calculate percentages
DM01_CDC_Merged_ByDT_Month$Percentage_CDC <- (DM01_CDC_Merged_ByDT_Month$`Total Activity.y`/ DM01_CDC_Merged_ByDT_Month$`Total Activity.x`) * 100
DM01_CDC_Merged_ByDT_Month <- DM01_CDC_Merged_ByDT_Month[order(-DM01_CDC_Merged_ByDT_Month$Percentage_CDC), ]
View(DM01_CDC_Merged_ByDT_Month)

#create visualization 4
ggplot(DM01_CDC_Merged_ByDT_Month, aes(x = `Period`, y = `Percentage_CDC`, colour = `Diagnostic Tests`)) +
  geom_line() +
  ggtitle("The Percentage of Total NHSE Diagnostic Activity Accountable to CDC's Per Month, Per Diagnostic Test (4) ") +
  scale_x_date(name = "Period", date_breaks = "1 month", date_labels = "%b %Y") +  # Change frequency of x-axis date labels to month and year +
  scale_y_continuous(name = "Percentage of Diagnostic Activity Accountable to CDC", breaks = seq(0, 70, by = 1))  # Change frequency of y-axis labels