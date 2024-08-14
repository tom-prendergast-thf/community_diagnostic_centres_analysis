# In this script, analysis for the question 'How has diagnostic activity changed over time?" is started.


# create new data frame containing just Period and Total Activity Columns
DM01_data_Period_TA <- select(DM01_data, 'Period', 'Total Activity')

#create a new data frame, aggregating Total Activity when grouped by 'Period'
DM01_data_Period_TA %>%
  group_by(Period) %>%
  summarise_all(sum) %>%
  data.frame() -> DM01_data_Period_TA_agg
  print(DM01_data_Period_TA_agg)
  
  # create a visualization of the above
  ggplot(data = DM01_data_Period_TA_agg, aes(x = Period, y = Total.Activity)) + geom_line() + labs(title='Exploring NHSE Diagnostic Activity Trends Over Time', x='Year', y='Amount of Diagnostic Tests')
  
  
  
# template to create visualization on how tests have changed over time when grouping by type of test.
    #create new df containing period, total activity and diagnostic tests columns
  DM01_data_Period_TA_DT <- select(DM01_data, 'Period', 'Total Activity', 'Diagnostic Tests')
  
 # aggregating Total activity data by Type of Diagnostic Test, and Month of test.
  # for reference, this is the website I used (https://www.geeksforgeeks.org/group-by-function-in-r-using-dplyr/)
DM01_data_Period_TA_DT_agg = DM01_data_Period_TA_DT %>% group_by(Period, `Diagnostic Tests`) %>%
  summarise(Diagnostic_Activity = sum(`Total Activity`), 
            .groups = 'drop')
View(DM01_data_Period_TA_DT_agg)
#removing rows that show 'total' number of tests : these are not needed.
DM01_data_Period_TA_DT_agg <- DM01_data_Period_TA_DT_agg %>%
  filter(!grepl("total", `Diagnostic Tests`, ignore.case = TRUE))

#this  creates multi-line graph based upon above aggregation
options(scipen = 999)
ggplot(DM01_data_Period_TA_DT_agg, aes(x = Period, y = Diagnostic_Activity, colour = `Diagnostic Tests`)) +
  geom_line() + 
  ggtitle("Comparing NHSE Diagnostic Activity Trends by Type of Diagnostic Test")

