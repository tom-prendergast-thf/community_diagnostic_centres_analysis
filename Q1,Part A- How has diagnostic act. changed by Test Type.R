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
  ggplot(data = DM01_data_Period_TA_agg, aes(x = Period, y = Total.Activity)) + geom_line() + labs(title='Total Diagnostic Activity Over Time', x='Year', y='Amount of Diagnostic Tests')
  
  
  
# template to create visualization on how tests have changed over time when grouping by type of test.
    #create new df containing period, total activity and diagnostic tests columns
  DM01_data_Period_TA_DT <- select(DM01_data, 'Period', 'Total Activity', 'Diagnostic Tests')

# trying to find how to group by period if diagnostic test character is the same
  
# ATTEMPT 1
# Group by 'Period' if 'Diagnostic Tests' characters are equivalent
  DM01_data_Period_TA_DT_agg <- DM01_data_Period_TA_DT %>%
    group_by('Period', 'Diagnostic Tests') %>%
      summarise(sum('Total Activity')) %>%
  
# Print the result
  print(DM01_data_period_TA_DT_agg)
# not working due to invalide (chaaracter) argument
  
#ATTEMPT 2
  DM01_data_Period_TA_DT %>%
    group_by(Period, 'Diagnostic Tests',) %>%
    summarise(sum('Total Activity')) %>%
    data.frame() -> DM01_data_Period_TA_DT_agg1
  print(DM01_data_Period_TA_DT_agg1)
  
#ATTEMPT 3
  # Group by similar characters in the "Diagnostic Tests" column
  DM01_data_Period_TA_DT_agg1 <- DM01_data_Period_TA_DT %>%
    group_by(Period, 'Diagnostic Tests') %>%
    summarise(Total_Activity = sum('Total Activity')) %>%
    as.data.frame()
  
# ATTEMPT 4 - this one works. 
  # for reference, this is the website I used (https://www.geeksforgeeks.org/group-by-function-in-r-using-dplyr/)
# aggregating Total activity data by Type of Diagnostic Test, and Month of test.
DM01_data_Period_TA_DT_agg = DM01_data_Period_TA_DT %>% group_by(Period, `Diagnostic Tests`) %>%
  summarise(Diagnostic_Activity = sum(`Total Activity`), 
            .groups = 'drop')
view(DM01_data_Period_TA_DT_agg)

# attempting to create a graph of the below.
ggplot(data = DM01_data_Period_TA_DT_agg, aes(x = Period, y = Total.Activity, Diagnostic.Test)) + geom_line() + labs(title='Total Diagnostic Activity Over Time Per Type of Test', x='Year', y='Amount of Diagnostic Tests')
    # the above is unhelpful -I need a multi line graph, to allow for visualisation of growth per test type.

#this should createa multi lie graph, but it produces no values ???
ggplot(DM01_data_Period_TA_DT_agg, aes(x = factor(Period), y = 'Diagnostic_Activity', colour = 'Diagnostic.Tests')) +
  geom_line()
  