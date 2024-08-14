# The NHS aim is to not have waiting lists of more than 6 weeks. Hence, we will explore how the number of patients on the waiting list for more than 6 weeks has changed with time
#create tibble containing period, 6<7 weeks column
DM01_data_Period_6WeekWait <- select(DM01_data, 'Period', `06 < 07 Weeks`)
View(DM01_data_Period_6WeekWait)

#Group by Period 
DM01_data_Period_6WeekWait_agg = DM01_data_Period_6WeekWait %>% group_by(Period) %>%
  summarise( `06 < 07 Weeks` = sum( `06 < 07 Weeks`), 
            .groups = 'drop')
View(DM01_data_Period_6WeekWait_agg)

# Create visualization
ggplot(DM01_data_Period_6WeekWait_agg, aes(x = `Period`, y = `06 < 07 Weeks`)) +
  geom_line() +
  ggtitle("Exploring NHSE Waiting List Trends") + 
  labs(x = "Year", y = "Number of Patients Waiting More Than 6 Weeks for a Diagnostic Test")






#Now, compare number of patiets on waiting list for more than 6 weeks by REGION.
#create tibble containing period, 6<7 weeks column, and ICB clum
DM01_data_Period_6WeekWait_ICB <- select(DM01_data, 'Period', `06 < 07 Weeks`, `Provider Parent Org Code`)
View(DM01_data_Period_6WeekWait_ICB)

#Group by Period & ICB
DM01_data_Period_6WeekWait_ICB_agg <- DM01_data_Period_6WeekWait_ICB %>% group_by(Period, `Provider Parent Org Code`) %>%
  summarise( `06 < 07 Weeks` = sum( `06 < 07 Weeks`), 
             .groups = 'drop')

# link ICB codes to region
DM01_data_Period_6WeekWait_ICB_agg_Region = merge(x = DM01_data_Period_6WeekWait_ICB_agg, y = ICB_National_Region_ODS, by = "Provider Parent Org Code")

DM01_data_Period_6WeekWait_ICB_agg_Region <- DM01_data_Period_6WeekWait_ICB_agg_Region[order(as.Date(DM01_data_Period_6WeekWait_ICB_agg_Region$Period, format="%m/%d/%Y")),]
View(DM01_data_Period_TA_ICB_agg_Region)
#group linked data set by Region
DM01_data_Period_6WeekWait_ICB_agg_Region = DM01_data_Period_6WeekWait_ICB_agg_Region %>% group_by(Period, `National Grouping Name`) %>%
  summarise(`06 < 07 Weeks` = sum(`06 < 07 Weeks`), 
            .groups = 'drop')
View(DM01_data_Period_6WeekWait_ICB_agg_Region)

#create visualation
ggplot(DM01_data_Period_6WeekWait_ICB_agg_Region, aes(x = `Period`, y = `06 < 07 Weeks`, colour = `National Grouping Name`)) +
  geom_line() +
  ggtitle("Comparing NHSE Waiting List Trends by Geographical Area")





#Now, compare number of patients on waiting list for more than 6 weeks by type of diagnostic test.
#create tibble containing period, 6<7 weeks column, and Diagnostic Test
DM01_data_Period_6WeekWait_DT <- select(DM01_data, 'Period', '06 < 07 Weeks', 'Diagnostic Tests')

# aggregating Total activity data by Type of Diagnostic Test, and Month of test.
# for reference, this is the website I used (https://www.geeksforgeeks.org/group-by-function-in-r-using-dplyr/)
DM01_data_Period_6WeekWait_DT_agg = DM01_data_Period_6WeekWait_DT %>% group_by(Period, `Diagnostic Tests`) %>%
  summarise(`06 < 07 Weeks` = sum(`06 < 07 Weeks`), 
            .groups = 'drop')
View(DM01_data_Period_6WeekWait_DT_agg)
#removing rows that show 'total' number of tests : these are not needed.
DM01_data_Period_6WeekWait_DT_agg <- DM01_data_Period_6WeekWait_DT_agg %>%
  filter(!grepl("total", `Diagnostic Tests`, ignore.case = TRUE))

#plot the above
options(scipen = 999)
ggplot(DM01_data_Period_6WeekWait_DT_agg, aes(x = Period, y = `06 < 07 Weeks`, colour = `Diagnostic Tests`)) +
  geom_line() + 
  ggtitle("Comparing NHSE Waiting List Tr ends by Type of Diagnostic Test")