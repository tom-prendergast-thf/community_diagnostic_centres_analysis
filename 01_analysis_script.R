# 01: CDC DATA ANALYSIS SCRIPT

# In this script, we perform the actual analysis of the CDC data and the related DM01 data
# Packages are loaded in, folders are created and data is downloaded by the project setup script, which is loaded
# via the source command at the beginning of this script. 
# From there, the CDC data is selected and loaded into the workspace, then combined into a single dataframe.
# The same is done for the simvastatinDM01 data. This is very large, so an alternative version is also filtered to only 
# cover the same space of time that the CDC data covers, for ease of comparison. 

source(here('00_project_setup.R'))

# Select and load in CDC data only 

CDC_data_names <- as.data.frame(list.files(path = 'raw_data/')) %>%
  rename(files = 1)%>% 
  filter(grepl('CDC', files) == TRUE)

all_CDC_data_list <- lapply(CDC_data_names$files, function(filename){
  df <- read_csv(paste0('raw_data/', filename))
  return(df)
}) 


# The below (CDC_data) collects all available CDC data in one dataframe - this is ready for analysis! 
CDC_data <- do.call('rbind', all_CDC_data_list) %>%
  mutate(Period = sub('DM01-', '', Period)) %>%       # turning the Period variable into an actual date
  mutate(Period = sub('-', ' ', Period)) %>%
  mutate(Period = my(Period))

earliest_CDC_date <- min(CDC_data$Period)


# Select and load in recent years of DM01 data
DM01_data_names <- as.data.frame(list.files(path = 'raw_data/')) %>%
  rename(files = 1)%>% 
  filter(grepl('CDC', files) == FALSE) %>%
  filter(grepl(paste(c('2024','2023','2022','2021'), collapse = '|'), files) == TRUE)

all_DM01_data_list <- lapply(DM01_data_names$files, function(filename){
  df <- read_csv(paste0('raw_data/', filename))
  return(df)
}) 

# The below collects all available DM01 data in one dataframe - this is ready for analysis! 
DM01_data <- do.call('rbind', all_DM01_data_list) %>%
  mutate(Period = sub('DM01-', '', Period)) %>%
  mutate(Period = sub('-', ' ', Period)) %>%
  mutate(Period = my(Period))

DM01_timeseries <- DM01_data %>% 
  group_by(Period) %>%
  summarise(activity = sum(`Total Activity`))

ggplot(data = DM01_timeseries, aes(x = Period, y = activity)) +
  geom_line()

ICB_populations <- read.csv('resources/ICB populations.csv')

ICB_populations$Total <- as.numeric(sub(',', '', sub(',', '', ICB_populations$Total))) # This contained commas which R doesn't like - got rid of the commas and changed to a numeric variable type


### QUESTION 1 continued: WHAT PORTION OF DIAGNOSTIC ACTIVITY IS ACCOUNTED FOR BY CDCs? 

# Step 1: Load in trust to CDC matching data 

cdc_trust_lookup <- read.csv('resources/CDC trust lookup.csv')

CDC_trust_join <- left_join(CDC_data, cdc_trust_lookup, by = c('Provider Org Code' = 'Code'))

CDC_trust_join <- CDC_trust_join %>%
  select(Period, provider_org_code = `Provider Org Code`, provider_org_name = `Provider Org Name`, commissioner_org_code = `Commissioner Org Code`, commissioner_name = `Commissioner Org Name`,
         test_code = `Diagnostic Tests Sort Order`, diagnostic_tests = `Diagnostic Tests`, CDC_waiting_list_activity = `Waiting List Activity`, CDC_planned_activity = `Planned Activity`, CDC_unscheduled_activity = `Unscheduled Activity`, CDC_total_activity = `Total Activity`)

CDC_data <- CDC_data %>%
  select(Period, provider_org_code = `Provider Org Code`, provider_org_name = `Provider Org Name`, commissioner_org_code = `Commissioner Org Code`, commissioner_name = `Commissioner Org Name`,
         test_code = `Diagnostic Tests Sort Order`, diagnostic_tests = `Diagnostic Tests`, CDC_waiting_list_activity = `Waiting List Activity`, CDC_planned_activity = `Planned Activity`, CDC_unscheduled_activity = `Unscheduled Activity`, CDC_total_activity = `Total Activity`)

CDC_data_grouped <- CDC_data %>%
  group_by(Period, commissioner_org_code, commissioner_name, test_code, diagnostic_tests) %>%
  summarise(CDC_waiting_list_activity = sum(CDC_waiting_list_activity), CDC_planned_activity = sum(CDC_planned_activity), CDC_unscheduled_activity = sum(CDC_unscheduled_activity), CDC_total_activity = sum(CDC_total_activity))
  

# Step 2: Join DM01 data to CDC data
# This join will need to be performed based on the trust, date and test type. The trust codes tend to be more reliable for joining in case of any typos/differences in capitalisation between the trust names in different sources.  
# Best approach would probably be a left join, with the DM01 data as the left dataset

DM01_data_grouped <- DM01_data %>%
  group_by(Period, `Commissioner Org Code`, `Commissioner Org Name`, `Diagnostic Tests`) %>%
  summarise(up_to_1_week = sum(`00 < 01 Week`),                
            one_two_weeks = sum(`01 < 02 Weeks`), two_three_weeks = sum(`02 < 03 Weeks`),               
            three_four_weeks = sum(`03 < 04 Weeks`), four_five_weeks = sum(`04 < 05 Weeks`),               
            five_six_weeks = sum(`05 < 06 Weeks`), six_seven_weeks = sum(`06 < 07 Weeks`),               
            seven_eight_weeks = sum(`07 < 08 Weeks`), eight_nine_weeks = sum(`08 < 09 Weeks`),               
            nine_ten_weeks = sum(`09 < 10 Weeks`), ten_eleven_weeks = sum(`10 < 11 Weeks`),               
            eleven_twelve_weeks = sum(`11 < 12 Weeks`), twelve_thirteen_weeks = sum(`12 < 13 Weeks`),               
            over_13_weeks = sum(`13+ Weeks`), dm01_total_WL = sum(`Total WL`),                    
            dm01_WL_activity = sum(`Waiting List Activity`), dm01_planned_activity = sum(`Planned Activity`),            
            dm01_unscheduled_activity = sum(`Unscheduled Activity`), dm01_total_activity = sum(`Total Activity`))

by <- join_by(Period, `Commissioner Org Code` == commissioner_org_code, `Commissioner Org Name` == commissioner_name, `Diagnostic Tests` == diagnostic_tests)


# Step 3: Compute percentage of activity per month accounted for by CDCs, on each level of interest.
# If you aggregate this on higher levels than trust/test type, the percentages should be re-calculated based on the aggregated sums of activity.

all_joined <- left_join(DM01_data_grouped, CDC_data_grouped, by) %>%
  mutate(proportion_CDC_activity = CDC_total_activity/dm01_total_activity) %>%
  mutate(non_CDC_activity = dm01_total_activity-CDC_total_activity)

summable_joined <- all_joined

summable_joined$CDC_total_activity[is.na(summable_joined$CDC_total_activity)] <- 0

all_joined_totals <- summable_joined %>%
  group_by(Period) %>%
  summarise(dm01_total_activity = sum(dm01_total_activity), CDC_total_activity = sum(CDC_total_activity)) %>%
  mutate(non_CDC_activity = dm01_total_activity-CDC_total_activity)

all_joined_totals %>%
  pivot_longer(cols = 2:4, names_to = 'metric', values_to = 'values') %>%
  filter(metric != 'dm01_total_activity') %>%
  ggplot(., aes(x= Period, y = values, fill = metric)) +
  geom_area()+
  theme_minimal()

# Step 4: Visualise portion of CDC activity by test type
# It would be interesting to see this overall (or for select time period, say total of the past 12 months), with a horizontal bar chart
# And time series for test types would be good as well

all_joined_test_totals <- summable_joined %>%
  group_by(Period, `Diagnostic Tests`) %>%
  summarise(dm01_total_activity = sum(dm01_total_activity), CDC_total_activity = sum(CDC_total_activity))


# Step 5: Visualise portion of CDC activity by ICB and trust.
# ICB level would be interesting on a bar chart (either totals or monthly averages rather than over time)
# Trust level could be visualised either through a distribution chart/histogram, or a scatter plot in comparison to any other variables of interest (there may not be any for now)


### QUESTION 2: HOW HAS DIAGNOSTIC ACTIVITY PER PERSON CHANGED SINCE THE INTRODUCTION OF CDCs?
# This will need to be done on an ICB level as population estimates are not available for lower geographic levels

# Step 1: Load in population data from NHS

ICB_populations <- read.csv('resources/ICB populations.csv')

ICB_populations$Total <- as.numeric(sub(',', '', sub(',', '', ICB_populations$Total))) # This contained commas which R doesn't like - got rid of the commas and changed to a numeric variable type



# Step 2: Join population data to the joined DM01/CDC data, created earlier.
# This will need to be done on an ICB level, based on ICB codes or names (the commissioning body). Codes tend to be more reliable in case of any typos/differences in capitalisation between the ICB names in different sources.  



# Step 3: Compute diagnostic activity per person using joined data



# Step 4: Inspect and visualise over time for levels of interest



### QUESTION 3: HOW DO DIAGNOSTIC WAITING TIMES COMPARE BETWEEN AREAS WITH AND WITHOUT CDCs?


# Step 1: In joined DM01/CDC data, create an additional variable which flags which trusts have a CDC associated with them, and which do not


# Step 2: A simple comparison of the distribution of waiting times for each test between trusts which have a CDC compared to those which don't, first presented in the aggregate.  
# There are a few different ways of approaching this - waiting times can be treated as a categorical variable, as they are presented in the data, or they can be 
# converted into numbers. If converting to numbers, box and whisker graphs would be a good way to compare. 


# Step 3: Create a scatter plot (trust and test level) of average waiting times vs proportion of activity performed by CDCs.
# Waiting times will need to be numeric to compute averages. If a lot of trusts have no associated CDC, it may be an idea to only look at trusts with CDCs. 



# Step 4: Difference-in-difference comparisons? We'll discuss this when I'm back from leave





<<<<<<< HEAD
head(recent_DM01_data)
=======

>>>>>>> cdc73cd (Activity script updates)

### i lost the recent DM01 data so the below remakes it ###
  #ensure period column is in date format 
  DM01_data$Period <- as.Date(DM01_data$Period)
  #filter to get only dates after 2023.01.01
  recent_DM01_data <- DM01_data %>% filter(Period >= as.Date("2023-03-01"))
  #display filtered df
  View(recent_DM01_data)

  save(recent_DM01_data, file = "recent_DM01_data.RData")
  load("recent_DM01_data.RData")
