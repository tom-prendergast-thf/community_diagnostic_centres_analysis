# 01: CDC DATA ANALYSIS SCRIPT

# In this script, we perform the actual analysis of the CDC data and the related DM01 data
# Packages are loaded in, folders are created and data is downloaded by the project setup script, which is loaded
# via the source command at the beginning of this script. 
# From there, the CDC data is selected and loaded into the workspace, then combined into a single dataframe.
# The same is done for the DM01 data. This is very large, so an alternative version is also filtered to only 
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

# The below (CDC_data) collects all available DM01 data in one dataframe - this is ready for analysis! 
DM01_data <- do.call('rbind', all_DM01_data_list) %>%
  mutate(Period = sub('DM01-', '', Period)) %>%
  mutate(Period = sub('-', ' ', Period)) %>%
  mutate(Period = my(Period))

# However, the above data is massive and slow to work with - for ease of working, here's also a version which 
# only covers the same period that the CDC data does

recent_DM01_data <- DM01_data %>%
  filter(Per > earliest_CDC_date)



