
# Select and load in recent years of DM01 data
DM01_data_names1 <- as.data.frame(list.files(path = 'raw_data/')) %>%
  rename(files = 1)%>% 
  filter(grepl('CDC', files) == FALSE) %>%
  filter(grepl(paste(c('2024','2023','2022','2021', '2020', '2019', '2018'), collapse = '|'), files) == TRUE)

all_DM01_data_list1 <- lapply(DM01_data_names1$files, function(filename){
  df <- read_csv(paste0('raw_data/', filename))
  return(df)
}) 

# The below (CDC_data) collects all available DM01 data in one dataframe - this is ready for analysis! 
DM01_data1 <- do.call('rbind', all_DM01_data_list1) %>%
  mutate(Period = sub('DM01-', '', Period)) %>%
  mutate(Period = sub('-', ' ', Period)) %>%
  mutate(Period = my(Period))
