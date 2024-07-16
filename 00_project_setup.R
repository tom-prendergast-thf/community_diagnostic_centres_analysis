# Load in packages

packages <- c('tidyverse', 'rvest', 'here', 'lubridate')

installed_packages <- packages %in% row.names(installed.packages())

if (any(installed_packages == FALSE)){
  install.packages(packages[!installed_packages])
}

lapply(packages, library, character.only = TRUE)


## Set up folder structure

ifelse(!dir.exists(file.path(here('raw_data/'))), dir.create(file.path(here('raw_data/'))), print('Raw data directory already exists'))  

ifelse(!dir.exists(file.path(here('outputs/'))), dir.create(file.path(here('outputs/'))), print('Outputs directory already exists'))


## Download data

if(file.exists('raw_data/DM01-MAY-2024-full-extract.csv')){
  
} else {
  
  # URLs 
  
  dm01_24_25_link <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/monthly-diagnostics-data-2024-25/'
  
  dm01_23_24_link <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/monthly-diagnostics-data-2023-24/'
  
  dm01_22_23_link <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/monthly-diagnostics-data-2022-23/'
  
  dm01_21_22_link <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/monthly-diagnostics-data-2021-22/'
  
  dm01_20_21_link <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/monthly-diagnostics-data-2020-21/'
  
  dm01_19_20_link <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/monthly-diagnostics-data-2019-20/'  
  
  dm01_18_19_link <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/monthly-diagnostics-data-2018-19/'
  
  dm01_17_18_link <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/monthly-diagnostics-data-2017-18/'
  
  dm01_16_17_link <- 'https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/monthly-diagnostics-waiting-times-and-activity/monthly-diagnostics-data-2016-17/'
  
  # Make list of all urls 
  
  all_urls <- c(dm01_24_25_link, dm01_23_24_link, dm01_22_23_link, dm01_21_22_link, dm01_20_21_link, dm01_19_20_link, dm01_18_19_link, dm01_17_18_link, dm01_16_17_link)
  
  
  # Scrape for zips 
  
  scrape_zips <- function(url){
    
    data_nodes <- read_html(url) %>%  # Extract links to all the zips available on the webpage
      html_elements('a') %>%
      html_attr('href') %>%
      as.data.frame() %>%
      rename(links = '.') %>%
      filter(grepl('.zip', links) == TRUE)
    
    for (i in 1:nrow(data_nodes)){      # Unpack all zips and place them in the raw_data directory
      temp <- tempfile()
      
      download.file(data_nodes$links[i], temp)
      
      unzip(temp, exdir = 'raw_data/')
      
      unlink(temp)
      
    }
    
  }
  
  # Download all files from zips using above function
  lapply(all_urls, function(x){scrape_zips(url = x)})
  
}

