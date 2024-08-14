### Q2 - 

# loading and linking trust data

CDC_Join_Trusts <- full_join(CDC_data, cdc_trust_lookup, by = c("Provider Org Code" = "Code"))
View(CDC_Join_Trusts)

#######
### i lost the recent DM01 data so the below remakes it ###
#ensure period column is in date format 
DM01_data$Period <- as.Date(DM01_data$Period)
#filter to get only dates after 2023.01.01
recent_DM01_data <- DM01_data %>% filter(Period >= as.Date("2023-03-01"))
#display filtered df
View(recent_DM01_data)

## aggegating DM01 data and CDC x Trust Join Data
recent_DM01_data_1 <- select(recent_DM01_data, 'Period', `Diagnostic Tests`, `Commissioner Parent Org Code`, `Total Activity`)
recent_DM01_data_agg <- recent_DM01_data_1 %>% group_by(Period, `Commissioner Parent Org Code`,`Diagnostic Tests`) %>%
  summarise( `Total Activity` = sum( `Total Activity`), 
             .groups = 'drop')
recent_DM01_data_agg <- recent_DM01_data_agg %>% arrange(ymd(Period))
recent_DM01_data_agg <- na.omit(recent_DM01_data_agg)
View(recent_DM01_data_agg)

CDC_Join_Trusts_Agg <- select(CDC_Join_Trusts, 'Period', `Provider Org Code`, `Diagnostic Tests`, `Commissioner Parent Org Code`, `Name`, `Total Activity`)
CDC_Join_Trusts_Agg <- CDC_Join_Trusts_Agg %>% group_by(Period, `Commissioner Parent Org Code`,`Diagnostic Tests`, `Provider Org Code`, `Name`) %>%
  summarise( `Total Activity` = sum( `Total Activity`), 
             .groups = 'drop')
CDC_Join_Trusts_Agg <- CDC_Join_Trusts_Agg %>% arrange(ymd(Period))
CDC_Join_Trusts_Agg <- na.omit(CDC_Join_Trusts_Agg)
View(CDC_Join_Trusts_Agg)

### MERGE : this produces merged table of DM01 data and CDC/ Trust !! Ready for use
DM01_CDC_Trusts_Merged <- merge(recent_DM01_data_agg, CDC_Join_Trusts_Agg, by = c("Period", "Commissioner Parent Org Code", "Diagnostic Tests"), all = TRUE)
view (DM01_CDC_Trusts_Merged)
