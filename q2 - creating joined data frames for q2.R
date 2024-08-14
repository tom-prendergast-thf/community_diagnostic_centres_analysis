### QUESTION 2 -  2: HOW HAS DIAGNOSTIC ACTIVITY PER PERSON CHANGED SINCE THE INTRODUCTION OF CDC - creating joined data frame

# Step 1: Load in population data from NHS

ICB_populations <- read.csv('resources/ICB populations.csv')
ICB_populations$Total <- as.numeric(sub(',', '', sub(',', '', ICB_populations$Total))) # This contained commas which R doesn't like - got rid of the commas and changed to a numeric variable type

# Step 2: Join population data to the joined DM01/CDC data, created earlier (via ICB/ Commissioner Parent Org Code)
    # i need to make new join, as former one involves commisioner parent code, i need commissioner code

#aggregating each df
recent_DM01_data_2 <- select(recent_DM01_data, 'Period', `Diagnostic Tests`, `Commissioner Parent Org Code`, `Commissioner Org Name`, `Commissioner Org Code`, `Total Activity`)
recent_DM01_data_agg2 <- recent_DM01_data_2 %>% group_by(Period, `Commissioner Parent Org Code`, `Commissioner Org Code`, `Commissioner Org Name`, `Diagnostic Tests`) %>%
  summarise( `Total Activity` = sum( `Total Activity`), 
             .groups = 'drop')
recent_DM01_data_agg2 <- recent_DM01_data_agg2 %>% arrange(ymd(Period))
recent_DM01_data_agg2 <- na.omit(recent_DM01_data_agg2)
View(recent_DM01_data_agg2)

CDC_Join_Trusts2 <- select(CDC_Join_Trusts, 'Period', `Diagnostic Tests`, `Commissioner Parent Org Code`, `Commissioner Org Code`, `Commissioner Org Name`, `Name`, `Total Activity`)
CDC_Join_Trusts_Agg2 <- CDC_Join_Trusts2 %>% group_by(Period, `Commissioner Parent Org Code`, `Commissioner Org Code`, `Commissioner Org Name`, `Name`, `Diagnostic Tests`) %>%
  summarise( `Total Activity` = sum( `Total Activity`), 
             .groups = 'drop')
CDC_Join_Trusts_Agg2 <- CDC_Join_Trusts_Agg2 %>% arrange(ymd(Period))
CDC_Join_Trusts_Agg2 <- na.omit(CDC_Join_Trusts_Agg2)
View(CDC_Join_Trusts_Agg2)

### MERGE
DM01_CDC_Trusts_Merged2 <- merge(recent_DM01_data_agg2, CDC_Join_Trusts_Agg2, by = c("Period", "Commissioner Org Code", "Commissioner Org Name", "Commissioner Parent Org Code", "Diagnostic Tests"), all = TRUE)
view (DM01_CDC_Trusts_Merged2)
DM01_CDC_Trusts_Merged2 <<- DM01_CDC_Trusts_Merged2

### Merge new DM01_CDC_Trust data with ICB population data via Commissioner Org Code Column
DM01_CDC_Trusts_ICBPopulationMerge <- merge(DM01_CDC_Trusts_Merged2, ICB_Populations2, by = c("Commissioner Org Code"), all = TRUE)
# Delete unneeded columns
DM01_CDC_Trusts_ICBPopulationMerge <- subset(DM01_CDC_Trusts_ICBPopulationMerge, select = -c(SICBL.2023.Name, SICBL.2023.Code, ICB.2023.Code, NHSER.2023.Code))

  ### to compare before and after introduction of CDCs, must also merge population data with larger DM01 data file
    DM01_data2 <- select(DM01_data, 'Period', `Diagnostic Tests`, `Commissioner Parent Org Code`, `Commissioner Org Name`, `Commissioner Org Code`, `Total Activity`)
    DM01_data_agg2 <- DM01_data2 %>% group_by(Period, `Commissioner Parent Org Code`, `Commissioner Org Code`, `Commissioner Org Name`, `Diagnostic Tests`) %>%
      summarise( `Total Activity` = sum( `Total Activity`), 
                 .groups = 'drop')
    DM01_data_agg2 <- DM01_data_agg2 %>% arrange(ymd(Period))
    DM01_data_agg2 <- na.omit(DM01_data_agg2)
    View(DM01_data_agg2)
    
    ### Merge new DM01_data_agg2 data with ICB population data via Commissioner Org Code Column
    DM01_ICBPopulationMerge <- merge(DM01_data_agg2, ICB_Populations2, by = c("Commissioner Org Code"), all = TRUE)
    # Delete unneeded columns
    DM01_ICBPopulationMerge <- subset(DM01_ICBPopulationMerge, select = -c(SICBL.2023.Name, SICBL.2023.Code, ICB.2023.Code, NHSER.2023.Code))
    