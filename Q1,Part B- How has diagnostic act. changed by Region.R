# template to create visualization on how tests have changed over time when grouping by region

#first - checking how many ICB's there are.
#count unique values in Provider Parent Org Code column
n_distinct(DM01_data$`Provider Parent Org Code`)
# there are 43 - accurate as there are 42 ICBS. 

#creating a tibble, including Diagnostic Activity,  Period and ICB
DM01_data_Period_TA_ICB <- select(DM01_data, 'Period', 'Total Activity', `Provider Parent Org Code`)

#create tibble, aggregating diagnostic activity by Period and Provider Parent Org Code
DM01_data_Period_TA_ICB_agg = DM01_data_Period_TA_ICB %>% group_by(Period, `Provider Parent Org Code`) %>%
  summarise(Diagnostic_Activity = sum(`Total Activity`), 
            .groups = 'drop')
View(DM01_data_Period_TA_ICB_agg)

# Data is now aggregated by ICB  (Provider Parent Org Code). Now, want to link it to ODS General search file that categories Various ICB Codes into their respective 'National Regions'
# View ODS ICB//Region FIle
library(readr)
ICB_National_Region_ODS <- read_csv("ODS General Search (Include headers).csv")
#rename Code column to `Provider Parent Org Code` so that is is in same format as DM01_data_period_TA_ICB_agg
ICB_National_Region_ODS <- rename(ICB_National_Region_ODS, `Provider Parent Org Code` = Code)
View(ICB_National_Region_ODS)

#link ICB/ National Region dataset and DM01 data set so that we can group by Region
DM01_data_Period_TA_ICB_agg_Region = merge(x = DM01_data_Period_TA_ICB_agg, y = ICB_National_Region_ODS, by = "Provider Parent Org Code")

DM01_data_Period_TA_ICB_agg_Region <- DM01_data_Period_TA_ICB_agg_Region[order(as.Date(DM01_data_Period_TA_ICB_agg_Region$Period, format="%m/%d/%Y")),]
View(DM01_data_Period_TA_ICB_agg_Region)
#group linked data set by Region
DM01_data_Period_TA_ICB_agg_Region_agg = DM01_data_Period_TA_ICB_agg_Region %>% group_by(Period, `National Grouping Name`) %>%
  summarise(Diagnostic_Activity = sum(`Diagnostic_Activity`), 
            .groups = 'drop')
View(DM01_data_Period_TA_ICB_agg_Region_agg)
# create visualization of the above
ggplot(DM01_data_Period_TA_ICB_agg_Region_agg, aes(x = `Period`, y = `Diagnostic_Activity`, colour = `National Grouping Name`)) +
  geom_line() +
  ggtitle("Comparing NHSE Diagnostic Activity Trends by Geographical Area")

