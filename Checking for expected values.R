###Checks before analysis - making sure data has loaded as expected!###


#count unique values in Provider Parent Column
n_distinct(DM01_data$`Provider Parent Name`)
86
#count unique values in provider org code column
n_distinct(DM01_data$`Provider Parent Org Code`)
43
# we can see that there are 43 unique values, which is correct as there are 42 ICB's in England.


n_distinct(DM01_data$Period)
41
uniquevalues_period <- unique(DM01_data$Period)
print(uniquevalues_period)
[1] "2021-04-01" "2022-04-01" "2023-04-01" "2024-04-01" "2021-08-01" "2022-08-01" "2023-08-01" "2021-12-01" "2022-12-01" "2023-12-01" "2021-02-01" "2022-02-01" "2023-02-01"
[14] "2024-02-01" "2021-01-01" "2022-01-01" "2023-01-01" "2024-01-01" "2021-07-01" "2022-07-01" "2023-07-01" "2021-06-01" "2022-06-01" "2023-06-01" "2021-03-01" "2022-03-01"
[27] "2023-03-01" "2024-03-01" "2021-05-01" "2022-05-01" "2023-05-01" "2024-05-01" "2021-11-01" "2022-11-01" "2023-11-01" "2021-10-01" "2022-10-01" "2023-10-01" "2021-09-01"
[40] "2022-09-01" "2023-09-01"
order(uniquevalues_period)
uniquevalues_period[order(uniquevalues_period)]
[1] "2021-01-01" "2021-02-01" "2021-03-01" "2021-04-01" "2021-05-01" "2021-06-01" "2021-07-01" "2021-08-01" "2021-09-01" "2021-10-01" "2021-11-01" "2021-12-01" "2022-01-01"
[14] "2022-02-01" "2022-03-01" "2022-04-01" "2022-05-01" "2022-06-01" "2022-07-01" "2022-08-01" "2022-09-01" "2022-10-01" "2022-11-01" "2022-12-01" "2023-01-01" "2023-02-01"
[27] "2023-03-01" "2023-04-01" "2023-05-01" "2023-06-01" "2023-07-01" "2023-08-01" "2023-09-01" "2023-10-01" "2023-11-01" "2023-12-01" "2024-01-01" "2024-02-01" "2024-03-01"
[40] "2024-04-01" "2024-05-01"

#have now ordered the period values, allowing confirmation that all months are present between period of 01-01-2021 and 01-05-2024.


#### Checks for Question 1d ####
# checking to see why merge wont work
#are merge columns equal?
column_comparison <- all.equal(recent_DM01_data_agg$Period, CDC_data_agg$Period)
print(column_comparison)

column_comparison2 <-all.equal(recent_DM01_data_agg$`Diagnostic Tests`, CDC_data_agg$`Diagnostic Tests`)
print(column_comparison2)

column_comparison3 <- all.equal(recent_DM01_data_agg$`Commissioner Parent Org Code`, CDC_data_agg$`Commissioner Parent Org Code`)
print(column_comparison3)
#yes to all.

