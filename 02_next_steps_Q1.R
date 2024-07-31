
### QUESTION 1 continued: WHAT PORTION OF DIAGNOSTIC ACTIVITY IS ACCOUNTED FOR BY CDCs? 

# Step 1: Load in trust to CDC matching data 

cdc_trust_lookup <- read.csv('resources/CDC trust lookup.csv')


# Step 2: Join DM01 data to CDC data
# This join will need to be performed based on the trust, date and test type. The trust codes tend to be more reliable for joining in case of any typos/differences in capitalisation between the trust names in different sources.  
# Best approach would probably be a left join, with the DM01 data as the left dataset




# Step 3: Compute percentage of activity per month accounted for by CDCs, on each level of interest.
# If you aggregate this on higher levels than trust/test type, the percentages should be re-calculated based on the aggregated sums of activity.





# Step 4: Visualise portion of CDC activity by test type
# It would be interesting to see this overall (or for select time period, say total of the past 12 months), with a horizontal bar chart
# And time series for test types would be good as well




# Step 5: Visualise portion of CDC activity by ICB and trust.
# ICB level would be interesting on a bar chart (either totals or monthly averages rather than over time)
# Trust level could be visualised either through a distribution chart/histogram, or a scatter plot in comparison to any other variables of interest (there may not be any for now)









