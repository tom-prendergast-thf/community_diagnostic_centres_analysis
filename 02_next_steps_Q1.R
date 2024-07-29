
### QUESTION 1 continued: WHAT PORTION OF DIAGNOSTIC ACTIVITY IS ACCOUNTED FOR BY CDCs? 

# Step 1: Download trust to CDC matching data 




# Step 2: Join DM01 data to CDC data
# This join will need to be performed based on the trust, date and test type
# Best approach would probably be a left join, with the DM01 data as the left dataset




# Step 3: Compute percentage of activity per month accounted for by CDCs, on each level of interest.
# If you aggregate this on higher levels than trust/test type, the percentages should be re-calculated based on the aggregated sums of activity.





# Step 4: Visualise portion of CDC activity by test type
# It would be interesting to see this overall (or for select time period, say the past 12 months), with a horizontal bar chart
# And time series for test types would be good as well




# Step 5: Visualise portion of CDC activity by ICB
# Trust level is also of interest, but won't be easily visualised as there's so many of them (aside from showing a distribution, or in comparison to another variable on a scatter plot)










