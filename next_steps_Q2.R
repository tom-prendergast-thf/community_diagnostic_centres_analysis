
### QUESTION 2: HOW HAS DIAGNOSTIC ACTIVITY PER PERSON CHANGED SINCE THE INTRODUCTION OF CDCs?
# This will need to be done on an ICB level as population estimates are not available for lower geographic levels

# Step 1: Load in population data from NHS

ICB_populations <- read.csv('resources/ICB populations.csv')

ICB_populations$Total <- as.numeric(sub(',', '', sub(',', '', ICB_populations$Total))) # This contained commas which R doesn't like - got rid of the commas and changed to a numeric variable type



# Step 2: Join population data to the joined DM01/CDC data, created earlier.




# Step 3: Compute diagnostic activity per person using joined data



# Step 4: Inspect and visualise over time for levels of interest







