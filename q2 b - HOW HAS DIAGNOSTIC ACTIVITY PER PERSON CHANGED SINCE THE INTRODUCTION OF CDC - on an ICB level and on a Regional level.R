### QUESTION 2 -  2: HOW HAS DIAGNOSTIC ACTIVITY PER PERSON CHANGED SINCE THE INTRODUCTION OF CDC - on an ICB level and on a Regional level 

## how has DA/ PP changed per person since introduction of CDCs?
    #on the most basic level - group by Period, Commissioner Org Code and Diagnostic Tests, sum Total Activity and Population Total
DM01_ICBPopulationMerge1 <- DM01_ICBPopulationMerge %>%
  group_by(Period, `Commissioner Org Code`, `Diagnostic Tests`, `ICB.2023.Name`, `NSHER.2023.Name`) %>%
  summarize(
    Total_Activity = sum(`Total_Activity`, na.rm = TRUE),
    Population_Total = sum(`Population_Total`, na.rm = TRUE),
    .groups = 'drop'
  )
DM01_ICBPopulationMerge1 <- na.omit(DM01_ICBPopulationMerge1)
View(DM01_ICBPopulationMerge1)

# on most basis level - ICB DA pp over time
DM01_ICBPopulationMerge1a <- subset(DM01_ICBPopulationMerge1, select = -c(`Diagnostic Tests`))
DM01_ICBPopulationMerge1a <- DM01_ICBPopulationMerge1a %>%
  group_by(Period, `Commissioner Org Code`, `ICB.2023.Name`, `NSHER.2023.Name`) %>%
  summarize(
    Total_Activity = sum(`Total_Activity`, na.rm = TRUE),
    Population_Total = sum(`Population_Total`, na.rm = TRUE),
    .groups = 'drop'
  )
    View(DM01_ICBPopulationMerge1a)
    
    # calculate DA/PP 
    DM01_ICBPopulationMerge1a <- DM01_ICBPopulationMerge1a %>%
      mutate(`DA/PP` = `Total_Activity` / `Population_Total`)
    
      #visualisation
    ggplot(DM01_ICBPopulationMerge1a, aes(x = `Period`, y = `DA/PP`, colour = `Commissioner Org Code`)) +
      geom_line() +
      ggtitle("Diagnostic Activity Per Person Pre CDC's")
    
    # create a visulisation less messy than the above - split into separate graphs depending on UK region
          # Split the data frame based on unique values in the NSHER.2023.Name column
          split_dfs <- split(DM01_ICBPopulationMerge1a, DM01_ICBPopulationMerge1a$NSHER.2023.Name)
          # Assign each data frame to a new variable
          North_East_and_Yorkshire <- split_dfs[["North East and Yorkshire"]]
          North_West <- split_dfs[["North West"]]
          Midlands <- split_dfs[["Midlands"]]
          East_of_England <- split_dfs[["East of England"]]
          South_East <- split_dfs[["South East"]]
          South_West <- split_dfs[["South West"]]
          London <- split_dfs[["London"]]
          
        ##renaming problematic ones
        EOE <- split_dfs[["East of England"]]
        NEAY <- split_dfs[["North East and Yorkshire"]]
        NW <- split_dfs[["North West"]]
        SE <- split_dfs[["South East"]]
        SW <- split_dfs[["South West"]]
        
         #visualisations for the above
          ggplot(EOE, aes(x = `Period`, y = `DA/PP`, colour = `Commissioner Org Code`)) +
            geom_line() +
            ggtitle("Diagnostic Activity Per Person Pre CDC's (East of England)")
          #visualisations for the above
          ggplot(NEAY, aes(x = `Period`, y = `DA/PP`, colour = `Commissioner Org Code`)) +
            geom_line() +
            ggtitle("Diagnostic Activity Per Person Pre CDC's (North East and Yorkshire, England)")
          #visualisations for the above
          ggplot(NW, aes(x = `Period`, y = `DA/PP`, colour = `Commissioner Org Code`)) +
            geom_line() +
            ggtitle("Diagnostic Activity Per Person Pre CDC's (North West England")
          #visualisations for the above
          ggplot(SE, aes(x = `Period`, y = `DA/PP`, colour = `Commissioner Org Code`)) +
            geom_line() +
            ggtitle("Diagnostic Activity Per Person Pre CDC's (South East England)")
          #visualisations for the above
          ggplot(SW, aes(x = `Period`, y = `DA/PP`, colour = `Commissioner Org Code`)) +
            geom_line() +
            ggtitle("Diagnostic Activity Per Person Pre CDC's (South West England)")
          #visualisations for the above
          ggplot(London, aes(x = `Period`, y = `DA/PP`, colour = `Commissioner Org Code`)) +
            geom_line() +
            ggtitle("Diagnostic Activity Per Person Pre CDC's (London)")
          #visualisations for the above
          ggplot(Midlands, aes(x = `Period`, y = `DA/PP`, colour = `Commissioner Org Code`)) +
            geom_line() +
            ggtitle("Diagnostic Activity Per Person Pre CDC's (Midlands, England)")
          
      ##grouping into regions 
          # on most basis level - regional (via ICB) DA pp over time
          DM01_ICBPopulationMerge1b <- subset(DM01_ICBPopulationMerge1, select = -c(`Diagnostic Tests`, `ICB.2023.Name`, `Commissioner Org Code`))
          DM01_ICBPopulationMerge1b <- DM01_ICBPopulationMerge1b %>%
            group_by(Period, `NSHER.2023.Name`) %>%
            summarize(
              Total_Activity = sum(`Total_Activity`, na.rm = TRUE),
              Population_Total = sum(`Population_Total`, na.rm = TRUE),
              .groups = 'drop'
            )
          View(DM01_ICBPopulationMerge1b)
          
     # calculate DA/PP 
          DM01_ICBPopulationMerge1b <- DM01_ICBPopulationMerge1b %>%
            mutate(`DA/PP` = `Total_Activity` / `Population_Total`)
          
       #visualisation
          ggplot(DM01_ICBPopulationMerge1b, aes(x = `Period`, y = `DA/PP`, colour = `NSHER.2023.Name`)) +
            geom_line() +
            ggtitle("Diagnostic Activity Per Person Pre CDC's (Regionally)")