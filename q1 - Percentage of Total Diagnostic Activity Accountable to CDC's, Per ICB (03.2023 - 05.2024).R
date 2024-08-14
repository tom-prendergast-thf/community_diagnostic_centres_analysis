#### Wednesdays attempt ####

### "Percentage of Total Diagnostic Activity Accountable to CDC's, Per ICB (03.2023 - 05.2024) ###

#checking which columns can be grouped

uniquevalues_CDC_CPOC  <- unique(CDC_data$`Commissioner Parent Org Code`)
uniquevalues_CDC_CPOC[order(uniquevalues_CDC_CPOC)]

uniquevalues_DM01_CPOC  <- unique(recent_DM01_data$`Commissioner Parent Org Code`)
uniquevalues_DM01_CPOC[order(uniquevalues_DM01_CPOC)]


uniquevalues_CDC_PON <- unique(CDC_data$`Provider Org Name`)
uniquevalues_CDC_PON[order(uniquevalues_CDC_PON)]

uniquevalues_DM01_PON  <- unique(recent_DM01_data$`Provider Org Name`)
uniquevalues_DM01_PON[order(uniquevalues_DM01_PON)]

#aggregating each df
recent_DM01_data_1 <- select(recent_DM01_data, 'Period', `Diagnostic Tests`, `Commissioner Parent Org Code`, `Total Activity`)
recent_DM01_data_agg <- recent_DM01_data_1 %>% group_by(Period, `Commissioner Parent Org Code`,`Diagnostic Tests`) %>%
  summarise( `Total Activity` = sum( `Total Activity`), 
             .groups = 'drop')
                  recent_DM01_data_agg <- recent_DM01_data_agg %>% arrange(ymd(Period))
                  recent_DM01_data_agg <- na.omit(recent_DM01_data_agg)
                  View(recent_DM01_data_agg)
                  
                  CDC_data1 <- select(CDC_data, 'Period', `Diagnostic Tests`, `Commissioner Parent Org Code`, `Total Activity`)
                  CDC_data_agg <- CDC_data1 %>% group_by(Period, `Commissioner Parent Org Code`,`Diagnostic Tests`) %>%
                    summarise( `Total Activity` = sum( `Total Activity`), 
                               .groups = 'drop')
                  CDC_data_agg <- CDC_data_agg %>% arrange(ymd(Period))
                  CDC_data_agg <- na.omit(CDC_data_agg)
                  View(CDC_data_agg)
    
   ### MERGE
   DM01_CDC_Merged <- merge(recent_DM01_data_agg, CDC_data_agg, by = c("Period", "Commissioner Parent Org Code", "Diagnostic Tests"), all = TRUE)
   view (DM01_CDC_Merged)
   
### COMPUTING % OF ACTIVITY P/M ACCOUNTED FOR BY CDC ###
   
   DM01_CDC_Merged$Percentage_CDC <- (DM01_CDC_Merged$`Total Activity.y`/ DM01_CDC_Merged$`Total Activity.x`) * 100
   View(DM01_CDC_Merged)
   
 ## grouping by CDC (CPOC) and producing visualisation ##
   ## the below produces a table which computes the percentage of total diagnostic activity accountable to CDC's per month and per commisioner ##
  DM01_CDC_Merged_DT_Total <- DM01_CDC_Merged %>%
    filter(`Diagnostic Tests` == "TOTAL")
  View(DM01_CDC_Merged_DT_Total)
    DM01_CDC_Merged_NoMonthsJustICBS <- DM01_CDC_Merged %>%
      select(`Commissioner Parent Org Code`, `Total Activity.x`, `Total Activity.y`)
      DM01_CDC_Merged_NoMonthsJustICBS <- aggregate(cbind(`Total Activity.x`,`Total Activity.y`) ~ `Commissioner Parent Org Code`, DM01_CDC_Merged_NoMonthsJustICBS, FUN=sum)
      DM01_CDC_Merged_NoMonthsJustICBS$Percentage_CDC <- (DM01_CDC_Merged_NoMonthsJustICBS$`Total Activity.y`/ DM01_CDC_Merged_NoMonthsJustICBS$`Total Activity.x`) * 100
      DM01_CDC_Merged_NoMonthsJustICBS <- DM01_CDC_Merged_NoMonthsJustICBS[order(-DM01_CDC_Merged_NoMonthsJustICBS$Percentage_CDC), ]
      View(DM01_CDC_Merged_NoMonthsJustICBS)
     
      ## adding ICB names to table ##
      #first by creating tible from DM01_data containing Commissioner Parent Org Code and Name#
      DM01_CommissionerCodeAndName <- recent_DM01_data %>% 
        select(`Commissioner Parent Org Code`, `Commissioner Parent Name`)
      DM01_CommissionerCodeAndName1 <- DM01_CommissionerCodeAndName %>%
        group_by(across(everything())) %>%
        summarise(count = n(), .groups = 'drop')
      DM01_CommissionerCodeAndName1 <- DM01_CommissionerCodeAndName1 %>%
        drop_na()
      DM01_CommissionerCodeAndName1 <- DM01_CommissionerCodeAndName1 %>%
        select(-count)

      View(DM01_CommissionerCodeAndName1)

      ## linkig tibble with names to main merged CDC % data (at ICB level) ##
      DM01_CDC_Merged_NoMonthsJustICBS <- merge(DM01_CDC_Merged_NoMonthsJustICBS, DM01_CommissionerCodeAndName1, by = c( "Commissioner Parent Org Code"), all = TRUE)
      DM01_CDC_Merged_NoMonthsJustICBS <- DM01_CDC_Merged_NoMonthsJustICBS[order(-DM01_CDC_Merged_NoMonthsJustICBS$Percentage_CDC), ]
      View(DM01_CDC_Merged_NoMonthsJustICBS)
      
      ## create visuals
  
      ggp2 <- ggplot(DM01_CDC_Merged_NoMonthsJustICBS, 
                    aes(x = reorder(`Commissioner Parent Name.x`, Percentage_CDC), 
                        y = Percentage_CDC)) +    
        geom_bar(stat = "identity", fill = "darkblue") +  # Set bars to dark blue
        coord_flip() +  # Flip the coordinates for a horizontal bar graph
        labs(title = "Percentage of Total Diagnostic Activity Accountable to CDC's, Per ICB (03.2023 - 05.2024)", x = "Integrated Care Board", y = "Percentage of Total Diagnostic Activity Accountable To Local CDC (%)") +
        theme_minimal()
      print(ggp2)
