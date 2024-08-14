Q3flagged3 <- Q3flagged[, c("i.Period", "Provider Org Code", "Is.Operated.By...Code",   
                            
                            
                            
                            "Name", "i.Diagnostic Tests",   
                            
                            
                            
                            "i.00 < 01 Week", "i.01 < 02 Weeks", "i.02 < 03 Weeks",   
                            
                            
                            
                            "i.03 < 04 Weeks", "i.04 < 05 Weeks", "i.05 < 06 Weeks",   
                            
                            
                            
                            "i.06 < 07 Weeks", "i.07 < 08 Weeks", "i.08 < 09 Weeks",   
                            
                            
                            
                            "i.09 < 10 Weeks", "i.10 < 11 Weeks", "i.11 < 12 Weeks",   
                            
                            
                            
                            "i.12 < 13 Weeks", "i.13+ Weeks", "Total Activity", "i.Total Activity", "flag")]  



### question 3 part 3 - on a test level 

Q3flagged3aggTEST <- Q3flagged3 %>%  
  
  
  
  group_by(`i.Diagnostic Tests`, flag) %>%  
  
  
  
  summarise(across(  
    
    
    
    where(is.numeric),  
    
    
    
    ~sum(.x, na.rm = TRUE),  
    
    
    
    .names = "sum_{col}"  
    
    
    
  ))  



# Reshape data from wide to long format  

Q3flagged3aggTEST <- Q3flagged3aggTEST %>%  
  
  
  
  pivot_longer(  
    
    
    
    cols = starts_with("sum_i."),  
    
    
    
    names_to = "Week_Range",  
    
    
    
    values_to = "Value"  
    
    
    
  )  





# Create a new column that calculates ratio of activity accountable to CDC's for each trust 

Q3flagged3aggTEST <- Q3flagged3aggTEST %>% 
  
  mutate(New_Column = ifelse(Week_Range == 'sum_i.Total Activity', 
                             
                             `sum_Total Activity` / Value, 
                             
                             NA_real_)) 

# Assuming your data frame is named Q3flagged3aggTEST 

Q3flagged3aggTEST <- Q3flagged3aggTEST %>% 
  
  # Group by i.Diagnostic Tests and flag columns 
  
  group_by(`i.Diagnostic Tests`, flag) %>% 
  
  # Identify the value from rows with 'sum_i.Total Activity' 
  
  mutate(Replacement_Value = first(New_Column[Week_Range == 'sum_i.Total Activity'], order_by = row_number())) %>% 
  
  # Replace NA values with the identified Replacement_Value 
  
  mutate(New_Column = ifelse(is.na(New_Column), Replacement_Value, New_Column)) %>% 
  
  # Remove the temporary Replacement_Value column 
  
  select(-Replacement_Value) %>% 
  
  # Ungroup to return to a regular data frame 
  
  ungroup() 







# Remove the sum_Total Activity column and rows with sum_i.Total Activity in Week_Range 

Q3flagged3aggTEST <- Q3flagged3aggTEST %>% 
  
  # Remove the sum_Total Activity column 
  
  select(-`sum_Total Activity`) %>% 
  
  # Filter out rows with 'sum_i.Total Activity' in Week_Range 
  
  filter(Week_Range != 'sum_i.Total Activity') 

Q3flagged3aggTEST <- Q3flagged3aggTEST %>% 
  
  rename(CDC_Activity = New_Column) 









# Assuming 'Q3flagged3aggTRUST' is your data frame 

# Direct mapping using recode 

Q3flagged3aggTEST<- Q3flagged3aggTEST %>% 
  
  mutate(Week_Range = recode(Week_Range, 
                             
                             'sum_i.00 < 01 Week' = 1, 
                             
                             'sum_i.01 < 02 Weeks' = 2, 
                             
                             'sum_i.02 < 03 Weeks' = 3, 
                             
                             'sum_i.03 < 04 Weeks' = 4, 
                             
                             'sum_i.04 < 05 Weeks' = 5, 
                             
                             'sum_i.05 < 06 Weeks' = 6, 
                             
                             'sum_i.06 < 07 Weeks' = 7, 
                             
                             'sum_i.07 < 08 Weeks' = 8, 
                             
                             'sum_i.08 < 09 Weeks' = 9, 
                             
                             'sum_i.09 < 10 Weeks' = 10, 
                             
                             'sum_i.10 < 11 Weeks' = 11, 
                             
                             'sum_i.11 < 12 Weeks' = 12, 
                             
                             'sum_i.12 < 13 Weeks' = 13, 
                             
                             'sum_i.13+ Weeks' = 14)) 



# Calculate the weighted average week for each Code 

df_avg_week2 <- Q3flagged3aggTEST %>% 
  
  group_by(`i.Diagnostic Tests`) %>% 
  
  summarise(Average_Week = sum(Week_Range * Value) / sum(Value)) 



# Merge the result back into the original data frame 

Q3flagged3aggTEST <- Q3flagged3aggTEST %>% 
  
  left_join(df_avg_week2, by = "i.Diagnostic Tests") 



#delete NaN values 

Q3flagged3aggTEST <- Q3flagged3aggTEST %>% 
  
  filter(!is.nan(Average_Week)) 



#group by Test and flag 

Q3flagged3aggTEST <- Q3flagged3aggTEST %>% 
  
  group_by(`i.Diagnostic Tests`, flag) %>% 
  
  summarise( 
    
    CDC_Activity = mean(CDC_Activity, na.rm = TRUE), 
    
    Avg_Week = mean(Average_Week, na.rm = TRUE) 
    
  ) 





#visuals 

library(ggplot2) 



# Create the scatter plot with y-axis limited to values below 22 

ggplot(Q3flagged3aggTEST, aes(x = Avg_Week, y = CDC_Activity, color = factor(flag))) + 
  
  geom_point(size = 1) + 
  
  coord_cartesian(ylim = c(0, 0.76)) + 
  
  labs( 
    
    title = "Scatter Plot of Average Waiting Times vs proportion of activity performed by CDCs.", 
    
    x = "Average Waiting Time (Week)", 
    
    y = "CDC Activity", 
    
    color = "Flag" 
    
  ) + 
  
  theme_minimal() 

