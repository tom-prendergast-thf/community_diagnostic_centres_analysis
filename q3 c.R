# Create a bar plot to compare the distribution of waiting times for each diagnostic test 

ggplot(Q3flagged2agg_filtered_flag_0, aes(x = Week_Range, y = Value, fill = `i.Diagnostic Tests`)) + 
  
  geom_bar(stat = "identity", position = "dodge") + 
  
  labs(title = "Distribution of Waiting Times For Trusts without an Associated CDC", 
       
       x = "Waiting Time (weeks)", 
       
       y = "Number of People Waiting", 
       
       fill = "Diagnostic Test") + 
  
  theme_minimal() + 
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 



# Create a bar plot to compare the distribution of waiting times for each diagnostic test 

ggplot(Q3flagged2agg_filtered_flag_1, aes(x = Week_Range, y = Value, fill = `i.Diagnostic Tests`)) + 
  
  geom_bar(stat = "identity", position = "dodge") + 
  
  labs(title = "Distribution of Waiting Times for Trusts with a CDC Associated", 
       
       x = "Waiting Time (weeks)", 
       
       y = "Number of People Waiting", 
       
       fill = "Diagnostic Test") + 
  
  theme_minimal() + 
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 



# turning wait times numeric  



# Convert Week_Range to numeric values (midpoint of the week ranges) 

library(dplyr) 



# Direct mapping using recode 

Q3flagged2agg_numeric<- Q3flagged2agg_filtered %>% 
  
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



View(Q3flagged2agg_numeric) 





# Split the data frame into two based on flag values 

Q3flagged2agg_numeric0 <- Q3flagged2agg_numeric %>% 
  
  filter(flag == 0) 



# Create a bar plot to visualize the distribution of waiting times 

ggplot(Q3flagged2agg_numeric0, aes(x = factor(Week_Range), y = Value, fill = `i.Diagnostic Tests`)) + 
  
  geom_bar(stat = "identity", position = "dodge") + 
  
  labs(title = "Distribution of Waiting Times For Trusts without an Associated CDC", 
       
       x = "Number of Weeks Waiting", 
       
       y = "Number of People Waiting", 
       
       fill = "Diagnostic Test") + 
  
  theme_minimal() + 
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 



Q3flagged2agg_numeric1<- Q3flagged2agg_numeric %>% 
  
  filter(flag == 1) 



# Create a bar plot to visualize the distribution of waiting times 

ggplot(Q3flagged2agg_numeric1, aes(x = factor(Week_Range), y = Value, fill = `i.Diagnostic Tests`)) + 
  
  geom_bar(stat = "identity", position = "dodge") + 
  
  labs(title = "Distribution of Waiting Times For Trusts with an Associated CDC", 
       
       x = "Number of Weeks Waiting", 
       
       y = "Number of People Waiting", 
       
       fill = "Diagnostic Test") + 
  
  theme_minimal() + 
  
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 



#create distrubution percentages   

# Create a summary column by grouping and summarizing the data 

Q3flagged2agg_numericSUM <- Q3flagged2agg_numeric %>% 
  
  group_by(`i.Diagnostic Tests`, flag) %>% 
  
  mutate(Summary_Value = sum(Value, na.rm = TRUE)) %>% 
  
  ungroup() 

# Create a new column that divides the Value column by the Summary_Value column 

Q3flagged2agg_numericSUM <- Q3flagged2agg_numericSUM %>% 
  
  mutate(Proportion = (Value / Summary_Value) * 100) 