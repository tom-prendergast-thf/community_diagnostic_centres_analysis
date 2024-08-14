### Charts reccomended by Tom
# 1) Layered Histogram of Wait Time Distrubtion for CDC's vs Non-CDC's.
    # Prepare data: Group, reshape, and aggregate
    Q3flagged3aggNATIONAL <- Q3flagged3 %>%
      group_by(`Is.Operated.By...Code`, flag) %>%
      summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE), .names = "sum_{col}"), .groups = 'drop') %>%
      pivot_longer(cols = starts_with("sum_i."), names_to = "Week_Range", values_to = "Value") %>%
      group_by(`Is.Operated.By...Code`) %>%
      mutate(New_Column = ifelse(Week_Range == 'sum_i.Total Activity', Value, NA)) %>%
      fill(New_Column, .direction = "down") %>%
      rename(`CDC Activity` = `sum_Total Activity`, `Total Activity` = New_Column) %>%
      select(-`Is.Operated.By...Code`) %>%
      group_by(flag, Week_Range) %>%
      summarise(
        Value = sum(Value, na.rm = TRUE),
        `CDC Activity` = mean(`CDC Activity`, na.rm = TRUE),
        `Total Activity` = mean(`Total Activity`, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      filter(Week_Range != 'sum_i.Total Activity') %>%
      group_by(flag) %>%
      mutate(
        TOTAL_WL = sum(Value, na.rm = TRUE),
        Percentage = (Value / TOTAL_WL) * 100
      ) %>%
      ungroup() %>%
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
                                 'sum_i.13+ Weeks' = 14)) %>%
      mutate(Week_Range_Binned = cut(Week_Range, 
                                     breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15), 
                                     labels = c('0-1', '1-2', '2-3', '3-4', '4-5', '5-6', '6-7', '7-8', '8-9', '9-10', '10-11', '11-12', '12-13', '13-14', '14+'),
                                     right = FALSE)) %>%
      mutate(flag = factor(flag, levels = c(0, 1), labels = c("Trusts Without An Associated CDC", "Trusts With An Associated CDC")))
    
    # Plot
    ggplot(Q3flagged3aggNATIONAL, aes(x = Week_Range_Binned, y = Percentage, fill = flag)) +
      geom_bar(stat = "identity", position = "identity", alpha = 0.5) +
      scale_fill_manual(values = c("royalblue4", "firebrick2")) +
      labs(
        x = "Week Range", 
        y = "Distribution of Values", 
        fill = "Trust Type"
      ) +
      ggtitle("Layered Histogram To Compare Aggregated Wait Time For Trusts With And Without A CDC") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
      )

# 2)  'Scatter plot of waiting times vs CDC activity for CDC Trusts Only
    Q4b <- Q3flagged3aggTRUST[Q3flagged3aggTRUST$flag != 0, ]
    # Create the scatter plot with y-axis limited to values below 22 
    ggplot(Q4b, aes(x = Avg_Week, y = CDC_Activity)) + 
      geom_point(size = 1.5, color = "darkblue") + 
      coord_cartesian(ylim = c(0, 0.76)) + 
      labs( 
        title = "The Relationship Between Average Wait Times Within A Trust, And The Proportion of The Trust's Diagnostic Activity Accountable to CDC's.", 
        x = "Average Waiting Time (Week)", 
        y = "Fraction of Diagnostic Activity Accountable to CDC's"
      ) + 
      theme_minimal()

# 3) Time Series of Total DM01 Activity Over Time Since Earliest Data Avaliable, With Introduction of CDC's Marked