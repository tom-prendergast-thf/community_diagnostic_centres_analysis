
### creating histogram for just trust ###
    # Ensure the column is numeric
    DM01_CDC_Trusts_Merged_NoMonthsJustTrust$`Total Activity.y` <- as.numeric(DM01_CDC_Trusts_Merged_NoMonthsJustTrust$`Total Activity.y`)
    
    # Remove rows with missing values in the specified column
    DM01_CDC_Trusts_Merged_NoMonthsJustTrust <- DM01_CDC_Trusts_Merged_NoMonthsJustTrust %>%
      filter(!is.na(`Total Activity.y`))
    
    # Calculate the range of the data
    range_value <- range(DM01_CDC_Trusts_Merged_NoMonthsJustTrust$`Total Activity.y`)
    min_value <- range_value[1]
    max_value <- range_value[2]
    
    # Set the number of bins
    number_of_bins <- 23.63
    
    # Calculate the bin width
    binwidth <- (max_value - min_value) / number_of_bins
    
    # Create the histogram
    ggplot(DM01_CDC_Trusts_Merged_NoMonthsJustTrust, aes(x = `Total Activity.y`)) +
      geom_histogram(binwidth = binwidth, fill = "lightblue", color = "darkblue", boundary = min_value) +  # Use calculated binwidth and boundary
      labs(
        title = "Distribution of Diagnostic Activity Carried Out by Each CDC Trust",
        x = "Total Activity Carried Out By Each Trust",
        y = "Frequency"
      ) +
      theme_minimal() +
      scale_x_continuous(breaks = seq(min_value, max_value, by = binwidth))


### creating histogram for percentages of Total Diagnostic Activity Attributed to CDC Trust Per ICB region ###
  # Load necessary libraries
  library(scales)  # For formatting axis labels
  
  # Ensure the column is numeric
  DM01_CDC_Trusts_Merged_NoMonthsJustTrust$`Percentage_CDC` <- as.numeric(DM01_CDC_Trusts_Merged_NoMonthsJustTrust$`Percentage_CDC`)
  
  # Remove rows with missing values in the specified column
  DM01_CDC_Trusts_Merged_NoMonthsJustTrust <- DM01_CDC_Trusts_Merged_NoMonthsJustTrust %>%
    filter(!is.na(`Percentage_CDC`))
  
  # Calculate the range of the data
  range_value1 <- range(DM01_CDC_Trusts_Merged_NoMonthsJustTrust$`Percentage_CDC`)
  min_value1 <- range_value1[1]
  max_value1 <- range_value1[2]
  
  # Set the number of bins
  number_of_bins1 <- 8.757841
  
  # Calculate the bin width
  binwidth1 <- (max_value1 - min_value1) / number_of_bins1
  
  # Create the histogram
  ggplot(DM01_CDC_Trusts_Merged_NoMonthsJustTrust, aes(x = `Percentage_CDC`)) +
    geom_histogram(binwidth = binwidth1, fill = "lightblue", color = "darkblue", boundary = min_value1) +  # Use calculated binwidth and boundary
    labs(
      title = "Distribution of The Percentage of Total Diagnostic Activity Within an ICB Carried Out by Each CDC Trust",
      x = "Percentage of Activity (%) ",
      y = "Frequency"
    ) +
    theme_minimal() +
    scale_x_continuous(breaks = seq(min_value1, max_value1, by = binwidth1), labels = scales::number_format(accuracy = 1))
