### Percentage of Total Diagnostic Activity Accountable to CDCS, per ICB, per/ over each month (03.2023 - 05.2024) ###

##using DM01_CDC_Merged_DT_Total

#link icb names#
DM01_CDC_Merged_MONTHSandICBS <- merge(DM01_CDC_Merged_DT_Total, DM01_CommissionerCodeAndName1, by = c( "Commissioner Parent Org Code"), all = TRUE)
DM01_CDC_Merged_MONTHSandICBS <- DM01_CDC_Merged_MONTHSandICBS[order(-DM01_CDC_Merged_MONTHSandICBS$`Commissioner Parent Name`), ]
View(DM01_CDC_Merged_MONTHSandICBS)

# Calculate the number of rows
n <- nrow(DM01_CDC_Merged_MONTHSandICBS)

# Create a factor that splits the data frame into 4 groups
groups <- cut(1:n, breaks = 4, labels = FALSE)

# Split the data frame into 4 parts based on the groups
DM01_CDC_Merged_MONTHSandICBS_SPLIT <- split(DM01_CDC_Merged_MONTHSandICBS, groups)


# Access individual parts
DM01_CDC_Merged_MONTHSandICBS_SPLIT1 <- DM01_CDC_Merged_MONTHSandICBS_SPLIT[[1]]
DM01_CDC_Merged_MONTHSandICBS_SPLIT2 <- DM01_CDC_Merged_MONTHSandICBS_SPLIT[[2]]
DM01_CDC_Merged_MONTHSandICBS_SPLIT3 <- DM01_CDC_Merged_MONTHSandICBS_SPLIT[[3]]
DM01_CDC_Merged_MONTHSandICBS_SPLIT4 <- DM01_CDC_Merged_MONTHSandICBS_SPLIT[[4]]

#create visualisation 1
ggplot(DM01_CDC_Merged_MONTHSandICBS_SPLIT1, aes(x = `Period`, y = `Percentage_CDC`, colour = `Commissioner Parent Name`)) +
  geom_line() +
  ggtitle("The Percentage of Total NHSE Diagnostic Activity Accountable to CDC's Per Month, Per ICB (1)") +
  scale_x_date(name = "Period", date_breaks = "1 month", date_labels = "%b %Y") +  # Change frequency of x-axis date labels to month and year +
  scale_y_continuous(name = "Percentage of Diagnostic Activity Accountable to CDC", breaks = seq(0, 70, by = 1))  # Change frequency of y-axis labels

#create visualisation 2
ggplot(DM01_CDC_Merged_MONTHSandICBS_SPLIT2, aes(x = `Period`, y = `Percentage_CDC`, colour = `Commissioner Parent Name`)) +
  geom_line() +
  ggtitle("The Percentage of Total NHSE Diagnostic Activity Accountable to CDC's Per Month, Per ICB (2) ") +
  scale_x_date(name = "Period", date_breaks = "1 month", date_labels = "%b %Y") +  # Change frequency of x-axis date labels to month and year +
  scale_y_continuous(name = "Percentage of Diagnostic Activity Accountable to CDC", breaks = seq(0, 70, by = 1))  # Change frequency of y-axis labels

#create visualisation 3
ggplot(DM01_CDC_Merged_MONTHSandICBS_SPLIT3, aes(x = `Period`, y = `Percentage_CDC`, colour = `Commissioner Parent Name`)) +
  geom_line() +
  ggtitle("The Percentage of Total NHSE Diagnostic Activity Accountable to CDC's Per Month, Per ICB (3) ") +
  scale_x_date(name = "Period", date_breaks = "1 month", date_labels = "%b %Y") +  # Change frequency of x-axis date labels to month and year +
  scale_y_continuous(name = "Percentage of Diagnostic Activity Accountable to CDC", breaks = seq(0, 70, by = 1))  # Change frequency of y-axis labels

#create visualisation 4
ggplot(DM01_CDC_Merged_MONTHSandICBS_SPLIT4, aes(x = `Period`, y = `Percentage_CDC`, colour = `Commissioner Parent Name`)) +
  geom_line() +
  ggtitle("The Percentage of Total NHSE Diagnostic Activity Accountable to CDC's Per Month, Per ICB (4) ") +
  scale_x_date(name = "Period", date_breaks = "1 month", date_labels = "%b %Y") +  # Change frequency of x-axis date labels to month and year +
  scale_y_continuous(name = "Percentage of Diagnostic Activity Accountable to CDC", breaks = seq(0, 70, by = 1))  # Change frequency of y-axis labels