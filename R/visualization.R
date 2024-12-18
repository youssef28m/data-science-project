# Load necessary libraries
library(ggplot2)  # For creating visualizations
library(dplyr)    # For data manipulation
library(scales)   # For formatting scales (e.g., adding commas to numbers)
library(plotly)   # For creating interactive visualizations

# Commented code (For development only)
# Uncomment and edit the following line to load your dataset.
# data <- read.csv("C:/path/to/data.csv")

# Function to summarize total spending by age
age_spending_summray <- function(data) {
  return(
    data %>%
      group_by(age) %>%  # Group the data by the 'age' column
      summarize(total_spending = sum(total, na.rm = TRUE))  # Calculate the total spending for each age, ignoring missing values
  )
}

# Function to visualize total spending by age
age_spending_visualization <- function(age_summray) {

  # Create a bar plot for total spending by age
  plot <- ggplot(age_summray, aes(x = age, y = total_spending)) +
    geom_bar(stat = "identity", fill = "steelblue") +  # Create bars with height corresponding to total spending
    labs(
      title = "Total Spending by Age",  # Main plot title
      x = "Age",                        # Label for the x-axis
      y = "Total Spending"              # Label for the y-axis
    ) +
    scale_x_continuous(breaks = unique(age_summray$age)) +  # Ensure all unique age values are shown on the x-axis
    scale_y_continuous(
      breaks = seq(0, max(age_summray$total_spending), by = 100000),  # Y-axis intervals
      labels = label_comma()  # Format labels with commas for readability
    ) +
    theme(axis.text.x = element_text(size = 7))  # Adjust x-axis text size for better visibility

  return(ggplotly(plot))  # Convert the plot to an interactive Plotly plot
}

# Function to summarize total spending by city
city_spending_summray <- function(data) {
  return(
    data %>%
      group_by(city) %>%  # Group the data by the 'city' column
      summarize(total_spending = sum(total, na.rm = TRUE)) %>%  # Calculate the total spending for each city
      arrange(desc(total_spending))  # Sort the data by total spending in descending order
  )
}

# Function to visualize total spending by city
city_spending_visualization <- function(city_summray) {

  # Create a bar plot for total spending by city
  plot <- ggplot(city_summray, aes(x = reorder(city, -total_spending), y = total_spending, fill = city)) +
    geom_bar(stat = "identity") +  # Create bars with height corresponding to total spending
    labs(
      title = "Total Spending by City",  # Main plot title
      x = "City",                        # Label for the x-axis
      y = "Total Spending"               # Label for the y-axis
    ) +
    theme_minimal() +  # Use a minimal theme for cleaner visuals
    scale_y_continuous(
      breaks = seq(0, max(city_summray$total_spending), by = 500000),  # Y-axis intervals
      labels = label_comma()  # Format labels with commas
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

  return(ggplotly(plot))  # Convert the plot to an interactive Plotly plot
}

# Function to summarize total spending by payment type
pay_type_summray <- function(data) {
  return(
    data %>%
      group_by(paymentType) %>%  # Group the data by the 'paymentType' column
      summarize(total_spending = sum(total, na.rm = TRUE))  # Calculate total spending for each payment type
  )
}

# Function to visualize total spending by payment type
pay_type_visualization <- function(payment_summray) {

  # Create a pie chart for total spending by payment type
  return(
    ggplot(payment_summray, aes(x = "", y = total_spending, fill = paymentType)) +
      geom_bar(stat = "identity", width = 1, color = "white") +  # Create pie chart bars
      coord_polar(theta = "y") +  # Convert bar chart into a pie chart
      labs(title = "Total Spending by Payment Type") +  # Add the main title
      scale_fill_manual(values = c("Cash" = "steelblue", "Credit" = "#5BBC9B")) +  # Customize colors
      theme_void() +  # Remove axis and background
      geom_text(
        aes(label = paste(paymentType, "\n", label_comma()(total_spending))),  # Add labels for slices
        position = position_stack(vjust = 0.5), color = "white", size = 5
      )
  )
}

# Function to visualize the distribution of total spending
spending_distribution_visualization <- function(data) {

  # Create a histogram to visualize the spending distribution
  plot <- ggplot(data, aes(x = total)) +
    geom_histogram(binwidth = 100, fill = "steelblue", color = "black") +  # Define bin width and colors
    labs(
      title = "Distribution of Total Spending",  # Main plot title
      x = "Total Spending",                      # Label for the x-axis
      y = "Count"                                # Label for the y-axis
    ) +
    scale_x_continuous(
      breaks = seq(0, max(data$total), by = 200)  # Set intervals for the x-axis
    ) +
    scale_y_continuous(
      breaks = seq(0, 500, by = 50)  # Set intervals for the y-axis
    )

  return(ggplotly(plot))  # Convert the plot to an interactive Plotly plot
}
