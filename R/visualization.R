library(ggplot2)  # For visualizations
library(dplyr)    # For data manipulation
library(scales)   # For formatting scales
library(plotly)   # For interactive visualizations

# Function to summarize total spending by age
age_spending_summary <- function(data) {
  data %>%
    group_by(age) %>%
    summarize(total_spending = sum(total, na.rm = TRUE))
}

# Function to visualize total spending by age
age_spending_visualization <- function(age_summary) {
  plot <- ggplot(age_summary, aes(x = age, y = total_spending)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Total Spending by Age", x = "Age", y = "Total Spending") +
    scale_x_continuous(breaks = unique(age_summary$age)) +
    scale_y_continuous(
      breaks = seq(0, max(age_summary$total_spending), by = 100000),
      labels = label_comma()
    ) +
    theme(axis.text.x = element_text(size = 7))
  ggplotly(plot)
}

# Function to summarize total spending by city
city_spending_summary <- function(data) {
  data %>%
    group_by(city) %>%
    summarize(total_spending = sum(total, na.rm = TRUE)) %>%
    arrange(desc(total_spending))
}

# Function to visualize total spending by city
city_spending_visualization <- function(city_summary) {
  plot <- ggplot(city_summary, aes(x = reorder(city, -total_spending), y = total_spending, fill = city)) +
    geom_bar(stat = "identity") +
    labs(title = "Total Spending by City", x = "City", y = "Total Spending") +
    theme_minimal() +
    scale_y_continuous(
      breaks = seq(0, max(city_summary$total_spending), by = 500000),
      labels = label_comma()
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggplotly(plot)
}

# Function to summarize total spending by payment type
pay_type_summary <- function(data) {
  data %>%
    group_by(paymentType) %>%
    summarize(total_spending = sum(total, na.rm = TRUE))
}

# Function to visualize total spending by payment type
pay_type_visualization <- function(payment_summary) {
  ggplot(payment_summary, aes(x = "", y = total_spending, fill = paymentType)) +
    geom_bar(stat = "identity", width = 1, color = "white") +
    coord_polar(theta = "y") +
    labs(title = "Total Spending by Payment Type") +
    scale_fill_manual(values = c("Cash" = "steelblue", "Credit" = "#5BBC9B")) +
    theme_void() +
    geom_text(
      aes(label = paste(paymentType, "\n", label_comma()(total_spending))),
      position = position_stack(vjust = 0.5), color = "white", size = 5
    )
}

# Function to visualize the distribution of total spending
spending_distribution_visualization <- function(data) {
  plot <- ggplot(data, aes(x = total)) +
    geom_histogram(binwidth = 100, fill = "steelblue", color = "black") +
    labs(title = "Distribution of Total Spending", x = "Total Spending", y = "Count") +
    scale_x_continuous(breaks = seq(0, max(data$total), by = 200)) +
    scale_y_continuous(breaks = seq(0, 500, by = 50))
  ggplotly(plot)
}


