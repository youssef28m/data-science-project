library(stringr)

# Function to clean the dataset
data_cleaning <- function(data) {

  # Remove rows with missing values and duplicates
  data <- na.omit(data)
  data <- data[!duplicated(data), ]

  # Trim extra spaces from character columns
  data$items <- trimws(data$items)
  data$customer <- trimws(data$customer)
  data$city <- trimws(data$city)
  data$paymentType <- trimws(data$paymentType)

  # Remove outliers from the `total` column based on IQR
  Q1 <- quantile(data$total, 0.25, na.rm = TRUE)
  Q3 <- quantile(data$total, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower_bound <- Q1 - 1.5 * IQR
  upper_bound <- Q3 + 1.5 * IQR
  data <- data[!(data$total < lower_bound | data$total > upper_bound), ]

  return(data)
}


