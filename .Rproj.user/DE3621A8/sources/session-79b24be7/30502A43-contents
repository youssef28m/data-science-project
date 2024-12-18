library(arules)
library(arulesViz)

# Function to generate association rules using the Apriori algorithm
association_rules <- function(data, min_confidence, min_support) {

  # Convert the items column into a transaction object
  transactions_list <- strsplit(as.character(data$items), ",")
  transactions <- as(transactions_list, "transactions")

  # Generate association rules with the specified parameters
  rules <- apriori(
    transactions,
    parameter = list(supp = min_support, conf = min_confidence, target = "rules", minlen = 2)
  )

  # Sort rules by confidence in descending order
  rules <- rules[order(quality(rules)$confidence, decreasing = TRUE),]

  return(rules)
}

# Function to check if any association rules exist
association_rules_exist <- function(rules) {
  return(length(rules) > 0)
}

# Function to create a graph visualization of the association rules
association_graph <- function(rules) {
  if (association_rules_exist(rules)) {
    inspect(rules) # Display rules in the console
    return(plot(rules, method = "graph", engine = "htmlwidget"))
  }
}

# Function to convert association rules to a data frame
association_Data_frame <- function(rules) {
  if (association_rules_exist(rules)) {
    return(as(rules, "data.frame"))
  }
}
