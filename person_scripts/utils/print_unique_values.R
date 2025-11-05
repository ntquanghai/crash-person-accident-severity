cat("\n=== UNIQUE VALUES AND COUNTS PER VARIABLE ===\n\n")

data.set = train_top40

for (colname in names(data.set)) {
  cat("\n•", colname, "→", length(unique(data.set[[colname]])), "unique values\n")
  
  # Create a frequency table
  counts <- sort(table(data.set[[colname]]), decreasing = TRUE)
  
  # Display top 15 values for readability
  if (length(counts) > 15) {
    cat("  (showing top 15 values)\n")
    print(head(counts, 15))
  } else {
    print(counts)
  }
  
  cat("--------------------------------------\n")
}
