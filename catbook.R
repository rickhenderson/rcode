# Catbook.csv Sample Code
# Created by Rick Henderson
# Created on January 20, 2016

# This file serves as sort of a tutorial so that
# you can become familiar with basic R commands using a small 
# dataset that is easy to understand.

# The catbook.csv dataset contains 6 rows and 4 columns.
# Only 1 row contains two values that are set to NA,
# and two other rows have a single NA value.
# This way you can become familiar with exactly how
# complete.cases() works


# Write a function to be called from the R command line to analyze the catbook.csv file.
analyzeCatbook <- function() {
  # Read in the data you want
  catbook <- read.csv("catbook.csv")
  
  # Display the variable names or column headings
  names(catbook)
  
  # Display the results if you want
  catbook
  
  # Create a logical vector of the complete cases
  cleanCatBook <- complete.cases(catbook)
  
  # Display the vector, or you can view it in the R Studio Environment
  cleanCatBook
  
  # We see that it considers ALL rows that contain 1 or more NA values to be a complete case.
}
