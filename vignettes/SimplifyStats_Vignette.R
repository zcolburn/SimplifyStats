## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(SimplifyStats)

# Generate data.
df <- iris

# Modify df to demonstrate additional functionality.
## Add an NA.
df$Sepal.Length[1] <- NA
## Add another grouping variable.
df$Condition <- rep(c("untreated","treated"), 75)

# Generate descriptive statistics.
group_summarize(
  df, 
  group_cols = c("Species","Condition"), 
  var_cols = c("Sepal.Length","Sepal.Width"), 
  na.rm = TRUE
)

## ------------------------------------------------------------------------
# Generate descriptive statistics.
pairwise_stats(
  df, 
  group_cols = c("Species","Condition"), 
  var_cols = c("Sepal.Length", "Sepal.Width"),
  t.test
)

