context("group_summarize")

test_that(
  "invalid inputs produce an error",
  {
    expect_error({
      input <- data.frame(
        "x" = c(0),
        "y" = c(0)
      )
      input <- input[-1,]
      group_summarize(input, "Species", "Sepal.Length")
    })
    
    expect_error({
      group_summarize(iris, "Specie", "Sepal.Length")
    })
    
    expect_error({
      group_summarize(iris, "Species", "Sepal")
    })
  }
)

test_that(
  "valid input produce valid output",
  {
    # Generate data.
    set.seed(8)
    test <- iris
    test$Sepal.Length[1] <- NA
    test$letter <- sample(letters, nrow(test), replace = TRUE)
    
    expect_true({
      computation <- group_summarize(
        test, 
        group_cols = c("Species","letter"), 
        var_cols = c("Sepal.Length","Sepal.Width"), 
        na.rm = TRUE
      )
      computation$result$Sepal.Length$Mean[1] == 4.4
    })
  }
)
