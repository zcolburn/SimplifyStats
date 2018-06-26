#' @title Calculate pairwise statistics between groups
#' 
#' @description pairwise_stats performs a provided function pairwise between all combinations of groups. The first two arguments of the function passed to pairwise_stats must accept vectors of values as inputs. These vectors should correspond to the values for group A and group B, respectively.
#' 
#' The function takes as input a data.frame or tibble, the column name of a categorical variable, the column name for a variable of interest, and a function.
#'
#' @param x A data.frame or tibble.
#' @param factor_col Name of the grouping column
#' @param var_col Name of the variable of interest
#' @param fxn The function to be applied
#' @param two_way Whether the order of data inputs to fxn matter
#' @param ... Extra arguments passed to fxn, i.e. alternative = "greater", etc.
#' 
#' @import assertthat broom dplyr
#' @importFrom utils combn
#'
#' @export
#' @return A data.frame
#' @example pairwise_stats(iris, "Species", "Sepal.Length", t.test)
pairwise_stats <- function(x, factor_col, var_col, fxn, two_way = FALSE, ...){
  # Check input variables.
  # # Is x a data.frame of dimensions greater than 0 x 0?
  assertthat::assert_that(class(x) %in% c("tbl_df", "data.frame"))
  assertthat::assert_that(nrow(x) > 0)
  assertthat::assert_that(ncol(x) > 0)
  # # Are factor_col and var_col character variables found in x's colnames?
  assertthat::assert_that(class(factor_col) == "character")
  assertthat::assert_that(nchar(factor_col) > 0)
  assertthat::assert_that(factor_col %in% colnames(x))
  assertthat::assert_that(class(var_col) == "character")
  assertthat::assert_that(nchar(var_col) > 0)
  assertthat::assert_that(var_col %in% colnames(x))
  # # Is fxn a function?
  assertthat::assert_that(class(fxn) == "function")
  # # Is two_way a logical?
  assertthat::is.flag(two_way)
  
  # If factor_col is not a factor variable then convert it to one.
  if(class(x[[factor_col]]) != "factor"){
    x[[factor_col]] <- factor(x[[factor_col]])
  }
  groups <- unique(x[[factor_col]])
  # # If there are fewer than two groups then exit.
  if(length(groups) == 1){
    stop("factor_col must contain more than one level.")
  }
  
  # Find all combinations of pairs.
  group_pairs <- t(combn(as.character(groups), 2))
  # Produce reverse combinations if specified by two_way.
  if(two_way){
    group_pairs <- rbind(group_pairs, matrix(rev(group_pairs), ncol = 2))
  }
  # Perform fxn on each pair of data sets.
  output_stats <- lapply(
    split(group_pairs, 1:nrow(group_pairs)),
    function(group_elements){
      output_stats <- fxn(
        x[[var_col]][x[[factor_col]] == group_elements[1]],
        x[[var_col]][x[[factor_col]] == group_elements[2]],
        ...
      )
      # Tidy the results.
      broom::tidy(output_stats)
    }
  )
  # Stack the results of each comparison on top of each other.
  output_stats <- dplyr::bind_rows(output_stats)
  
  # Create a data.frame of group pairs listed row-wise.
  group_pairs <- as.data.frame(group_pairs)
  colnames(group_pairs) <- c("Group_A", "Group_B")
  
  # Append the comparisons data.frame and the results column-wise.
  dplyr::bind_cols(
    group_pairs,
    output_stats
  )
}
