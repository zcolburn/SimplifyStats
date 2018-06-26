#' @title Calculate pairwise statistics between groups
#' 
#' @description pairwise_stats performs a provided function pairwise between 
#' all combinations of groups. The first two arguments of the function passed 
#' to pairwise_stats must accept vectors of values as inputs. These vectors 
#' should correspond to the values for group A and group B, respectively.
#' 
#' The function takes as input a data.frame or tibble, the column names of 
#' grouping variables, the column name for a variable of interest, and a 
#' function.
#'
#' @param x A data.frame or tibble.
#' @param group_cols Vector of the names of the grouping columns
#' @param var_col Name of the variable of interest
#' @param fxn The function to be applied
#' @param two_way Whether the order of data inputs to fxn matter
#' @param ... Extra arguments passed to fxn, i.e. alternative = "greater", etc.
#' 
#' @return An object of class pairwise_stats.
#' 
#' @import broom dplyr tibble
#' @importFrom utils combn
#' @importFrom assertthat assert_that is.flag
#'
#' @export
#' @return A pairwise_stats object with slots for the results, grouping 
#' variables, variable of interest, and any other parameters passed in, 
#' excluding the input data frame.
#' 
#' @examples pairwise_stats(iris, "Species", "Sepal.Length", t.test)
pairwise_stats <- function(x, group_cols, var_col, fxn, two_way = FALSE, ...){
  # Check input variables.
  # # Is x a data.frame of dimensions greater than 0 x 0?
  if(!class(x) %in% c("tbl_df", "data.frame")){
    stop("x must be a data.frame or tbl_df.")
  }
  if((nrow(x) == 0) | (ncol(x) == 0)){
    stop("x must no have a dimension of length 0.")
  }
  # # Are group_col and var_col character variables found in x's colnames?
  if(!(class(group_cols) == "character")){
    stop("group_cols must be of the character class.")
  }
  assert_that(all(sapply(group_cols, nchar) > 0))
  group_cols_in <- group_cols %in% colnames(x)
  if(!all(group_cols_in)){
    stop(paste0(
      paste0(group_cols[!group_cols_in], collapse = " "), 
      " not in x."
    ))
  }
  if(!(class(var_col) == "character")){
    stop("var_col must be of the character class.")
  }
  if(length(var_col) > 1){
    stop("var_col must be of length 1.")
  }
  assert_that(nchar(var_col) > 0)
  if(!(var_col %in% colnames(x))){
    stop(paste0(
      var_col, 
      " is not in x."
    ))
  }
  # # Is fxn a function?
  if(!(class(fxn) == "function")){
    stop("fxn must be a function.")
  }
  # # Is two_way a logical?
  is.flag(two_way)
  
  ############################# Main function
  
  # Create distinct group IDs.
  groups_df <- x[, group_cols, drop = FALSE]
  group_ids <- interaction(groups_df)
  groups <- unique(group_ids)
  # # If there are fewer than two groups then exit.
  if(length(groups) == 1){
    stop("group_col must contain more than one level.")
  }
  # Create a group_id to group_cols data.frame.
  unique_groups_df <- unique(groups_df)
  unique_group_ids <- interaction(unique_groups_df)
  rownames(unique_groups_df) <- unique_group_ids
  
  # Find all combinations of pairs.
  group_pairs <- t(combn(as.character(groups), 2))
  # Produce reverse combinations if specified by two_way.
  if(two_way){
    group_pairs <- rbind(group_pairs, matrix(rev(group_pairs), ncol = 2))
  }
  
  # Create an output groups data.frame to show comparisons for the final output.
  prefix <- c("A.", "B.")
  colnames_out <- colnames(unique_groups_df)
  colnames_out <- paste0(
    rep(prefix, each = length(group_cols)), 
    sep = colnames_out
  )
  groups_out <- dplyr::bind_cols(
    unique_groups_df[group_pairs[,1],,drop = FALSE],
    unique_groups_df[group_pairs[,2],,drop = FALSE]
  )
  colnames(groups_out) <- colnames_out
  
  # Perform fxn on each pair of data sets.
  output_stats <- lapply(
    split(group_pairs, 1:nrow(group_pairs)),
    function(group_elements){
      output_stats <- fxn(
        x[[var_col]][group_ids == group_elements[1]],
        x[[var_col]][group_ids == group_elements[2]],
        ...
      )
      # Tidy the results.
      broom::tidy(output_stats)
    }
  )
  # Stack the results of each comparison on top of each other.
  output_stats <- dplyr::bind_rows(output_stats)
  
  # Append the comparisons data.frame and the results column-wise.
  output <- dplyr::bind_cols(
    groups_out,
    output_stats
  )
  
  # Convert to a tibble.
  output <- tibble::as_tibble(output)
  
  params <- as.list(match.call())
  output <- tibble::lst(
    result = output,
    group_cols = group_cols,
    var_col = var_col,
    two_way = two_way,
    fxn = fxn,
    other_parameters = params[!(
      names(params) %in% c(1:5, "x", "group_cols", "var_col", "fxn", "two_way")
    )]
  )
  
  class(output) <- "pairwise_stats"
  
  
  output
}


#' Print a pairwise_stats object
#'
#' @param x An object of class pairwise_stats.
#' @param ... Additional parameters passed to print.
#'
#' @export
#'
#' @examples \dontrun{
#' print(pairwise_stats(iris, "Species", "Sepal.Length", t.test))
#' }
print.pairwise_stats <- function(x, ...){
  cat("Pairwise comparisons were performed on:\n")
  if(length(x$group_cols) > 1){plural <- "s"}else{plural <- ""}
  cat("  Grouping variable", plural, ": ", trimws(paste(x$group_cols)), "\n")
  cat("  Variable of interest: ", x$var_col, "\n\n")
  print(x$result)
}
