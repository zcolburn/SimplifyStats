#' @title Calculate descriptive statistics for each group
#' 
#' @description group_summarize performs descriptive statistics for each group 
#' in a data set.
#' 
#' The function takes as input a data.frame or tibble, the column names of 
#' grouping variables, and the column names of variables of interest.
#'
#' @param x A data.frame or tibble.
#' @param group_cols Vector of the names of the grouping columns.
#' @param var_cols Vector of the names of the variables of interest.
#' @param ... Extra arguments passed to fxn, i.e. na.rm = FALSE, etc.
#' 
#' @return An object of class group_summary. group_summary is a list of data 
#' frames. The name of each element of the list is the name of the 
#' corresponding variable analyzed.
#' 
#' @aliases group_summarise
#' 
#' @import assertthat broom dplyr moments
#' @importFrom utils combn
#' @importFrom stats median quantile sd shapiro.test
#'
#' @export
#' @return A data.frame
#' @examples \dontrun{
#' pairwise_stats(iris, "Species", c("Sepal.Length","Sepal.Width"))
#' }
group_summarize <- function(x, group_cols, var_cols, ...){
  # Check input variables.
  # # Is x a data.frame of dimensions greater than 0 x 0?
  if(!class(x) %in% c("tbl_df", "data.frame")){
    stop("x must be a data.frame or tbl_df.")
  }
  if((nrow(x) == 0) | (ncol(x) == 0)){
    stop("x must no have a dimension of length 0.")
  }
  # # Are group_col and var_cols character variables found in x's colnames?
  if(!(class(group_cols) == "character")){
    stop("group_cols must be of the character class.")
  }
  assertthat::assert_that(all(sapply(group_cols, nchar) > 0))
  group_cols_in <- group_cols %in% colnames(x)
  if(!all(group_cols_in)){
    stop(paste0(
      paste0(group_cols[!group_cols_in], collapse = " "), 
      " not in x."
    ))
  }
  if(!(class(var_cols) == "character")){
    stop("var_cols must be of the character class.")
  }
  assertthat::assert_that(all(sapply(var_cols, nchar) > 0))
  var_cols_in <- var_cols %in% colnames(x)
  if(!all(var_cols_in)){
    stop(paste0(
      paste0(var_cols[!var_cols_in], collapse = " "), 
      " not in x."
    ))
  }
  
  ############################# Main function
  
  # Create distinct group IDs.
  groups_df <- x[, group_cols, drop = FALSE]
  group_ids <- interaction(groups_df)
  groups_df$.unique_group_id_column <- group_ids
  unique_groups_df <- unique(groups_df)
  
  # Create a group_id to group_cols data.frame.
  groups <- unique_groups_df$.unique_group_id_column
  unique_groups_df <- unique_groups_df[,-which(
    colnames(unique_groups_df) == ".unique_group_id_column")]
  rownames(unique_groups_df) <- groups
  
  # Functions to be applied to each variable of interest.
  fxn_list <- list(
    "N" = function(vals,...){sum(!is.na(vals))},
    "Mean" = mean,
    "StdDev" = sd,
    "StdErr" = function(vals,...){sd(vals,...)/sqrt(sum(!is.na(vals)))},
    "Min" = min,
    "Quartile1" = function(vals,...){
      params <- as.list(match.call())
      do_with_na <- FALSE
      if("na.rm" %in% names(params)){do_with_na <- params$na.rm}
      na_vals <- is.na(vals)
      if(!do_with_na & any(na_vals)){return(NA)}
      quantile(vals[!na_vals], probs = 0.25, ...)
    },
    "Median" = median,
    "Quartile3" = function(vals,...){
      params <- as.list(match.call())
      do_with_na <- FALSE
      if("na.rm" %in% names(params)){do_with_na <- params$na.rm}
      na_vals <- is.na(vals)
      if(!do_with_na & any(na_vals)){return(NA)}
      quantile(vals[!na_vals], probs = 0.75, ...)
    },
    "Max" = max,
    "PropNA" = function(vals,...){sum(is.na(vals))/length(vals)},
    "Kurtosis" = moments::kurtosis,
    "Skewness" = moments::skewness,
    "Jarque-Bera_p.value" = function(vals,...){
      params <- as.list(match.call())
      do_with_na <- FALSE
      if("na.rm" %in% names(params)){do_with_na <- params$na.rm}
      na_vals <- is.na(vals)
      if(!do_with_na & any(na_vals)){return(NA)}
      if(sum(!na_vals) <= 3){return(NA)}
      moments::jarque.test(vals[!na_vals])[["p.value"]]
    },
    "Shapiro-Wilk_p.value" = function(vals,...){
      params <- as.list(match.call())
      do_with_na <- FALSE
      if("na.rm" %in% names(params)){do_with_na <- params$na.rm}
      na_vals <- is.na(vals)
      if(!do_with_na & any(na_vals)){return(NA)}
      num_na <- sum(!na_vals)
      if((num_na <= 3) | num_na >= 5000){
        if(!shapiro_warning){
          shapiro_warning <<- TRUE
          print("The Shapiro-Wilk test requires 3 < N < 5000.")
        }
        return(NA)
      }
      shapiro.test(vals[!na_vals])[["p.value"]]
    }
  )
  
  # Set warnings.
  shapiro_warning <- FALSE
  
  # Perform fxn on each pair of data sets.
  output_stats <- lapply(# For each variable
    split(var_cols, 1:length(var_cols)),
    function(var){
      group_results <- lapply(# Subset each group
        split(groups, 1:length(groups)),
        function(group){
          logical_groups <- group_ids == group
          vals <- x[[var]][logical_groups]
          var_out <- lapply(# and perform the functions in fxn_list
            names(fxn_list),
            function(fxn_name){
              data.frame(fxn_name = fxn_list[[fxn_name]](vals, ...))
            }
          )
          # Rearrange the values and label the output columns.
          var_out <- dplyr::bind_cols(var_out)
          colnames(var_out) <- names(fxn_list)
          var_out
        }
      )
      # Stack the results for each group.
      group_results <- dplyr::bind_rows(group_results)
      
      # Put the group data in front of the group results.
      dplyr::bind_cols(
        unique_groups_df,
        group_results
      )
    }
  )
  # Label each element of the output list with the variable of interest.
  names(output_stats) <- var_cols
  
  class(output_stats) <- "group_summary"
  output_stats
}
