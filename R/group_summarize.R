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
#' @param output_format The version of the output format (v0 or v1).
#' @param ... Extra arguments passed to fxn, i.e. na.rm = FALSE, etc.
#' 
#' @return A tibble
#' 
#' @aliases group_summarise
#' 
#' @import broom dplyr moments
#' @importFrom utils combn
#' @importFrom stats median quantile sd shapiro.test
#' @importFrom assertthat assert_that
#'
#' @export
#' @return A group_summary object with slots for the results, grouping 
#' variables, variables of interest, and any other parameters passed in.
#' 
#' @examples 
#' group_summarize(iris, "Species", c("Sepal.Length", "Sepal.Width"))
group_summarize <- function(x, group_cols, var_cols, output_format = "v1", ...){
  # Check input variables.
  # # Is x a data.frame of dimensions greater than 0 x 0?
  if(!(class(x)[1] %in% c("tbl_df", "data.frame"))){
    stop("x must be a data.frame or tbl_df.")
  }
  if((nrow(x) == 0) | (ncol(x) == 0)){
    stop("x must no have a dimension of length 0.")
  }
  if(class(x)[1] == "tbl_df"){
    x <- dplyr::ungroup(x)
  }
  # # Are group_col and var_cols character variables found in x's colnames?
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
  if(!(class(var_cols) == "character")){
    stop("var_cols must be of the character class.")
  }
  assert_that(all(sapply(var_cols, nchar) > 0))
  var_cols_in <- var_cols %in% colnames(x)
  if(!all(var_cols_in)){
    stop(paste0(
      paste0(var_cols[!var_cols_in], collapse = " "), 
      " not in x."
    ))
  }
  # Ensure that output_format is valid.
  if(is.null(output_format)) stop("output_format cannot be NULL.")
  if(
    !(
      is.character(output_format) &
      (length(output_format) == 1) &
      (output_format %in% c("v0", "v1"))
    )
  ){
    stop("output_format must be 'v0' or 'v1'.")
  }
  
  ############################# Main function
  
  # Create distinct group IDs.
  groups_df <- x[, group_cols, drop = FALSE]
  group_ids <- interaction(groups_df)
  groups_df$.unique_group_id_column <- group_ids
  unique_groups_df <- unique(groups_df)
  
  # Create a group_id to group_cols data.frame.
  groups <- unique_groups_df$.unique_group_id_column
  unique_groups_df <- as.data.frame(unique_groups_df)[,-which(
    colnames(unique_groups_df) == ".unique_group_id_column"), drop = FALSE]
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
          cat("\nThe Shapiro-Wilk test requires 3 < N < 5000.\n\n")
        }
        return(NA)
      }
      shapiro.test(vals[!na_vals])[["p.value"]]
    }
  )
  
  # Set warnings.
  shapiro_warning <- FALSE
  
  # Perform fxn on each pair of data sets.
  output <- lapply(# For each variable
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
              tibble::as_tibble(
                data.frame(fxn_name = fxn_list[[fxn_name]](vals, ...))
              )
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
      output <- dplyr::bind_cols(
        unique_groups_df,
        group_results
      )
      
      # Convert to a tibble.
      tibble::as_tibble(output)
    }
  )
  # Label each element of the output list with the variable of interest.
  names(output) <- var_cols
  
  params <- as.list(match.call())
  output <- tibble::lst(
    result = output,
    group_cols = group_cols,
    var_cols = var_cols,
    other_parameters = params[!(
      names(params) %in% c(1:3, "x", "group_cols", "var_cols")
    )]
  )
  
  # Create the appropriate output based on the version number specified by
  # output_format.
  if(output_format == "v0"){
    class(output) <- "group_summary"
    return(output)
  }else if(output_format == "v1"){
    for(var in names(output$result)){
      if(nrow(output$result[[var]]) > 0){
        output$result[[var]] <- dplyr::mutate(
          output$result[[var]],
          Variable = var
        )
      }
    }
    output <- dplyr::bind_rows(output$result)
    if(nrow(output) > 0){
      arranged_cols <- colnames(output)
      arranged_cols <- c("Variable", arranged_cols[arranged_cols != "Variable"])
      output <- output[,arranged_cols]
    }
    return(output)
  }
  
}
