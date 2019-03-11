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
#' @param group_cols Vector of the names of the grouping columns.
#' @param var_cols Vector of the names of the variables of interest.
#' @param fxn The function to be applied.
#' @param two_way Whether the order of data inputs to fxn matter.
#' @param output_format The version of the output format (v0 or v1).
#' @param ... Extra arguments passed to fxn, i.e. alternative = "greater", etc.
#' 
#' @return A tibble.
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
#' @examples 
#' pairwise_stats(iris, "Species", "Sepal.Length", t.test)
pairwise_stats <- function(
  x, group_cols, var_cols, fxn, two_way = FALSE, output_format = "v1", ...
){
  # Check input variables.
  # # Is x a data.frame of dimensions greater than 0 x 0?
  if(!(class(x)[1] %in% c("tbl_df", "data.frame"))){
    stop("x must be a data.frame or tbl_df.")
  }
  if((nrow(x) == 0) | (ncol(x) == 0)){
    stop("x must not have a dimension of length 0.")
  }
  if(class(x)[1] == "tbl_df"){
    x <- dplyr::ungroup(x)
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
  if(!(class(var_cols) == "character")){
    stop("var_col must be of the character class.")
  }
  if(length(var_cols) == 0){
    stop("var_cols must be of length greater than 0.")
  }
  assert_that(all(sapply(var_cols, nchar) > 0))
  if(!all(var_cols %in% colnames(x))){
    stop("Not all var_cols are in x.")
  }
  # # Is fxn a function?
  if(!(class(fxn) == "function")){
    stop("fxn must be a function.")
  }
  # # Is two_way a logical?
  is.flag(two_way)
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
  
  # If output_format is specified as v0, then ensure that var_cols is of length 
  # 1.
  if(output_format == "v0"){
    if(length(var_cols) != 1){
      stop("When output_format is set to v0, the length of var_cols must be 1.")
    }
  }
  
  ############################# Main function
  output <- lapply(
    var_cols,
    function(var_col){
      # Create distinct group IDs.
      groups_df <- x[, group_cols, drop = FALSE]
      group_ids <- interaction(groups_df)
      groups <- unique(group_ids)
      # # If there are fewer than two groups then exit.
      if(length(groups) == 1){
        stop("group_col must contain more than one level.")
      }
      # Create a group_id to group_cols data.frame.
      unique_groups_df <- as.data.frame(unique(groups_df))
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
          try(
            {
              output_stats <- fxn(
                x[[var_col]][group_ids == group_elements[1]],
                x[[var_col]][group_ids == group_elements[2]],
                ...
              )
              # Tidy the results.
              output_stats <- broom::tidy(output_stats)
              return(output_stats)
            },
            silent = TRUE
          )
          return(NULL)
        }
      )
      
      # If all tests failed then return an error. Otherwise, set each row of output 
      # for failed tests to NA.
      nulls <- sapply(output_stats, is.null)
      if(any(nulls)){
        if(any(!nulls)){
          template <- output_stats[[which(!nulls)[1]]]
          for(i in 1:ncol(template)){
            template[,i] <- NA
          }
          for(i in which(nulls)){
            output_stats[[i]] <- template
          }
        }else{
          stop("All tests failed!")
        }
      }
      
      # Stack the results of each comparison on top of each other.
      output_stats <- dplyr::bind_rows(output_stats)
      
      # Append the comparisons data.frame and the results column-wise.
      output <- dplyr::bind_cols(
        groups_out,
        output_stats
      )
      
      # Convert to a tibble.
      output <- tibble::as_tibble(output)
      
      # Insert variable name.
      output <- dplyr::mutate(output, Variable = var_col)
      arranged_cols <- colnames(output)
      arranged_cols <- c("Variable", arranged_cols[arranged_cols != "Variable"])
      output <- output[,arranged_cols]
      
      params <- as.list(match.call())
      output <- tibble::lst(
        result = output,
        group_cols = group_cols,
        var_col = var_col,
        two_way = two_way,
        fxn = fxn,
        other_parameters = params[!(
          names(params) %in% c(
            1:5, "x", "group_cols", "var_col", "fxn", "two_way"
          )
        )]
      )
      
      class(output) <- "pairwise_stats"
      
      return(output)
    }
  )
  
  # Create the appropriate output based on the version number specified by
  # output_format.
  if(output_format == "v0"){
    output <- output[[1]]
    return(output)
  }else if(output_format == "v1"){
    output <- lapply(output, function(i){return(i$result)})
    output <- dplyr::bind_rows(output)
    return(output)
  }
  
}
