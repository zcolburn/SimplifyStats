#' @title Add rows for unused combinations of factor levels
#' 
#' @description add_missing adds rows for unused combinations of factor levels.
#' 
#' The function takes as input a data.frame or tibble, the column names of 
#' grouping variables, and a named list of default values.
#'
#' @param x A data.frame or tibble.
#' @param group_cols Vector of the names of the grouping columns.
#' @param defaults A named list of default values.
#' 
#' @return A tibble
#' 
#' @importFrom assertthat assert_that
#'
#' @export
#' @return A tibble or data.frame, depending on the class of x.
#' 
#' @examples 
#' iris_sub <- dplyr::filter(iris, Species != "virginica")
#' iris_summary <- dplyr::group_by(iris_sub, Species)
#' iris_summary <- dplyr::summarise(iris_summary, N = dplyr::n())
#' iris_summary <- dplyr::ungroup(iris_summary)
#' add_missing(iris_summary, "Species", list(N = 0))
add_missing <- function(x, group_cols, defaults){
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
  # # Are group_col character variables found in x's colnames?
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
  
  ############################# Main function
  
  # Identify missing combinations of factors.
  factors <- vector(mode = "character", length(group_cols))
  names(factors) <- group_cols
  factors <- as.list(factors)
  for(i in group_cols){
    factors[[i]] <- levels(x[[i]])
  }
  combos <- expand.grid(factors)
  missing_combos <- rep(FALSE, nrow(combos))
  for(i in 1:nrow(combos)){
    truth <- rep(TRUE, nrow(x))
    for(j in 1:ncol(combos)){
      truth <- truth & (x[[group_cols[j]]] == as.character(combos[i,j]))
    }
    if(!any(truth)) missing_combos[i] <- TRUE
  }
  
  # If there are no missing combinations then return x.
  if(!any(missing_combos)) return(x)
  
  # Treat the data differently if it is a data.frame or tibble.
  # In both cases, subset the missing combinations from the combos data.frame.
  x_class <- class(x)
  combos_df <- combos[missing_combos,,drop = FALSE]
  
  # Add default values to the subset combos data.frame.
  for(i in names(defaults)){
    combos_df[[i]] <- defaults[[i]]
  }
  
  # Append the new rows.
  x_out <- dplyr::bind_rows(x, combos_df)
  
  # Ensure that a data.frame is returned if it was the input.
  if((length(x_class) == 1) && (x_class == "data.frame")){
    x_out <- as.data.frame(x_out)
  }
  
  # Return the result.
  return(x_out)
}
