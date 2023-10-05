#' Check Names for Policy File
#'
#' This functions checks the names throughout the policy file
#'
#' @param policy_file A policy file that's validated for having the right format in terms of names and nesting
#'
#' @return No output, only stops and warnings
#' @importFrom purrr map
#' @export
check_names <- function(policy){
    level_one_names <- names(policy)
    
    if (! setequal(level_one_names, c("coursewide", "categories", "cutoff") ) ){
        stop("Incorrect names of nested lists in policy files")
    }
    
    cutoff_grades <- names(policy$cutoff)
    
    if (! setequal(cutoff_grades, c("A", "B", "C", "D", "F") )){
        stop("Incorrect names of nested list, cutoff")
    }
    
    category_names <- purrr::map(policy$categories, names)
    
    are_valid_cat_names <- purrr::map(category_names, check_cat_names) |>
        unlist()
    
    if (sum(!are_valid_cat_names) > 0){
        stop("One of the categories has incorrectly formatted names")
    }
    
}

#' Check Category Names
#'
#' This functions checks the names of a single category in a policy file
#'
#' @param category_name the names within a category in a policy file
#'
#' @return returns logical vector
#' @export
check_cat_names <- function(category_name){
    setequal(category_name, c("name", "slipdays", "late_time1", "late_time2", 
                              "late_scale1", "late_scale2", "after", "weight", 
                              "drops", "weighted_method", "clobber", "assigns"))
}

#' Check Category Names
#'
#' This functions checks that all assignments within categories of policy file exists in the Gradescope data
#'
#' @param policy policy file to validate
#' @param pivot_df pivotted gradescope data
#'
#' @return None
#' @importFrom dplyr filter
#' @export
check_assignment_names <- function(policy, pivot_df){
    policy_assigns <- purrr::map(policy$categories, "assigns") |> unlist()
    
    pivot_assigns <- unique(pivot_df$assignments)
    
    additional_assigns <- as.data.frame(policy_assigns) %>%
        filter(!(policy_assigns %in% pivot_assigns)) 
    
    if (nrow(additional_assigns) > 0){
        stop(paste0("There is no data on the following assignments: ", additional_assigns))
    }
    
}