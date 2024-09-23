#' Read Grading Policy .yml
#'
#' This functions reads the grading policy YAML file, checks for correct format,
#'  converts NA values to zeros and computes scores for each assignment.
#'  
#'
#' @param path Path to policy file
#' @param verbose whether or not to print messages
#'
#' @return R list
#' @importFrom yaml read_yaml
#' @export
read_policy <- function(path, verbose = FALSE){
  # read in yaml
  policy <- yaml::read_yaml(path) |>
    check_keys(verbose = verbose)
  policy
}

check_keys <- function(policy, verbose = FALSE){
  #needs to be written
  flat_policy <- flatten_policy(policy)
  #check that all categories have the "category" and "assignments" key
  purrr::walk(policy$categories, function(cat){
    if (!("category" %in% names(cat) & "assignments" %in% names(cat))){
      stop(paste0("Not all categories have a category and assignments argument"))
    }
  })
  
  #check that all keys are defined in package
  purrr::walk(policy$categories, function(cat){
    valid_keys <- c("category","score", "aggregation", "lateness", 
                    "drop_n_lowest","weight", "weights", "aggregation_max_pts", 
                    "aggregation_lateness", "assignments")
    if ("weights" %in% names(cat)){
      warning(paste0("The weights argument in Category ", cat$category, " may be overwritten.",
                     " THIS COULD RESULT IN UNINTENDED GRADES!"))
    }
    if (sum(names(cat) %in% valid_keys) != length(names(cat))){
      stop(paste0("Category ", cat$category, " has incorrect key(s) ",
                  paste(names(cat)[-(names(cat) %in% valid_keys)], 
                        collapse = ", ")))
    }
  })
  
  policy
}
