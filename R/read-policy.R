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
  purrr::walk(policy$categories, function(cat){
    if (!("category" %in% names(cat) & "assignments" %in% names(cat))){
      stop(paste0("Not all categories have a category and assignments argument"))
    }
  })
  policy
}