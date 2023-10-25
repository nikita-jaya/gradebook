#' Unnest subcategories from a category
#' 
#' Takes a top level category of a policy file (e.g. problem sets) and acts recursively
#' to extract all categories that are nested within and return them as a list of a 
#' single level.
#'
#' @param category An element of a policy file, itself an R list.
#'
#' @return A list of categories (themselves lists of depth 1) beginning the with the
#' most deeply nested and ending with the outer layer of the input category.
#'
#' @examples
#' # Example
#' extract_nested(policy_demo[[1]])
#' @importFrom purrr map map_chr list_flatten
#' @export
extract_nested <- function(category) {
    # If there's no more nesting, return the category as a list
    if (!("assignments" %in% names(category) && is.list(category$assignments))) {
        return(list(category))
    }
    
    # Otherwise, get the nested categories
    nested_categories <- purrr::map(category$assignments, extract_nested) |>
        purrr::list_flatten()
    
    # Modify the current category to contain the nested categories as assignments
    category$assignments <- purrr::map_chr(nested_categories, "category")
    
    # Return the modified current list after nested items
    c(nested_categories, list(category))
}

#' Reshape policy file from nested to flat
#' 
#' Cycles through the top-level categories of a policy file and unnested all
#' subcategories to create a single level list of all categories and subcategories.
#'
#' @param policy A valid policy file stored as a list.
#'
#' @return A single level list of all categories and subcategories ordered where 
#' all leaves will precede the parent category in the list order.
#'
#' @examples
#' # Example
#' flatten_policy(policy_demo)
#' @importFrom purrr map list_flatten
#' @export
flatten_policy <- function(policy) {
    purrr::map(policy, extract_nested) |>
        purrr::list_flatten()
}

