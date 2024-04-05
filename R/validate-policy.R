#' Validate Policy File
#' 
#' Flattens and validates policy file
#'  @param policy YAML policy file
#'  @param gs Gradescope data
#'  @importFrom purrr map discard
#'  @export
validate_policy <- function(policy, gs){
  policy <- flatten_policy(policy)
  # drop categories with unavailable assignments
  categories <- map(policy$categories, "category") |> unlist()
  # assignments is a vector that also includes category names
  assignments <- c(get_assignments(gs), categories)
  # this is because some categories have only nested categories as their assignments
  # these categories should not be dropped
  policy$categories <- map(policy$categories, function(cat){
    cat$assignments <- cat$assignments[cat$assignments %in% assignments]
    if (length(cat$assignments) == 0){
      return (NULL)
    }
    return (cat)
  }) |>
    purrr::discard(is.null)
  
  default_cat <- list(
    aggregation = "equally_weighted",
    aggregation_max_pts = "sum_max_pts",
    aggregation_lateness = "max_lateness"
  ) 
  
  # add default values if missing
  policy$categories <- map(policy$categories, function(cat){
    for (default_name in names(default_cat)){
      if (!(default_name %in% names(cat))){
        default <- list(default_cat[[default_name]])
        names(default) <- default_name
        cat <- append(cat, default)
      }
    }
    return (cat)
  })

  return (policy)
}

#' Reshape policy file from nested to flat
#' 
#' First propagates `lateness` to all child categories then cycles through the
#' top-level categories of a policy file and unnests all subcategories to create
#' a single level list of all categories and subcategories.
#'
#' @param policy A valid policy file stored as a list.
#'
#' @return A single level list of all categories and subcategories ordered where 
#' all leaves will precede the parent category in the list order.
#'
#' @examples
#' # Example
#' flatten_policy(simple_policy)
#' @importFrom purrr map list_flatten
#' @export
flatten_policy <- function(policy) {
    policy$categories <- policy$categories |>
        purrr::map(extract_nested) |> 
        purrr::list_flatten()
    
    return(policy)
}

#' @importFrom purrr map list_flatten
extract_nested <- function(category) {
  
  # If there's no more nesting, return the category as a list
  if (!("assignments" %in% names(category) && is.list(category$assignments)
  )) {
    return(list(category))
  }
  
  
  # Otherwise, get nested categories
  nested_categories <- purrr::map(category$assignments, extract_nested)
  
  # Flatten the nested categories
  nested_categories_flattened <- list()
  nested_categories_flattened <- c(nested_categories_flattened, purrr::list_flatten(nested_categories))
  
  # Modify the current category's assignments
  category$assignments <- sapply(category$assignments, function(x) x$category)
  
  # Return the flattened nested categories followed by the current category
  c(nested_categories_flattened, list(category))
}