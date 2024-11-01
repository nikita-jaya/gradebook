#' Validate Policy File
#' 
#' Flattens and validates policy file
#' @param policy YAML policy file
#' @param verbose if FALSE, throws error if no assignments found in gs
#' @importFrom purrr map discard
#' @export
process_policy <- function(policy, verbose = FALSE){
  policy <- policy |>
    # insert extract_weights here!
    find_weights() |>
    flatten_policy()
  
  return (policy)
}
#' @importFrom purrr map
find_weights <- function(policy){
  # This function will return a policy file where the weights have been extracted from 
  # assignments into the above category featuring aggregation "weighted_mean", where the returned policy file is
  # otherwise identical to the input. 
  policy$categories <- policy$categories |>
    purrr::map(extract_weights)
  return(policy)
}
#'
#'@importFrom purrr map map_dbl
extract_weights <- function(category){
  # If there's no more nesting, return the category as a list
  if (!("assignments" %in% names(category) && is.list(category$assignments)
  )) {
    # remove the weight from the individual category it is in for cleanliness
    #category$weight <- NULL
    return(category)
  }
  
  #potential problems encoded as warnings in order to allow execution to continue for app
  if ( category$aggregation == "weighted_mean"){
    
    category$weights <- purrr::map_dbl(category$assignments, 
                                       function(x){
                                         weight <- x$weight
                                         if (is.null(weight) ){
                                           weight = 0
                                           warning(paste0(
                                             "Category ", x$category,
                                             " is missing a weight"
                                           ))
                                         }
                                         if(length(weight) > 1){
                                           weight <- weight[[1]]
                                           warning(paste0(
                                             
                                               "Category ", x$category,
                                               " has a weight attribute of length > 1.",
                                               " Weight can only be of length 1.",
                                               " Check for unintended consequences."
                                           ))
                                         }
                                         return(weight)
                                       })
    #normalize weights
    #policy under such conditions can be a subject of discussion
    if (sum(category$weights) == 0){
      category$weights <- rep.int(1/length(category$weights), length(category$weights))
    } else {
      category$weights <- category$weights / sum(category$weights)
    }
    
    
  
  } 
  
  category$assignments <- purrr::map(category$assignments, extract_weights)
  
  return(category)
  
}

#' Reconcile policy file with Gradescope data
#' 
#' This function drops any assignments present in the policy file that are not in the Gradescope data.
#'
#' @param policy A valid policy file stored as a list.
#' @param gs Gradescope data
#' @param verbose if FALSE, throws error if no assignments found in gs
#' @return A policy list
#'
#' @importFrom purrr map list_flatten
#' @export
reconcile_policy_with_gs <- function(policy, gs, verbose = FALSE){
  #set_default uses gs to determine if there is a "score" key for categories made up of gs assignments
  policy <- policy |>
    set_defaults(gs, verbose = verbose)
  prev_length <- 0
  current_length <- length(policy$categories)
  #keep dropping until no more drops necessary
  while (prev_length != current_length){
    prev_length <- length(policy$categories)
    categories <- map(policy$categories, "category") |> unlist()
    assignments <- c(get_assignments(gs), categories)
    policy$categories <- map(policy$categories, function(cat){
      # if (cat$aggregation == "weighted_mean"){
      #   #drop the respective weight(s) if category not found
      #   cat$weights <- cat$weights[cat$assignments %in% assignments]
      # }
      # drop categories with unavailable assignments/nested categories
      remaining_assignments <- cat$assignments %in% assignments
      cat$assignments <- cat$assignments[remaining_assignments]
      cat$weights <- cat$weights[remaining_assignments]
      if (length(cat$assignments) == 0){
        #if category has no assignments, drop
        return (NULL)
      }
      return (cat)
    }) |>
      purrr::discard(is.null)
    current_length <- length(policy$categories)
    
  }
  
  if (length(policy$categories) == 0){
    if (verbose){
      return (NULL)
    } 
    stop("None of the assignments in policy file are found in gs.")
  }
  
  #check if lateness is attempted to be used with Canvas data
  if ((attr(gs, "source") == "Canvas") && 
      (any(purrr::map_lgl(policy$categories, 
                        function(x){
                          "lateness" %in% names(x)
                        })))){
    stop("Canvas data does not allow for lateness calculations.")
  }
  
  policy
}

set_defaults <- function(policy, gs, verbose = FALSE){
  default_cat <- list(
    aggregation = "equally_weighted",
    aggregation_max_pts = "sum_max_pts",
    aggregation_lateness = "max_lateness"
  ) 
  
  # add default values if missing
  policy$categories <- map(policy$categories, function(cat){
    #if min_score/max_score aggregation
    if ("aggregation" %in% names(cat) & cat[["aggregation"]] %in% c("min_score", "max_score")){
      #default for max pts aggregation is "mean_max_pts"
      default_cat[["aggregation_max_pts"]] = "mean_max_pts"
    } else {
      default_cat[["aggregation_max_pts"]] = "sum_max_pts"
    }
    
    #merge default_cat to category
    for (default_name in names(default_cat)){
      if (!(default_name %in% names(cat))){
        default <- list(default_cat[[default_name]])
        names(default) <- default_name
        cat <- append(cat, default)
      }
    }
    #if all assignments are in gs (i.e. there are no nested categories)
    if (!("score" %in% names(cat)) & sum(cat[["assignments"]] %in% get_assignments(gs)) != 0){
      #default score is raw_over_max
      score <- list(
        category = cat[["category"]],
        score = "raw_over_max")
      cat[["category"]] <- NULL #to make sure "category" is still first item
      cat <- append(score, cat)
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
#' flatten_policy(policy_demo)
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

#' 
#' find_weights <- function(policy){
#'   # This function will return a policy file where the weights have been extracted from 
#'   # assignments into the above category featuring aggregation "weighted_mean", where the returned policy file is
#'   # otherwise identical to the input. 
#'   policy$categories[[1]] <- extract_weights(policy$categories[[1]])
#'   return(policy)
#' }
#' #'
#' #'@importFrom purrr map map_dbl
#' extract_weights <- function(category){
#'   # If there's no more nesting, return the category as a list
#'   if (!("assignments" %in% names(category) && is.list(category$assignments)
#'   )) {
#'     # remove the weight from the individual category it is in for cleanliness
#'     #category$weight <- NULL
#'     return(category)
#'   }
#'   if ( category$aggregation == "weighted_mean"){
#'     
#'     category$weights <- purrr::map_dbl(category$assignments, 
#'                                        function(x){
#'                                          x$weight
#'                                        })
#'     #normalize weights
#'     category$weights <- category$weights / sum(category$weights)
#'   }
#'   
#'   category$assignments <- purrr::map(category$assignments, extract_weights)
#'   
#'   return(category)
#'   
#' }