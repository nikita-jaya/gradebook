#'Find Weights for Weighted Mean Categories
#'
#'This function will return a policy file where the weights have been extracted from 
#'assignments into the above category featuring aggregation "weighted_mean", where the returned policy file is
#'otherwise identical to the input. 
#'
#'@param policy Un-flattened Policy File
#'@export
#'
find_weights <- function(policy){
  policy$categories[[1]] <- extract_weights(policy$categories[[1]])
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
  if ( category$aggregation == "weighted_mean"){
    
    category$weights <- purrr::map_dbl(category$assignments, 
                                       function(x){
                                         x$weight
                                       })
    #normalize weights
    category$weights <- category$weights / sum(category$weights)
  }
  
  category$assignments <- purrr::map(category$assignments, extract_weights)
  
  return(category)
  
}