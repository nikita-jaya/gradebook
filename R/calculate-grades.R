#' Calculate Grades
#' This function calculates all grades based on the policy file.
#'
#' @param gs A processed data in wide format 
#' @param policy A policy file
#'
#' @return A data frame
#'
#' @importFrom dplyr select
#' 
#' @export

get_grades <- function(gs, policy){
  # convert gs into a matrix with only assignment info
  grades_mat <- gs |>
    #only assignment info
    select(-get_id_cols(gs)) |> 
    #convert lateness into minutes
    mutate_at(vars(ends_with(" - Lateness (H:M:S)")), convert_to_min) |> 
    #convert to matrix
    data.matrix()
  #rownames are SID of student
  rownames(grades_mat) <- gs$SID
  
  # flatten policy file
  policy <- flatten_policy(policy)
  
  #iterate through each policy item
  for (policy_item in policy$categories){
    #compute calculations for each policy file and save into grades matrix
    grades_mat <- get_category_grade(grades_mat, policy_item)
  }
  
  grades <- grades_mat |>
    data.frame()
  names(grades) <- colnames(grades_mat)
  grades$SID <- rownames(grades_mat)
  #add back ID cols
  
  return (grades)
}

get_category_grade <- function(grades_mat, policy_item){
  #get all keys except category and assignments (which are not functions)
  keys <- names(policy_item)[-which(names(policy_item) %in% c("category", "assignments"))]
  #all assignments + their associated cols for this category
  for (key in keys){
    grades_mat <- get(key)(grades_mat, policy_item[[key]], policy_item$category, policy_item$assignments)
  }
  
  ## insert code to re-add new/editted columns from grades_mat into grades_mat
  return (grades_mat)
}



#' Key Functions
#'
#' @description
#' 
#' A collection of functions that access policy_item keys
#'
#' * `score()` computes the percentage score and saves back into raw points col
#' 
#' * `aggregation()` computes score for category
#' 
#' * `lateness()` applies any relevant lateness penalty.
#'
#' * `drops()` drops n lowest/highest assignment scores.
#' 
#' * `aggregation_max_pts()` computes max points for category.
#' 
#'
#' @param grades_mat Matrix with assignments + assosciated cols for that category
#' @param policy_line Policy list item for that key
#' @param category Category name
#' 
#' @return A single aggregated score (a vector of length 1). 
#'
#' @family {Key Functions}
#' 
#' @export
score <- function(grades_mat, policy_line, category, assignments){
  get(policy_line)(grades_mat, assignments)
}

aggregation <- function(grades_mat, policy_line, category, assignments){
  # TBD
}

lateness <- function(grades_mat, policy_line, category, assignments){
  # TBD
}
drops <- function(grades_mat, policy_line, category, assignments){
  # TBD
}

aggregation_max_pts <- function(grades_mat, policy_line, category, assignments){
  #TBD
}


#' Score Functions
#'
#' @description
#' 
#' * `raw_over_max()` computes score by dividing raw points by max points
#' 
#' A collection of functions that calculate score for assignments
raw_over_max <- function(grades_mat, assignments){
  grades_mat[,assignments] <- grades_mat[, assignments] / grades_mat[, paste0(assignments, " - Max Points")]
  return(grades_mat)
}





#' @importFrom lubridate hms period_to_seconds 
convert_to_min <- function(hms){
  save <- lubridate::hms(hms) |>
    lubridate::period_to_seconds()
  save <- save/60
  return (save)
  
  # placeholder for apply_clobbers()
}
