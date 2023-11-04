#' Aggregate assignment scores
#'
#' @description
#' 
#' A collection of functions to aggregate assignment scores. Their s
#'
#' * `equally_weighted()` computes a mean with the option to drop lowest scores.
#' 
#' * `weighted_by_points()` computes a weighted mean using a weights vector after
#' optionally dropping the lowest scores.
#'
#' * `max_score()` computes the max score.
#' 
#' * `min_score()` computes the min score after optionally dropping lowest scores.
#' 
#' * `none()` if there is only 1 score, returns it, otherwise throws an error. 
#' Serves as a passthrough aggregation for categories with only 1 assignment.
#'
#' @param scores A vector of assignment scores.
#' @param weights A vector of weights of the same length as `scores`. Weights
#' that do not sum to one are ok - they get normalized.
#' @param n_drops The number of lowest scores to drop before aggregating.
#' 
#' @return A single aggregated score (a vector of length 1). 
#'
#' @family {Aggregation functions}
#' @examples
#' my_scores <- c(.7, .9, .1)
#' my_weights <- c(15, 10, 10)
#' 
#' equally_weighted(scores = my_scores, n_drops = 1)
#' weighted_by_points(scores = my_scores, weights = my_weights, n_drops = 1)
#' max_score(my_scores)
#' min_score(my_scores, n_drops = 1)
#' my_score <- c(.7)
#' none(my_score)
#' 
#' @export
equally_weighted <- function(scores, n_drops = 0, ...) {
    scores <- as.numeric(scores)
    
    if (n_drops > 0) {scores[order(scores)[1:n_drops]] <- NA}
    
    mean(scores, na.rm =TRUE)
}

#' @rdname equally_weighted
#' @export
weighted_by_points <- function(scores, weights, n_drops = 0, ...) {
    weights <- as.numeric(weights) #fix coercion issues
    scores <- as.numeric(scores) #fix coercion issues
    
    if (n_drops > 0) {
        drop_idx <- order(scores)[1:n_drops]
        weights[drop_idx] <- NA
        scores[drop_idx] <- NA
    }

    sum(scores * (weights / sum(weights, na.rm = TRUE)), na.rm =TRUE)
}

#' @rdname equally_weighted
#' @export
max_score <- function(scores, ...) {
    max(scores)
}

#' @rdname equally_weighted
#' @export
min_score <- function(scores, n_drops = 0, ...) {
    min(scores)
}

#' @rdname equally_weighted
#' @export
none <- function(scores, ...) {
    ifelse(length(scores) == 1, scores, stop("Can only use `aggregation: none`
                                             if there is only 1 assignment in 
                                             the category."))
}

#' Get one category grade
#'
#' @description
#' 
#' Applies one of the aggregation functions to grade data from one student to 
#' calculate one category grade for that student.
#'
#' @param gs_row A vector of assignment scores and weights coming from a row of the
#' gs data frame.
#' @param policy_item A single-layer list containing, at least `assignments`,
#' a vector and optionally `n_drops`, an integer.
#' 
#' @return A single aggregated category grade.
#' @export
get_one_grade <- function(gs_row, policy_item) {
    get(policy_item$aggregation)(
        scores = gs_row[paste0(policy_item$assignments, "_-_percent")],
        weights = gs_row[paste0(policy_item$assignments, "_-_max_points")],
        n_drops = ifelse(is.null(policy_item$n_drops), 0, policy_item$n_drops))
}

#' Get category grades for all students
#'
#' @description
#' 
#' Iterates `get_one_grade()` across all categories of a policy file and across
#' all students present in a gs file.
#'
#' @param gs A vector of assignment scores and weights coming from a row of the
#' gs data frame.
#' @param policy_item A single-layer list containing, at least `assignments`,
#' a vector and optionally `n_drops`, an integer.
#' 
#' @return An extended version of the gs data frame, with columns added for each 
#' category described in the policy file containing the grades for each student.
#' @export
get_category_grades <- function(gs, policy) {
    for (policy_item in policy) {
        gs[[policy_item$category]] <- apply(gs, 1, get_one_grade, 
                                            policy_item = policy_item)
    }
    gs
}

#' Drop NA Assignments
#' 
#' This function drops any assignments that aren't assigned to a category
#'
#' @param merged_files merged file of pivot_df + policy
#'
#' @return data frame with only assigned assignments
#' @importFrom tidyr drop_na
#' @export
drop_na_assignments <- function(merged_files){
    merged_files <- merged_files %>%
        tidyr::drop_na(category)
  return(merged_files)
}

#' Calculating Lateness
#' 
#' This function calculates lateness; adds a column for calculated score after lateness is accounted for
#'
#' @param lateness merged file with only assigned assignments
#'
#' @return data frame with only assigned assignments including lateness accounted in score
#' @importFrom dplyr mutate case_when
#' @export
lateness <- function(merged_files){
    #num_assigns
    lateness <- merged_files |>
        dplyr::mutate(lateness_min1 = convert_to_min(late_time1),
                      lateness_min2 = convert_to_min(late_time2))|>
        dplyr::mutate(points_after_lateness = case_when(
                      #late1
                      #lateness_min > 0 & lateness_min <= late_time1_min ~ raw_points*as.numeric(late_scale1),
                      
                      lateness_min <= late_time1_min ~ raw_points*as.numeric(late_scale1),
                      #between late1 and late2
                      lateness_min > late_time1_min & lateness_min <= late_time2_min ~ raw_points*as.numeric(late_scale2),
                      #not late
                      TRUE ~ raw_points
                      )) |>
              mutate(score_after_lateness = points_after_lateness/max_points)
    
  
    return(lateness)
}

#' Accounting for Drops
#' 
#' This function accounts the allowed drops per category and removes the dropped assignments 
#' from further calculations
#'
#' @param merged_files merged file with only assigned assignments
#'
#' @return data frame with drops accounted for
#' @importFrom dplyr group_by mutate ungroup case_when
#' @export
#' 
drops <- function(merged_files){
  #num_assigns
  after_drops <- merged_files |>
    dplyr::group_by(sid, category) |>
    dplyr::mutate(num_assigns = n()) |>
    dplyr::ungroup() |>
    dplyr::mutate(relevant_assigns = as.numeric(num_assigns) - as.numeric(drops))#|>
    
  return(after_drops)
}


##############------ want to add a utility function for the drops unstead of pluggin 
##############------ it in the top function drops()

# utility_drop_function <- function(merged_files) {
#         
#         #drops lowest n score after lateness
#         num_drops_in_class <- df_with_lateness %>%
#           summarize(sum_drops = sum(as.numeric(drops)))
#         df_with_drops <- df_with_lateness %>%#default if no drops in class
#           mutate(dropped = FALSE)
#         if (num_drops_in_class > 0){
#           kept_assignments <- df_with_lateness %>%
#             group_by(sid, category) %>%
#             arrange(score_after_lateness) %>% #arrange in ascending order based on group_by
#             slice( (as.numeric(drops) + 1) :n() ) %>% #drop the number of drops and keep the rest assignments
#             mutate(dropped = FALSE)
#           
#           dropped_assignments <- df_with_lateness %>%
#             filter(as.numeric(drops) > 0) %>%
#             group_by(sid, category) %>%
#             arrange(score_after_lateness) %>% #arrange in ascending order based on group_by
#             slice(1: as.numeric(drops)) %>% #drop the number of drops and keep the rest assignments
#             mutate(dropped = TRUE)
#           
#           df_with_drops <- rbind(kept_assignments, dropped_assignments) %>%
#             arrange(sid)   
#         }
#         
# }

#' Convert to Min
#' 
#' Converted a time in HH:MM:SS format to minutes
#'
#' @param hms time in HH:MM:SS format
#'
#' @return number of minutes
#' @importFrom lubridate hms period_to_seconds 
#' @export
convert_to_min <- function(hms){
    save <- lubridate::hms(hms) |>
        lubridate::period_to_seconds()
    save <- save/60
    return (save)
}


# - points/score after lateness
# 
# - drops (can do min/max too??)
# 
# - do category grading by weights
# 
# - calculate overall grades

