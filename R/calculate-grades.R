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
    save <- lubridate::hms(hms)
    save <- lubridate::period_to_seconds(save)
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

