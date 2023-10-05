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
    merged_files %>%
        tidyr::drop_na(category)
}

#' Intermediary Grading Computations
#' 
#' This function creates columns of computations needed for grading, like number of relevant
#' assignments and late_time1/late_time2 in terms of minutes
#'
#' @param merged_files merged file with only assigned assignments
#'
#' @return data frame with only assigned assignments and intermediary columns
#' @importFrom dplyr group_by mutate ungroup case_when
#' @export
intermediary_computations <- function(merged_files){
    #num_assigns
    merged_files %>%
        dplyr::group_by(sid, category) %>%
        dplyr::mutate(num_assigns = n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(relevant_assigns = dplyr::case_when(
            is.numeric(drops) ~ num_assigns-drops,
            !is.numeric(drops) ~ 1), #if drop is "min" or "max"
        lateness_min1 = convert_to_min(late_time1),
        lateness_min2 = convert_to_min(late_time2)
        )
    
}

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

