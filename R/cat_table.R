#' Add or update category to policy list
#' 
#' @param policy_list a policy list if adding category to previous one.
#' @param name name of category.
#' @param slipdays allowed slip days.
#' @param late_time1 cutoff for first lateness policy.
#' @param late_time2 cutoff for second lateness policy.
#' @param late_scale1 scale for first lateness policy.
#' @param late_scale2 scale for second lateness policy.
#' @param weight weight for category.
#' @param drops allowed drops for category.
#' @param weighted_equally whether it's weighted equally or by points.
#' @param clobber whether it's clobbered.
#' @param assigns any assignments included in category.
#' @returns A list.
#' @export
#' @examples
#' update_category(name = "Labs")
#' update_category(name = "Quizzes")

update_category <- function(policy_list, name = "Category", slipdays = 0, 
                         late_time1 = "00:00:00", late_time2 = "00:00:00", 
                         late_scale1 = 1, late_scale2 = 0, 
                         weight = 0,drops = 0, weighted_equally = TRUE, 
                         clobber = "None", assigns = c()){
    if (missing(policy_list))
        policy_list <- list()
    
    if (name == "Category") 
        name <- paste0("Category ", length(policy_list))
    category <- list("name" = name, "slipdays" = slipdays,
                     "late_time1" = late_time1, "late_time2" = late_time2,
                     "late_scale1" = late_scale1, "late_scale2" = late_scale2,
                     "weight" = weight, "drops" = drops, 
                     "weighted_equally" = weighted_equally,
                     "clobber" = clobber, "assigns" = assigns
    )
    policy_list[[getCatIndex(policy_list, name)]] <- category
    return (policy_list)
    
}

#' Finds index of category within a policy list
#' 
#' @param policy_list a policy list if adding category to previous one.
#' @param name name of category.
#' @returns An integer.
#' @export
#' @examples
#' getCatIndex( update_category(name = "LABS"), "LABS")



getCatIndex <- function(policy_list, name){
    nrs <- purrr::map(policy_list, "name") |>
        unlist()
    i <- which(nrs == name)
    if (length(i) == 0){
        i <- length(policy_list)+1
    }
    return (i)
}