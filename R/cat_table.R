#' Add or update category to policy list
#' 
#' @param policy_list a policy list if adding category to previous one.
#' @param name name of category.
#' @param slipdays allowed slip days.
#' @param late_time1 cutoff for first lateness policy.
#' @param late_time2 cutoff for second lateness policy.
#' @param late_scale1 scale for first lateness policy.
#' @param late_scale2 scale for second lateness policy.
#' @param after whether late_scale1/late_scale2 is applied after or until late_time1/late_time2
#' @param weight weight for category.
#' @param drops allowed drops for category.
#' @param weight_method whether it's weighted equally or by points.
#' @param clobber whether it's clobbered.
#' @param assigns any assignments included in category.
#' @returns A list.
#' @export
#' @examples
#' update_category(name = "Labs")
#' update_category(name = "Quizzes")

update_category <- function(policy_list, name = "Category", slipdays = 0, 
                         late_time1 = "00:00:00", late_time2 = "00:00:00", 
                         late_scale1 = 1, late_scale2 = 0, after = TRUE,
                         weight = 0,drops = 0, weight_method = c("equally", "by_points"), 
                         clobber = "None", assigns = c()){
    if (missing(policy_list))
        policy_list <- list()
    
    if (name == "Category") 
        name <- paste0("Category ", length(policy_list))
    category <- list("name" = name, "slipdays" = slipdays,
                     "late_time1" = late_time1, "late_time2" = late_time2,
                     "late_scale1" = late_scale1, "late_scale2" = late_scale2,
                     "after" = after,"weight" = weight, "drops" = drops, 
                     "weighted_method" = weight_method,
                     "clobber" = clobber, "assigns" = assigns
    )
    policy_list[[get_cat_index(policy_list, name)]] <- category
    return (policy_list)
    
}

#' Finds index of category within a policy list
#' 
#' @param policy_list a policy list if adding category to previous one.
#' @param name name of category.
#' @returns An integer.
#' @export
#' @examples
#' get_cat_index( update_category(name = "LABS"), "LABS")



get_cat_index <- function(policy_list, name){
    nrs <- purrr::map(policy_list, "name") |>
        unlist()
    i <- which(nrs == name)
    if (length(i) == 0){
        i <- length(policy_list)+1
    }
    return (i)
}

#' Deletes category with name "name"
#' 
#' @param policy_list a policy list if adding category to previous one.
#' @param name name of category.
#' @returns A list.
#' @export
delete_category <- function(policy_list, name){
    i <- get_cat_index(policy_list, name)
    policy_list <- policy_list[-i]
    return (policy_list)
}