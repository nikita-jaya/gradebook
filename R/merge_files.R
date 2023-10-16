#' Create an Assignments Table from Policy File
#' This function creates an assignment table where each row is an assignment with its category and relevant category information.
#' This is an intermediary table used to combine the policy file and Gradescope data
#'
#' @param policy A validated policy file
#'
#' @return A data frame
#'
#' @examples
#' # Example
#' create_assigns_table(policy_demo)
#' @importFrom purrr map
#' @importFrom stringr str_replace_all

#' @export
#' 
create_assigns_table <- function(policy){
    assignments <- purrr::map(policy$categories, "assigns") 
    
    categories <- purrr::map(policy$categories, "name") |> unlist()
    slipdays <- purrr::map(policy$categories, "slipdays") |> unlist()
    late_time1 <- purrr::map(policy$categories, "late_time1") |> unlist()
    late_time2 <- purrr::map(policy$categories, "late_time2") |> unlist()
    late_scale1 <- purrr::map(policy$categories, "late_scale1") |> unlist()
    late_scale2 <- purrr::map(policy$categories, "late_scale2") |> unlist()
    after <- purrr::map(policy$categories, "after") |> unlist()
    weights <- purrr::map(policy$categories, "weight") |> unlist()
    drops <- purrr::map(policy$categories, "drops") |> unlist()
    weighted_method <- purrr::map(policy$categories, "weighted_method") |> unlist()
    clobber <- purrr::map(policy$categories, "clobber") |> unlist()
    
    
    n_assigns_per_cat <- unlist(purrr::map(assignments, length))
    num_cat <- length(categories)
    iterations_of_each <- rep(1:num_cat, times = n_assigns_per_cat)
    
    
    assigns_table <- data.frame(assignments = unlist(assignments),
                                category = categories[iterations_of_each],
                                slipdays = slipdays[iterations_of_each],
                                late_time1 = late_time1[iterations_of_each],
                                late_time2 = late_time2[iterations_of_each],
                                late_scale1 = late_scale1[iterations_of_each],
                                late_scale2 = late_scale2[iterations_of_each],
                                after = after[iterations_of_each],
                                weights = weights[iterations_of_each],
                                drops = drops[iterations_of_each],
                                weighted_method = weighted_method[iterations_of_each],
                                clobber = clobber[iterations_of_each]
                                )
    
    return (assigns_table)
}
#' Merges processed pivot data
#' This function merges processed pivot data with assignment table
#'
#' @param pivot_df A processed pivotted data from Gradescope
#' @param assigns_table An assignment table made from the policy file
#'
#' @return A data frame
#'
#' @importFrom dplyr left_join
#' @export
data_merge_assigns <- function(assigns_table, pivot_df){
    add_categories_to_pivot <- pivot_df |>
        left_join(assigns_table, by = c("assignments" = "assignments"))
    
    return (add_categories_to_pivot)
}