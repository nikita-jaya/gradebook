policy_nested <- list(
    list(category = "problem_sets",
         aggregation = "equally_weighted",
         assignments = list(
             list(category = "ps_1",
                  aggregation = "weighted_by_points",
                  assignments = c("ps_1_written", "ps_1_code")),
             list(category = "ps_2",
                  aggregation = "weighted_by_points",
                  assignments = c("ps_2_written", "ps_2_code")))),
    list(category = "exam",
         aggregation = "none",
         assignments = c("final_exam")))

policy_flat <- list(list(category = "ps_1",
                         aggregation = "weighted_by_points",
                         assignments = c("ps_1_written", "ps_1_code")),
                    list(category = "ps_2",
                         aggregation = "weighted_by_points",
                         assignments = c("ps_2_written", "ps_2_code")),
                    list(category = "problem_sets",
                         aggregation = "equally_weighted",
                         assignments = c("ps_1", "ps_2")),
                    list(category = "exam",
                         aggregation = "none",
                         assignments = c("final_exam")))

extract_nested <- function(category) {
    # If there's no more nesting, return the category as a list
    if (!("assignments" %in% names(category) && is.list(category$assignments))) {
        return(list(category))
    }
    
    # Otherwise, get the nested categories
    nested_categories <- purrr::map(category$assignments, extract_nested) |>
        purrr::list_flatten(nested_categories)
    
    # Modify the current category to contain the nested categories as assignments
    category$assignments <- purrr::map_chr(nested_categories, "category")
    
    # Return the modified current list after nested items
    c(nested_categories, list(category))
}

flatten_policy <- function(policy) {
    purrr::map(policy, extract_nested) |>
        purrr::list_flatten()
}

flatten_policy(policy_nested)

