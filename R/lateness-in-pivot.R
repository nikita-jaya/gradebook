#' Create an Lateness Table
#' This function creates an assignment table where each row is an assignment with its category and relevant lateness information.
#'
#' @param flat_policy A validated policy file in flattened form
#'
#' @return A data frame
#'
#' @importFrom purrr map
#' @importFrom data.table rbindlist
#' @importFrom dplyr mutate across starts_with
#' @export
create_lateness_table <- function(flat_policy){
    late_policies <- map(flat_policy, function(x){
        if ("lateness" %in% names(x)){
            return (x)
        }
    })
    
    late_policies <- late_policies[lengths(late_policies) != 0]
    
    assignments <- purrr::map(late_policies, "assignments")
    category <- purrr::map(late_policies, "category") |> unlist()
    
    
    n_assigns_per_cat <- unlist(purrr::map(assignments, length))
    num_cat <- length(category)
    iterations_of_each <- rep(1:num_cat, times = n_assigns_per_cat)
    
    
    assigns_table <- data.frame(assignments = unlist(assignments),
                                category = category[iterations_of_each]
    )
    
    late_len <- purrr::map(late_policies, function(x){
        l <- length(x$lateness$scale)
        return (l)
    }) |> unlist() |> max()
    
    scalars <- purrr::map(late_policies, function(x){
        l <- c(x$category, x$lateness$scale, unlist(x$lateness$interval))
        len <- length(x$lateness$scale)
        names(l) <- c("category",paste0("scale", 1:len), paste0(c("lb", "ub"), rep (1:len, each = 2)))
        return (l)
    }) |> data.table::rbindlist(fill = TRUE)
    
    assigns_table <- left_join(assigns_table, scalars, by = "category") |>
        mutate(across(starts_with("ub"), ~ifelse(.x %in% c(-Inf, Inf), .x,convert_to_min(.x)))) |>
        mutate(across(starts_with("lb"), ~ifelse(.x %in% c(-Inf, Inf), .x,convert_to_min(.x))))
    
    return (assigns_table)
}

#' Pivot for Lateness
#' This function merges processed pivot data with lateness table
#'
#' @param pivot_df A processed pivotted data from Gradescope
#' @param assigns_table A lateness table made from the policy file
#'
#' @return A data frame
#'
#' @importFrom dplyr left_join filter
#' @export
pivot_for_lateness <- function(lateness_table, pivot_df){
    assigns <- lateness_table$assignments |> unique()
    add_categories_to_pivot <- pivot_df |>
        filter(assignments %in% assigns) |>
        left_join(lateness_table, by = c("assignments" = "assignments"))
    
    return (add_categories_to_pivot)
}

#' Calculate Scores after Lateness
#' This function uses the merged pivotted data + lateness table to compute
#' raw scores after lateness.
#'
#' @param lateness_table A processed pivotted data from Gradescope
#'
#' @return A data frame
#'
#' @importFrom dplyr mutate across between starts_with cur_column
#' @importFrom stringr str_replace
#' @export
calculate_lateness <- function(lateness_table){
    lateness <- lateness_table |>
        mutate(`lateness_(h_m_s)` = as.numeric(`lateness_(h_m_s)`)) |>
        mutate(across(starts_with("ub"), as.numeric),
               across(starts_with("lb"), as.numeric),
               across(starts_with("scale"), as.numeric)) |> 
        mutate(across(starts_with("scale"), 
                      ~between(`lateness_(h_m_s)`,get(str_replace(cur_column(), "scale", "lb")),
                               get(str_replace(cur_column(), "scale", "ub")))*.x,
                      .names = '{.col}_final'
        )) |>
        mutate(final_scalar = rowSums(across(ends_with("_final")))) |>
        mutate(score_after_lateness = raw_score*final_scalar)
    
    return (lateness)
}


#' Lateness Scores in Wide Format
#' This function pivots lateness scores from tall format to wide format.
#'
#' @param pivotted_late_scores A pivotted dataframe with scores after lateness
#'
#' @return A data frame
#'
#' @importFrom dplyr select
#' @importFrom tidyr pivot_wider
#' @export
lateness_scores_in_wide <- function(pivotted_late_scores){
    wide_late_scores <- pivotted_late_scores |> 
        select(sid, assignments, score_after_lateness) |>
        pivot_wider(names_from = assignments,
                    names_glue = "{assignments}_-_raw_score",
                    values_from = score_after_lateness)
    return (wide_late_scores)
}

#' Replace Raw Scores with Lateness Scores
#' This function replaces original raw points with the scores after lateness 
#' policies have been applied.
#'
#' @param wide_processed_data A processed data in wide format
#' @param lateness_scores A dataframe with scores after lateness
#'
#' @return A data frame
#'
#' @importFrom dplyr select left_join
#' @export
replace_raw_with_lateness_scores <- function(wide_processed_data, lateness_scores){
    late_assigns <- names(lateness_scores)[names(lateness_scores) != "sid"] #all assignments with late scores
    
    after_lateness <- wide_processed_data |>
        select(-late_assigns) |>
        left_join(lateness_scores, by = "sid")
    
    return (after_lateness)
}

#' Compute Lateness
#' This function computes scores after lateness policy has been applied.
#'
#' @param wide_processed_data A processed data in wide format
#' @param flat_policy A flattened policy file
#'
#' @return A data frame
#'
#' @export
compute_lateness <- function(wide_processed_data, flat_policy){
    lateness_table <- flat_policy |> create_lateness_table()
    
   after_lateness <-  wide_processed_data |>
        pivot_gs() |>
        pivot_for_lateness(lateness_table = lateness_table) |>
        calculate_lateness() |>
        lateness_scores_in_wide() |>
        replace_raw_with_lateness_scores(wide_processed_data = wide_processed_data)
   
   return (after_lateness)
}