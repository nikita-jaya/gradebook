#' Compute Lateness
#' This function computes scores after lateness policy has been applied.
#'
#' @param wide_processed_data A processed data in wide format
#' @param flat_policy A flattened policy file
#'
#' @return A data frame
#'
#' @importFrom dplyr filter left_join select
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_remove_all
#' 
#' @export
calculate_lateness <- function(gs, policy){
  
  #create lateness table from flat_policy
  lateness_table <- policy |> create_lateness_table()
  
  if (is.null(lateness_table)) {
    return (gs) #if no lateness policy
  }
  
  #pivot gs into tall format
  pivotted_gs <-  gs |>
    pivot_gs()
  
  #list of assignments with lateness policy
  assigns <- lateness_table$assignments |> unique()
  
  after_lateness_scores <- pivotted_gs |>
    filter(assignments %in% assigns) |>
    
    #append lateness table to pivotted gs
    left_join(lateness_table, by = c("assignments" = "assignments")) |>
    
    #calculate scores after lateness
    calculate_scores_after_lateness() |>
    
    #pivot dataframe back into wide format
    pivot_wider(names_from = assignments,
                names_glue = "{assignments} - Score",
                values_from = score_after_lateness)
  
  late_assigns <- names(after_lateness_scores)[(names(after_lateness_scores)) != "SID"] #all assignments with late scores
  late_assigns <- str_remove_all(late_assigns, " - Score")
  #replace new lateness scores in original dataframe
  gs |>
    select(-late_assigns) |>
    left_join(after_lateness_scores, by = "SID") 
}

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
  
  if (is.null(late_policies[[1]])){
    return (NULL)
  }
  
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

#' Calculate Scores after Lateness
#' This function uses the merged pivotted data + lateness table to compute
#' raw scores after lateness.
#'
#' @param lateness_table A processed pivotted data from Gradescope
#'
#' @return A data frame
#'
#' @importFrom dplyr mutate across between starts_with cur_column select
#' @importFrom stringr str_replace
#' @export
calculate_scores_after_lateness <- function(lateness_table){
  lateness_table |>
    mutate(`Lateness (H:M:S)` = as.numeric(convert_to_min(`Lateness (H:M:S)`))) |>
    mutate(across(starts_with("ub"), as.numeric),
           across(starts_with("lb"), as.numeric),
           across(starts_with("scale"), as.numeric)) |> 
    mutate(across(starts_with("scale"), 
                  ~between(`Lateness (H:M:S)`,get(str_replace(cur_column(), "scale", "lb")),
                           get(str_replace(cur_column(), "scale", "ub")))*.x,
                  .names = '{.col}_final'
    )) |>
    mutate(final_scalar = rowSums(across(ends_with("_final")), na.rm = TRUE)) |>
    mutate(score_after_lateness = Score*final_scalar) |>
    select(SID, assignments, score_after_lateness)
}