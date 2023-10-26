#' Compute Percentages
#' 
#' This function computes percents by dividng the raw_scores by max_points for 
#' each assignment in a wide formatted, processed Gradescope data
#'
#' @param processed_gs_data a wide formatted, processed Gradescope data
#'
#' @return a wide formatted dataframe with percentages
#' @importFrom dplyr mutate across ends_with
#' @importFrom stringr str_replace
#' @export
compute_percent <- function(processed_gs_data){
        computed_data <- processed_gs_data |>
            mutate(across(ends_with('_raw_score'), ~ ./get(str_replace(cur_column(), 
                                                                       '_-_raw_score', '_-_max_points')), .names = '{.col}_percent'))
        colnames(computed_data) <- str_replace(colnames(computed_data), "raw_score_percent", "percent")
    return (computed_data)
}

by_points <- function(gs_data, aggregate_assign, sub_assigns){
    raw_cols <- paste0(sub_assigns, "_-_raw_score")
    total_raw <- gs_data |>   #sum up all earned points
        rowwise() |>
        summarize(raw = sum(c_across(sub_assigns))) %>%
        pull()
    
    max_cols <- paste0(sub_assigns, "_-_max_points")
    
    total_max <- gs_data |>   #sum up all max points
        rowwise() |>
        summarize(raw = sum(c_across(max_cols))) %>%
        pull()
    
    gs_data <- gs_data |>
        mutate(raw = total_raw, #raw pts for whole category
               score = total_raw/total_max, #computed percentage for whole category
               max = total_max) #total max points for whole category
    l <- length(colnames(gs_data))
    #below is naming columns by my convention of `lab1`, `lab1_score` and `lab1_max_pts`
    colnames(gs_data)[(l-2):l] <- paste0(aggregate_assign, c("", "_score", "_-_max_points"))
    
    return (gs_data)
}
by_percent <- function(gs_data, aggregate_assign, sub_assigns){
    percent_cols <- paste0(sub_assigns, "_-_percent")
    
    total_percent <- gs_data |> #get total score for category
        rowwise() |>
        summarize(raw = mean(c_across(percent_cols))) %>%
        pull()
    
    max_cols <- paste0(sub_assigns, "_-_max_points")
    
    total_max <- gs_data |> #get total max_pts for category
        rowwise() |>
        summarize(raw = sum(c_across(max_cols))) %>%
        pull()
    
    gs_data <- gs_data |>
        mutate(raw = total_percent*total_max, #computed raw_pts for category
               score = total_percent, #score for category
               max = total_max) #total max_pts for category
    l <- length(colnames(gs_data))
    #below is naming columns by my convention of `lab1`, `lab1_score` and `lab1_max_pts`
    colnames(gs_data)[(l-2):l] <- paste0(aggregate_assign, c("", "_score", "_-_max_pts"))
    
    return (gs_data)
}
by_weight <- function(gs_data, aggregate_assign, sub_assigns, weights) {
    percent_cols <- paste0(sub_assigns, "_score")
    
    total_percent <- gs_data |> #get total score for category
        rowwise() |>
        summarize(raw = sum(c_across(percent_cols)*weights)) %>% #uses weights
        pull()
    
    max_cols <- paste0(sub_assigns, "_-_max_pts")
    
    total_max <- gs_data |> #get total max_pts for category
        rowwise() |>
        summarize(raw = sum(c_across(max_cols))) %>%
        pull()
    
    gs_data <- gs_data |>
        mutate(raw = total_percent*total_max, #computed raw_pts for category
               score = total_percent, #score for category
               max = total_max) #total max_pts for category
    
    l <- length(colnames(gs_data))
    #below is naming columns by my convention of `lab1`, `lab1_score` and `lab1_max_pts`
    colnames(gs_data)[(l-2):l] <- paste0(aggregate_assign, c("", "_score", "_-_max_pts"))
    
    return (gs_data)
    
}
by_max <- function(gs_data){
} #need to code
by_min <- function(gs_data){
} #need to code
by_none <- function(gs_data){
    return (gs_data)
}

choose_aggregation <- function(aggregate_assign, aggregation,sub_assigns, gs_data, weights = c(), ...){
    if (aggregation ==  "by_points"){
        return ( by_points(gs_data, aggregate_assign = aggregate_assign, 
                           sub_assigns = sub_assigns) )
    } else if (aggregation == "by_percent"){
        return ( by_percent(gs_data, aggregate_assign = aggregate_assign, 
                            sub_assigns = sub_assigns) )
    } else if (aggregation == "by_weight"){
        return ( by_weight(gs_data, aggregate_assign = aggregate_assign, 
                           sub_assigns = sub_assigns, weights = weights) )
    }
    
    #default == none
    return (by_none(gs_data))
}

calculate_grades_with_for_loop <- function(gs_data, policy){
    #NOTE TO SELF: add policy$weights to policy file!!!!
    final_grades <- gs_data
    for (i in 1:length(policy$name)){
        if (policy$aggregation[i] == "by_weight"){
            final_grades <- choose_aggregation(policy$category[i], policy$aggregation[i], unlist(policy$assignments[i]), final_grades, weights = policy$weights)
        }
        else {
            final_grades <- choose_aggregation(policy$category[i], policy$aggregation[i], unlist(policy$assignments[i]), final_grades) 
        }
    }
}

create_overall_category <- function(policy_nested){
    assignments <- purrr::map(policy_nested, ~.x$category) |> unlist()
    weights <- purrr::map(policy_nested, ~.x$weight) |> unlist()
    i <- length(policy_nested)
    overall_grade_category <- list(category = "Overall Grade",
                                   aggregation = "by_weight",
                                   weights = weights,
                                   assignments = assignments)
    policy_nested <- append(policy_nested, list(overall_grade_category))
    return (policy_nested)
}

#' Prep Processed Gradescope
#' 
#' This function prepares processed Gradescope data for grading by computing percentages.
#' If wide == TRUE, this function removes all lateness and submission columns
#'
#' @param processed_gs_data a wide formatted, processed Gradescope data
#' @param wide TRUE if in wide format, FALSE if in pivotted format
#'
#' @return a wide formatted dataframe with scores
#' @importFrom dplyr select matches mutate_at vars contains mutate
#' @export
prep_for_grading <- function(processed_gs_data, wide = TRUE){
    prepped_data <- processed_gs_data
    if (wide){
        prepped_data <- processed_gs_data |>
            select(!matches(c("lateness", "submission"))) |>  #only use raw_score and max_pts cols
            mutate_at(vars(contains("raw")), ~replace(., is.na(.), 0)) |>#change any NA scores to zeros
            compute_percent() #compute so raw_score column has percentages
    } else{ #if in pivotted form
        prepped_data <- processed_gs_data |>
            mutate(percent = raw_score/max_points)
    }
    
    return (prepped_data)
}
