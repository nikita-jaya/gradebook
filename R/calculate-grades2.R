#' Calculate Grade Weighted by Points
#' 
#' This functions calculates the grade of a category with each assignment weighted by 
#' how many points it is worth.
#'
#' @param gs_data a wide formatted, processed Gradescope data
#' @param sub_category name of sub-category grade that is being calculated
#' @param sub_assigns assignments within this subcategory
#'
#' @return a wide formatted dataframe with raw score, max points and percent for new subcategory
#' @importFrom dplyr mutate across 
#' @export
by_points <- function(gs_data, sub_category, sub_assigns){
    raw_cols <- paste0(sub_assigns, "_-_raw_score") #names of raw score columns
    max_cols <- paste0(sub_assigns, "_-_max_points") #names of max points columns
    
    sub_category_raw_name <- paste0(sub_category, "_-_raw_score") #name of new subcategory raw score column
    sub_category_max_name <- paste0(sub_category, "_-_max_points") #name of new subcategory max points column
    
    gs_data <- gs_data |>
        mutate("{sub_category_raw_name}" := rowSums(across(raw_cols)), #compute raw points for new assignment/category
               "{sub_category_raw_name}" := rowSums(across(max_cols)), #compute max points for new assignment/category
               "{sub_category}_-_percent" := get(sub_category_raw_name)/get(sub_category_max_name) #compute percents for new assignment/category
               ) 
    return (gs_data)
}

#' Calculate Grade Weighted by Percent
#' 
#' This functions calculates the grade of a category with each assignment weighted equally
#'
#' @param gs_data a wide formatted, processed Gradescope data
#' @param sub_category name of sub-category grade that is being calculated
#' @param sub_assigns assignments within this subcategory
#'
#' @return a wide formatted dataframe with raw score, max points and percent for new subcategory
#' @importFrom dplyr mutate across 
#' @export
by_percent <- function(gs_data, sub_category, sub_assigns){
    percent_cols <- paste0(sub_assigns, "_-_percent") #names of raw score columns
    max_cols <- paste0(sub_assigns, "_-_max_points") #names of max points columns
    
    sub_category_percent_name <- paste0(sub_category, "_-_percent") #name of new subcategory raw score column
    sub_category_max_name <- paste0(sub_category, "_-_max_points") #name of new subcategory max points column
    
    gs_data <- gs_data |>
        mutate("{sub_category_percent_name}" := rowMeans(across(percent_cols)), #compute raw points for new assignment/category
               "{sub_category_max_name}" := rowSums(across(max_cols)), #compute max points for new assignment/category
               "{sub_category}_-_raw_score" := get(sub_category_percent_name)*get(sub_category_max_name) #compute percents for new assignment/category
        ) 
    
    return (gs_data)
}

# by_weight <- function(gs_data, aggregate_assign, sub_assigns, weights) {
#     percent_cols <- paste0(sub_assigns, "_score")
#     
#     total_percent <- gs_data |> #get total score for category
#         rowwise() |>
#         summarize(raw = sum(c_across(percent_cols)*weights)) %>% #uses weights
#         pull()
#     
#     max_cols <- paste0(sub_assigns, "_-_max_pts")
#     
#     total_max <- gs_data |> #get total max_pts for category
#         rowwise() |>
#         summarize(raw = sum(c_across(max_cols))) %>%
#         pull()
#     
#     gs_data <- gs_data |>
#         mutate(raw = total_percent*total_max, #computed raw_pts for category
#                score = total_percent, #score for category
#                max = total_max) #total max_pts for category
#     
#     l <- length(colnames(gs_data))
#     #below is naming columns by my convention of `lab1`, `lab1_score` and `lab1_max_pts`
#     colnames(gs_data)[(l-2):l] <- paste0(aggregate_assign, c("", "_score", "_-_max_pts"))
#     
#     return (gs_data)
#     
# } #need to re-code
by_max <- function(gs_data){
} #need to code
by_min <- function(gs_data){
} #need to code

#' No Grading Policy Function
#' 
#' This function is used as a placeholder function when there is no grading policy.
#'
#' @param gs_data a wide formatted, processed Gradescope data
#'
#' @return the same wide formatted dataframe
#' @export
by_none <- function(gs_data){
    return (gs_data)
}

#' Choose Aggregation Function
#' 
#' This function determines which aggregation function to apply for calculating
#' the grade of a subcategory.
#'
#' @param aggregation a string denoting which aggregation to use
#' @param ... other values needed for the relevant grade calculation
#'
#' @return the  wide formatted dataframe with the new calculated subcategory
#' @export
choose_aggregation <- function(aggregation, ...){
    if (aggregation ==  "weighted_by_points"){
        return ( by_points(gs_data, sub_category = sub_category, 
                           sub_assigns = sub_assigns) )
    } else if (aggregation == "equally_weighted"){
        return ( by_percent(gs_data, sub_category = sub_category, 
                            sub_assigns = sub_assigns) )
    } #else if (aggregation == "by_weight"){
    #     return ( by_weight(gs_data, sub_category = sub_category, 
    #                        sub_assigns = sub_assigns, weights = weights) )
    # } else if (aggregation == "by_max"){
    #     return ( by_max(gs_data, sub_category = sub_category, 
    #                         sub_assigns = sub_assigns) )
    # } else if (aggregation == "by_min"){
    #     return ( by_min(gs_data, sub_category = sub_category, 
    #                         sub_assigns = sub_assigns) )
    # }
    
    #default == none
    return (by_none(gs_data))
}

calculate_grades_with_for_loop <- function(gs_data, policy){
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
