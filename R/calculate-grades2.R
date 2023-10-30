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
        mutate("{sub_category_raw_name}" := rowSums(across(all_of(raw_cols))), #compute raw points for new assignment/category
               "{sub_category_max_name}" := rowSums(across(all_of(max_cols))), #compute max points for new assignment/category
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

#' Calculate Grade by Maximum Score in Category
#' 
#' This functions calculates the grade of a category by weighting assignments by 
#' given weights.
#'
#' @param gs_data a wide formatted, processed Gradescope data
#' @param sub_category name of sub-category grade that is being calculated
#' @param sub_assigns assignments within this subcategory
#' @param weights weights in same order as `sub_assigns`
#'
#' @return a wide formatted dataframe with raw score, max points and percent for new subcategory
#' @importFrom dplyr mutate across rowwise summarize
#' @export
by_weight <- function(gs_data, sub_category, sub_assigns, weights) {
    percent_cols <- paste0(sub_assigns, "_-_percent") #names of raw score columns
    
    total_percent <- gs_data |> #get total score for category
        rowwise() |>
        summarize("{sub_category}" := sum(c_across(percent_cols)*weights)/sum(weights))
    
    gs_data <-cbind(gs_data, total_percent)
    
    return (gs_data)
    
}

#' Calculate Grade by Maximum Score in Category
#' 
#' This functions calculates the grade of a category with the maximum percentage score of the category.
#' The max points of this new category is given by the average of the max points of assignments within
#' this category.
#'
#' @param gs_data a wide formatted, processed Gradescope data
#' @param sub_category name of sub-category grade that is being calculated
#' @param sub_assigns assignments within this subcategory
#'
#' @return a wide formatted dataframe with raw score, max points and percent for new subcategory
#' @importFrom dplyr mutate across rowwise
#' @export
by_max <- function(gs_data, sub_category, sub_assigns){
    percent_cols <- paste0(sub_assigns, "_-_percent") #names of raw score columns
    max_cols <- paste0(sub_assigns, "_-_max_points") #names of max points columns
    
    sub_category_percent_name <- paste0(sub_category, "_-_percent") #name of new subcategory raw score column
    sub_category_max_name <- paste0(sub_category, "_-_max_points") #name of new subcategory max points column
    
    gs_data <- gs_data |>
        rowwise() |>
        mutate("{sub_category_percent_name}" := max(across(all_of(percent_cols))) #compute raw points for new assignment/category
        )  |> ungroup() |>
        mutate(
            "{sub_category_max_name}" := rowMeans(across(all_of(max_cols))), #compute max points for new assignment/category
            "{sub_category}_-_raw_score" := get(sub_category_percent_name)*get(sub_category_max_name) #compute percents for new assignment/category
        )
    
}

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
choose_aggregation <- function(aggregation, gs_data, sub_category, sub_assigns, weights = c()){
    if (aggregation ==  "weighted_by_points"){
        gs_data <- by_points(gs_data, sub_category = sub_category, 
                             sub_assigns = sub_assigns)
    } else if (aggregation == "equally_weighted"){
        gs_data <- by_percent(gs_data, sub_category = sub_category, 
                              sub_assigns = sub_assigns)
    } else if (aggregation == "by_weight"){
        gs_data <- by_weight(gs_data, sub_category = sub_category,
                             sub_assigns = sub_assigns, weights = weights)
    }else if (aggregation == "max"){
        gs_data <- by_max(gs_data, sub_category = sub_category,
                          sub_assigns = sub_assigns)
    } #else if (aggregation == "by_min"){
    #     gs_data <- by_min(gs_data, sub_category = sub_category,
    #                       sub_assigns = sub_assigns)
    # } 
    else {
        gs_data <- by_none(gs_data)
    }
    
    #default == none
    return (gs_data)
} 


#' Calculate Grades with For Loop
#' 
#' This function calculates grades with the by-functions using a for loop
#'
#' @param gs_data processed and prepped Gradescope data
#' @param policy flattened and processed policy file
#'
#' @return the gs_data with appended subcategory and overall grades
#' @export
calculate_grades_with_for_loop <- function(gs_data, flat_policy){
    final_grades <- gs_data
    for (policy in flat_policy){
        if (policy$aggregation == "by_weight"){
            final_grades <- choose_aggregation(sub_category = policy$category, aggregation = policy$aggregation, sub_assigns = unlist(policy$assignments), gs_data = final_grades, weights = policy$weights)
        }
        else {
            final_grades <- choose_aggregation( aggregation = policy$aggregation, gs_data = final_grades, sub_category = policy$category, sub_assigns = unlist(policy$assignments))
        }
    }
    return (final_grades)
}


#' Create Overall Grade Category in Policy File
#' 
#' This function creates a super-category called "Overall Grade" within the policy file.
#' This represents the overall grade for the course.
#'
#' @param policy_nested policy file with (nested) categories
#'
#' @return a policy file with an additional "Overall Grade" category
#' @importFrom purrr map
#' @export
create_overall_category <- function(policy_nested){
    assignments <- map(policy_nested, ~.x$category) |> unlist()
    weights <- map(policy_nested, ~.x$weight) |> unlist()
    i <- length(policy_nested)
    overall_grade_category <- list(category = "overall_grade",
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
