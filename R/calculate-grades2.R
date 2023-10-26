compute_scores <- function(gs_data){
    #add _score columns!
} #need to code

by_points <- function(gs_data, aggregate_assign, sub_assigns){
    total_raw <- gs_data |>   #sum up all earned points
        rowwise() |>
        summarize(raw = sum(c_across(sub_assigns))) %>%
        pull()
    
    max_cols <- paste0(sub_assigns, "_max_pts")
    
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
    colnames(gs_data)[(l-2):l] <- paste0(aggregate_assign, c("", "_score", "_max_pts"))
    
    return (gs_data)
}
by_percent <- function(gs_data, aggregate_assign, sub_assigns){
    percent_cols <- paste0(sub_assigns, "_score")
    
    total_percent <- gs_data |> #get total score for category
        rowwise() |>
        summarize(raw = mean(c_across(percent_cols))) %>%
        pull()
    
    max_cols <- paste0(sub_assigns, "_max_pts")
    
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
    colnames(gs_data)[(l-2):l] <- paste0(aggregate_assign, c("", "_score", "_max_pts"))
    
    return (gs_data)
}
by_weight <- function(gs_data, aggregate_assign, sub_assigns, weights) {
    percent_cols <- paste0(sub_assigns, "_score")
    
    total_percent <- gs_data |> #get total score for category
        rowwise() |>
        summarize(raw = sum(c_across(percent_cols)*weights)) %>% #uses weights
        pull()
    
    max_cols <- paste0(sub_assigns, "_max_pts")
    
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
    colnames(gs_data)[(l-2):l] <- paste0(aggregate_assign, c("", "_score", "_max_pts"))
    
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

#drop lateness and submission and calcualte scores
prep_for_grading <- function(gs_data){
    
}