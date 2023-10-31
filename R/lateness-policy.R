
determine_scale <- function(interval, scale, late_time){
    late_time <- convert_to_min(late_time)
    scalar = 1 #default
    if (interval[1] != -Inf){
        interval[1] <- convert_to_min(interval[1])
    }
    if (interval[2] != Inf){
        interval[2] <- convert_to_min(interval[2])
    } 
    interval <- as.numeric(interval)
    if (late_time >= interval[1] & late_time <= interval[2]){
        scalar <- scale
    }
    return (scalar)
}

calculate_singular_lateness <- function(late_time, lateness_policy){
    final_scalar <- pmap(lateness_policy, determine_scale, late_time) |>
        unlist() |>
        prod()
    final_scalar
}


lateness_for_one_category <- function(singular_policy, gs_data){
    if ("lateness" %in% names(singular_policy)){
        assignments <- as.list(singular_policy$assignments)
        lateness_policy <- singular_policy$lateness
        for (assign in assignments){
            lateness_score <- lateness_for_one_assignment(gs_data |> select(contains(assign)), lateness_policy) |>
                as.data.frame()
            names(lateness_score) <- paste0(assign, "_after_lateness")
            gs_data <-cbind(gs_data, lateness_score)
        }
    }
    return (gs_data)
}

lateness_for_one_assignment <- function(assign_data, lateness_policy){
    column <- assign_data |> pull(ends_with("_-_lateness_(h_m_s)")) |> as.list()
    scalars <- map2(column, list(lateness_policy), calculate_singular_lateness)
    new_score <- assign_data |> pull(ends_with("_-_raw_score"))*unlist(scalars)
    return (new_score)
}

lateness_for_all <- function(flat_policy, gs_data){
   for (pol in flat_policy){
       gs_data <- lateness_for_one_category(pol, gs_data)
   }
    return (gs_data)
}

