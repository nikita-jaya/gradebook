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
    weights <- purrr::map(policy$categories, "weight") |> unlist()
    
    iterations_of_each_cat <- unlist(purrr::map(assignments, length))
    num_cat <- length(categories)
    iterations_of_each <- rep(1:num_cat, times = iterations_of_each_cat)
    
    
    assigns_table <- data.frame(assignments = unlist(assignments),
                                category = categories[iterations_of_each],
                                weights = weights[iterations_of_each])
    
    assigns_table$assignments <- assigns_table$assignments |>
        tolower() |>
        str_replace_all("[\\s:]+", "_")  # Replace whitespace/special characters
    
    # Replace all "," with ":" in the colnames to avoid 
    # issues with selecting these assignments in creating categories.
    assigns_table$assignments <- str_replace_all(assigns_table$assignments, ",", ":")
    
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
    add_categories_to_pivot <- pivot_df %>%
        left_join(assigns_table, by = c("assignments" = "assignments"))
    
    return (add_categories_to_pivot)
}