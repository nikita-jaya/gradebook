#' Create Assignments Table from Input Data
#'
#' This function takes the gradescope dataframe after some assignments have been created generating an assignment table.
#' 
#'
#' @param assignments_gs A dataframe (csv) from gradescope containing the unique student IDs information 
#' (this is the output of the `unique_student_ids` function).
#' 
#' @return A tibble dataframe with the processed assignment table.
#' 
#' @importFrom tibble tibble
#' @importFrom stringr str_replace_all str_detect
#' @importFrom dplyr mutate select if_else
#' 
#' @export
assignments_gs <- function(unique_student_ids) {
  gs_cols <- names(unique_student_ids)
  
  #fix names discrepancies 
  if (gs_cols[1] == "Name"){
    gs_cols[1] <- "Names"
  }

  assignments <- tibble(colnames = gs_cols) |>
    # General regex to rename assignments and add a column "category" in table
    mutate(new_colnames = str_replace_all(tolower(colnames), "[\\s:]+", "_"),
           type = if_else(!str_detect(new_colnames, "name|sections|max|time|late|email|sid"), "_-_raw_points", "" ),
           new_colnames = paste0(new_colnames, type))|> # concatenate gs_col and type
    select(new_colnames, colnames) |>
    mutate(category = "Unassigned")
  
  # Replace all "," with ":" in the colnames to avoid the issue with selecting these assignments in creating categories.
  assignments$colnames <- str_replace_all(assignments$colnames, ",", ":")
  assignments$new_colnames <- str_replace_all(assignments$new_colnames, ",", ":")
  
  return(assignments)
}
