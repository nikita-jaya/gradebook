#' Read Gradescope .csv
#'
#' This functions reads the Gradescope .csv, converts NA values to zeros and
#' computes scores for each assignment
#'
#' @param path Path to Gradescope CSV
#'
#' @return dataframe
#' @importFrom readr read_csv
#' @importFrom dplyr mutate across cur_column mutate_at vars all_of
#' @export
read_gs <- function(path){
  # read in csv
  gs_data <- read_csv(path)
  
  gs_data|>
    # convert all NA raw-point values into zeros
    mutate_at(vars(all_of( get_assignments(gs_data) )), ~replace(., is.na(.), 0)) |>
    # replace raw pts with score
    mutate(across(get_assignments(gs_data),
                  ~ . / get(paste0(cur_column(), " - Max Points"))))
}

#' Get the ID Columns for Gradescope Data
#'
#' This function identified the id columns from gradescope data
#'
#' @param gs_data  Gradescope dataframe
#' @param give_alert whether or not to return an alert of assignments
#' 
#' @return a list of id columns 
#' @importFrom stringr str_replace_all regex
#' @importFrom cli cli_alert_info
#' @export
#' 
#' 
get_id_cols <- function(gs_data, give_alert = TRUE){
  #REGEX pattern: case INsensitive, then matches the extensions
  #works with untouched GS dataframe so we can match the pattern
  regex = "(?i)( - max points| - submission time| - lateness \\(h:m:s\\))"
  
  # extract base names and excludes the extensions (max points, submission time and lateness)
  base_names <- stringr::str_replace_all(names(gs_data),regex, "")
  
  # Count occurrences of base names
  base_name_counts <- table(base_names)
  
  # identify base names that repeat exactly 4 times
  repeating <- names(base_name_counts[base_name_counts == 4])
  
  # identify columns to keep: those not repeating 4 times
  columns_to_keep <- names(gs_data)[!(base_names %in% repeating)]
  
  alert <- function() {
    cli::cli_div(theme = list(span.emph = list(color = "orange")))
    cli::cli_text("{.emph Important Message}")
    cli::cli_end()
    cli::cli_alert_info("The ID columns from Gradescope are {columns_to_keep}")
  }
  
  if (give_alert){
    alert()
  }
  
  
  return(columns_to_keep)
}

#' Get the Assignment Names for Gradescope Data
#'
#' This function identified the assignments from Gradescope data
#'
#' @param gs_data unprocessed Gradescope dataframe
#' @param give_alert whether or not to return an alert of assignments
#' 
#' @return vector 
#' @importFrom stringr str_replace_all regex
#' @importFrom cli cli_alert_info
#' @export
#' 
#' 
#' 
get_assignments <- function(gs_data, give_alert = TRUE){
  #REGEX pattern: case INsensitive, then matches the extensions
  #works with untouched GS dataframe so we can match the pattern
  regex = "(?i)( - max points| - submission time| - lateness \\(h:m:s\\))"
  
  # extract base names and excludes the extensions (max points, submission time and lateness)
  base_names <- stringr::str_replace_all(names(gs_data),regex, "")
  
  # Count occurrences of base names
  base_name_counts <- table(base_names)
  
  # identify base names that repeat exactly 4 times
  assignment_names <- names(base_name_counts[base_name_counts == 4])
  
  alert <- function() {
    cli::cli_div(theme = list(span.emph = list(color = "orange")))
    cli::cli_text("{.emph Important Message}")
    cli::cli_end()
    cli::cli_alert_info("The assignments from Gradescope are {assignment_names}")
  }
  
  if (give_alert){
    alert()
  }
  
  return (assignment_names)
}
