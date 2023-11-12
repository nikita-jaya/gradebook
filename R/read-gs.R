#' Read Gradescope .csv
#'
#' This functions reads the Gradescope .csv, checks for correct format,
#'  converts NA values to zeros and computes scores for each assignment.
#'  This function can also drop ungraded assignments
#'
#' @param path Path to Gradescope CSV
#' @param drop_ungraded whether or not to drop ungraded assignments
#'
#' @return dataframe
#' @importFrom readr read_csv
#' @importFrom dplyr mutate across cur_column mutate_at vars all_of
#' @export
read_gs <- function(path, drop_ungraded = FALSE){
  # read in csv
  gs_data <- read_csv(path) |>
    #check format
    check_data_format()
  
  if (drop_ungraded) {
    gs_data <- gs_data |>
      drop_ungraded_assignments()
  }
  
  gs_data |>
    # convert all NA raw-point values into zeros
    mutate_at(vars(all_of( get_assignments(gs_data) )), ~replace(., is.na(.), 0)) |>
    # replace raw pts with score
    mutate(across(get_assignments(gs_data),
                  ~ . / get(paste0(cur_column(), " - Max Points"))))
}

#' Check Formatting of Gradescope Data
#'
#' This functions checks the column names throughout the Gradescope data.
#' There must be an SID column and at least one assignment.
#' It also gives an alert for what id cols and assignments are in the data.
#'
#' @param gs_data Gradescope data 
#'
#' @return Same dataframe if no error
#' @export
check_data_format <- function(gs_data){
  
  col_names <- colnames(gs_data)
  
  id_cols <- get_id_cols(gs_data, TRUE)
  
  if ( !("SID" %in% id_cols) ){
    stop("There is no SID column")
  }
  
  assignment_names <- get_assignments(gs_data, TRUE)
  
  if (is.null(assignment_names) | length(assignment_names) == 0){
    stop("There are no assignments in this dataframe")
  }
  
  #if correct format, return same dataframe
  return (gs_data)
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
#' @importFrom cli cli_alert_info cli_div cli_text cli_end
#' @export
#' 
#' 
get_id_cols <- function(gs_data, give_alert = FALSE){
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
#' @importFrom cli cli_alert_info cli_div cli_text cli_end
#' @export
#' 
#' 
#' 
get_assignments <- function(gs_data, give_alert = FALSE){
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

#' Drop Ungraded Assignments
#'
#' This functions drops any assignments that have no grades for any students and replaced -Inf values
#'
#' @param gs_data A Gradescope data
#' @param give_alert whether or not to return an alert of assignments
#' @importFrom dplyr filter select
#' @importFrom purrr keep
#' @importFrom cli cli_alert_info cli_div cli_text cli_end
#' @return same dataframe without graded assignments
#' @export
drop_ungraded_assignments<- function(gs_data, give_alert = TRUE){
  
  assignments <- get_assignments(gs_data)
  #These are the dropped assignments with all NAs for raw-score
  dropped <- gs_data |> keep(~all(is.na(.x))) |> names()
  dropped <- dropped[dropped %in% assignments]
  
  alert <- function() {
    cli::cli_div(theme = list(span.emph = list(color = "orange")))
    cli::cli_text("{.emph Important Message}")
    cli::cli_end()
    if (length(dropped) != 0) {
      cli::cli_alert_info("These are your ungraded assignments: {dropped}")
    } else {
      cli::cli_alert_info("These are no ungraded assignments")
    }
  }
  if (give_alert){
    alert()
  }
  gs_data |> select(-contains(dropped))
}