#' Read Gradescope CSV File
#'
#' This functions reads the Gradescope CSV file and checks for correct Gradescope
#' format. Each assignment should follow the four-column format:
#' `Assignment Name`, `Assignment Name - Max Points`, `Assignment Name - Submission Time`,
#' `Assignment Name - Lateness (H:M:S)`. All other columns are designed as ID columns.
#'  
#'
#' @param path Path to Gradescope CSV file
#' @param verbose Whether or not to print messages
#'
#' @return A dataframe of Gradescope CSV, if no errors
#' @importFrom readr read_csv
#' @importFrom dplyr mutate across cur_column mutate_at vars all_of ends_with
#' @export
read_gs <- function(path, verbose = FALSE){
  # read in csv
  gs <- read_csv(path, trim_ws = FALSE) |>
    #check format
    check_data_format()
}

#' Check Formatting of Gradescope Data
#'
#' This functions checks the column names throughout the Gradescope data.
#' There must be an SID column and at least one assignment.
#' It also gives an alert for what ID columnss and assignments are in the data.
#'
#' @param gs Gradescope data frame
#' @param verbose Whether or not to print messages
#'
#' @return Same gs dataframe if no errors.
#' @export
check_data_format <- function(gs, verbose = FALSE){
  
  col_names <- colnames(gs)
  
  id_cols <- get_id_cols(gs, verbose = verbose)
  
  if ( !("SID" %in% id_cols) ){
    stop("There is no SID column")
  }
  
  assignment_names <- get_assignments(gs, verbose = verbose)
  
  if (is.null(assignment_names) | length(assignment_names) == 0){
    stop("There are no assignments in this dataframe")
  }
  
  #if correct format, return same dataframe
  return (gs)
}

#' Get the ID Columns for Gradescope Data
#'
#' This function identifies the ID columns from Gradescope data.
#'
#' @param gs  Gradescope dataframe
#' @param verbose Whether or not to return an alert of assignments
#' 
#' @return A vector of the names of the ID columns in the dataframe
#' @importFrom stringr str_replace_all regex
#' @importFrom cli cli_alert_info cli_div cli_text cli_end
#' @export

get_id_cols <- function(gs, verbose = FALSE) {
  #REGEX pattern: case INsensitive, then matches the extensions
  regex <- "(?i)( - max points| - submission time| - lateness \\(h:m:s\\))"
  
  # extract base names and excludes the extensions (max points, submission time and lateness)
  base_names <- stringr::str_replace_all(names(gs), regex, "")
  
  # Count occurrences of base names
  base_name_counts <- table(base_names)
  
  # identify base names that repeat exactly 4 times
  repeating <- names(base_name_counts[base_name_counts == 4])
  
  # identify columns to keep: those not repeating 4 times
  columns_to_keep <- names(gs)[!(base_names %in% repeating)]
  
  alert <- function() {
    cli::cli_div(theme = list(span.emph = list(color = "orange")))
    cli::cli_text("{.emph Important Message}")
    cli::cli_end()
    cli::cli_alert_info("The ID columns from Gradescope are {columns_to_keep}")
  }
  
  if (verbose){
    alert()
  }
  
  return(columns_to_keep)
}

#' Get the Assignment Names for Gradescope Data
#'
#' This function identifies the assignments from Gradescope data and returns the assignments' names.
#'
#' @param gs Gradescope dataframe
#' @param verbose Whether or not to print assignment names
#' 
#' @return A vector of the names of the assignments in the dataframe
#' @importFrom stringr str_replace_all regex
#' @importFrom cli cli_alert_info cli_div cli_text cli_end
#' @export

get_assignments <- function(gs, verbose = FALSE){
  #REGEX pattern: case INsensitive, then matches the extensions
  #works with untouched GS dataframe so we can match the pattern
  regex = "(?i)( - max points| - submission time| - lateness \\(h:m:s\\))"
  
  # extract base names and excludes the extensions (max points, submission time and lateness)
  base_names <- stringr::str_replace_all(names(gs), regex, "")
  
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
  
  if (verbose){
    alert()
  }
  
  return (assignment_names)
}