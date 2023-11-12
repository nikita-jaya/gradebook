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


#' Convert GS data to longer table
#'
#' This function takes a gradescope dataframe after it has been processed
#'  to remove duplicate students or invalid IDs
#' and it applies pivot longer function to allow for unit of observation be a student + individual assignment
#'
#' @param gs_data A dataframe (csv from Gradescope) containing a column named "sid" which holds student IDs.
#' @param names_sep String that controls how names are split during the pivot
#'
#' @return A dataframe "student_assignments_long": A dataframe with a unit of observation a student + each assignment.
#'
#' @examples
#'
#' gs_data <- tibble::tibble(
#'   `SID` = c(3032412514, 3032122516, 3032412516,3032412517,
#'             3032412518, 3032412519,3032412521, 3032412521),
#'   `Sections` = c("Class 001", "Class 001", "Class 001", "Class 001",
#'                 "Class 001", "Class 001", "Class 001", "Class 001"),
#'   
#'   `Names` = c("John Smith", "Jane Doe", "Robert Brown", "Emily Johnson",
#'              "Michael Davis", "Linda Wilson", "James Taylor", "Patricia Anderson"),
#'   `Email` = c("john.smith@berkeley.edu", "jane.doe@berkeley.edu",
#'               "robert.brown@berkeley.edu", "emily.johnson@berkeley.edu",
#'               "michael.davis@berkeley.edu", "linda.wilson@berkeley.edu",
#'               "james.taylor@berkeley.edu", "patricia.anderson@berkeley.edu" ),
#'   `Lab 1` = c(1, 0, 0.9, 0.5, 1, 0.9, 1, 0.8),
#'   `Lab 1 - Max Points` = c(1, 1, 1, 1, 1, 1, 1, 1),
#'   `Lab 1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0", 
#'                                "1/19/2023 10:00:00 AM", "0",
#'                                "1/19/2023 9:00:00 AM", "0", 
#'                                "1/19/2023 9:20:00 AM", "0"),
#'   `Lab 1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00",
#'                                 "0:00:00", "0:00:00", "0:00:00", "0:00:00"),
#'   
#'   `Lab 2` = c(1, 0, 0.9, 0.5, 1, 0.9, 1, 0.9),
#'   `Lab 2 - Max Points` = c(1, 1, 1, 1, 1, 1, 1, 1),
#'   `Lab 2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0", 
#'                                "1/20/2023 10:00:00 AM", "1/20/2023 9:50:00 AM",
#'                                "1/20/2023 9:00:00 AM", "0", 
#'                                "1/20/2023 9:20:00 AM", "0"),
#'   `Lab 2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00", 
#'                                "0:00:00", "0:00:00", "0:00:00", "0:00:00"),
#'   
#'   `Lab 3` = c(0, 0, 0.9, 0.5, 1, 0.9, 1, 0.9),
#'   `Lab 3 - Max Points` = c(1, 1, 1, 1, 1, 1, 1, 1),
#'   `Lab 3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM", 
#'                                "1/21/2023 9:50:00 AM","1/21/2023 9:00:00 AM", 
#'                                "1/21/2023 9:30:00 AM", "1/21/2023 9:20:00 AM", 
#'                                "1/21/2023 9:45:00 AM"),
#'   `Lab 3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00", 
#'                                "0:00:00", "0:00:00", "0:00:00", "0:00:00"),
#'   
#'   `Project 1` = c(0.9, 0, 0.4, 0, 0.99, 0.9, 1, 0.9),
#'   `Project 1 - Max Points` = c(1, 1, 1, 1, 1, 1, 1, 1),
#'   `Project 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0", 
#'                                "1/22/2023 10:00:00 AM", "0", 
#'                                "1/22/2023 9:00:00 AM","1/22/2023 9:30:00 AM", 
#'                                "1/22/2023 9:20:00 AM", "1/22/2023 9:45:00 AM"),
#'   `Project 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00", 
#'                                "0:00:00", "0:00:00", "0:00:00", "0:00:00")
#' )
#' 
#' student_assignments_long_data <- pivot_gs(gs_data)
#' 
#' @importFrom tidyr pivot_longer 
#' @importFrom dplyr mutate_at vars contains
#' @importFrom tibble tibble
#' @export
#' 
pivot_gs <- function(gs_data, names_sep = " - "){
  #takes gradescope data - after it has been cleaned up - and transforms using pivot_longer
  #yields 8 columns:  "name", "section", "email", "sid", "assignments", 
  #                   "max points","submission time", "lateness (h:m:s)"
  id_cols <- get_id_cols(gs_data, FALSE)
  
  gs_data <- gs_data |> mutate_at(vars(contains("Lateness")), as.character)
  
  new_names <- ifelse(
    names(gs_data) %in% get_assignments(gs_data, FALSE), 
    paste0(names(gs_data), " - Score"), #identify score columns in column name
    names(gs_data)
  )
  
  names(gs_data) <- new_names
  
  gs_data |>
    tidyr::pivot_longer(
      cols = -all_of(id_cols), # change the unit of obs to student x assignment
      names_to = c("Assignments", ".value"),
      names_sep = names_sep
    )
}

