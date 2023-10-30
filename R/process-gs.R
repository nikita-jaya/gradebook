
#' Convert GS data to longer table
#'
#' This function takes a gradescope dataframe after it has been processed
#'  to remove duplicate students or invalid IDs
#' and it applies pivot longer function to allow for unit of observation be a student + individual assignment
#'
#' @param processed_data A dataframe (csv from Gradescope) containing a column named "sid" which holds student IDs.
#'
#' @return A dataframe "student_assignments_long": A dataframe with a unit of observation a student + each assignment.
#'
#' @examples
#' # Example
#' processed_data <- tibble::tibble(
#'   `sid` = c(3032412514, 3032122516, 3032412516,
#'             3032412517, 3032412518, 3032412519,
#'             3032412521, 3032412521),
#'   `sections` = c("Stat20", "Stat20", "Stat20", "Stat20",
#'                 "Stat20", "Stat20", "Stat20", "Stat20"),
#'   
#'   `names` = c("John Smith", "Jane Doe", "Robert Brown", "Emily Johnson",
#'              "Michael Davis", "Linda Wilson", "James Taylor", "Patricia Anderson"),
#'   `email` = c("john.smith@berkeley.edu", "jane.doe@berkeley.edu",
#'               "robert.brown@berkeley.edu", "emily.johnson@berkeley.edu",
#'               "michael.davis@berkeley.edu", "linda.wilson@berkeley.edu",
#'               "james.taylor@berkeley.edu", "patricia.anderson@berkeley.edu"),
#'   `lab1_-_Raw_Score` = c(1, 0, 0.9, 0.5, 1, 0.9, 1, 0.8),
#'   `lab1_-_Max_Points` = c(1, 1, 1, 1, 1, 1, 1, 1),
#'   `lab1_-_Submission_Time` = c("1/19/2023 9:25:00 AM", "0", "1/19/2023 10:00:00 AM", "0",
#'                                "1/19/2023 9:00:00 AM", "1/19/2023 9:30:00 AM", "1/19/2023 9:20:00 AM", "1/19/2023 9:15:00 AM"),
#'   `lab1_-_Lateness_(H_M_S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00",
#'                                 "0:00:00", "0:00:00", "0:00:00", "0:00:00"),
#'   
#'   `lab2_-_Raw_Score` = c(1, 0, 0.9, 0.5, 1, 0.9, 1, 0.9),
#'   `lab2_-_Max_Points` = c(1, 1, 1, 1, 1, 1, 1, 1),
#'   `lab2_-_Submission_Time` = c("1/20/2023 9:25:00 AM", "0", "1/20/2023 10:00:00 AM", "1/20/2023 9:50:00 AM",
#'                                "1/20/2023 9:00:00 AM", "0", "1/20/2023 9:20:00 AM", "1/20/2023 9:30:00 AM"),
#'   `lab2_-_Lateness_(H_M_S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00", "0:00:00", "0:00:00", "0:00:00", "0:00:00"),
#'   
#'   `lab3_-_Raw_Score` = c(0, 0, 0.9, 0.5, 1, 0.9, 1, 0.9),
#'   `lab3_-_Max_Points` = c(1, 1, 1, 1, 1, 1, 1, 1),
#'   `lab3_-_Submission_Time` = c("0", "0", "1/21/2023 10:00:00 AM", "1/21/2023 9:50:00 AM", "1/21/2023 9:00:00 AM", "1/21/2023 9:30:00 AM", "1/21/2023 9:20:00 AM", "1/21/2023 9:45:00 AM"),
#'   `lab3_-_Lateness_(H_M_S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00", "0:00:00", "0:00:00", "0:00:00", "0:00:00"),
#'   
#'   `project1_-_Raw_Score` = c(0.9, 0, 0.4, 0, 0.99, 0.9, 1, 0.9),
#'   `project1_-_Max_Points` = c(1, 1, 1, 1, 1, 1, 1, 1),
#'   `project1_-_Submission_Time` = c("1/22/2023 9:25:00 AM", "0", "1/22/2023 10:00:00 AM", "0", "1/22/2023 9:00:00 AM", "1/22/2023 9:30:00 AM", "1/22/2023 9:20:00 AM", "1/22/2023 9:45:00 AM"),
#'   `project1_-_Lateness_(H_M_S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00", "0:00:00", "0:00:00", "0:00:00", "0:00:00")
#' )
#' 
#' student_assignments_long_data <- pivot_gs(processed_data)
#' 
#' @importFrom tidyr pivot_longer replace_na 
#' @importFrom dplyr mutate_at vars
#' @importFrom tibble as_tibble

#' @export
#' 
pivot_gs <- function(processed_data, names_sep = "_-_"){
#takes gradescope data - after it has been cleaned up - and transforms using pivot_longer
#yields 8 columns:  "name", "section", "email", "sid", "assignments", 
#                   "max points","submission time", "lateness (h:m:s)"
  
    
    processed_data <- processed_data |> mutate_at(vars(contains("lateness")), as.character)
  #get_id_cols(): extracts the col names
  id_cols <- get_id_cols(processed_data)
  
  sxa <- processed_data |>
    tidyr::pivot_longer(
                  cols = -all_of(id_cols), # change the unit of obs to student x assignment
                  names_to = c("assignments", ".value"),
                  names_sep = names_sep
      )
  
  return(sxa)
}


#' Remove white spaces and special characters from column names
#' 
#' This function takes a gradescope dataframe and
#' it removes white spaces and special characters from column names
#
#' @param processed_data A dataframe (csv from Gradescope)
#'
#' @return A dataframe "processed_data": Returns the same dataframe with modified column names.
#'
#' @examples
#' 
#' #gradebook gradescope demo data:
#' gs_demo
#' processed_assignments_df <- process_assignments(gs_demo)
#' 
#' @importFrom stringr str_replace_all 
#' @importFrom tidyr pivot_longer replace_na
#' @importFrom tibble as_tibble

#' @export
process_assignments <- function(processed_data){
  old_names <- colnames(processed_data)
  # Replace whitespace/special characters
  new_names <- old_names |>
    str_replace_all("[\\s:]+", "_")
  new_names <- str_replace_all(new_names, ",", ":")
  
  # Append " - raw_score" to specific column names
  new_names <- ifelse(
    !stringr::str_detect(new_names, stringr::regex("name|section|max|time|lateness|email|sid", ignore_case = TRUE)), 
    paste0(new_names, "_-_raw_score"),
    new_names
  )
  
  # Assigning the new column names back to processed_data
  colnames(processed_data) <- new_names
  
  return(processed_data)
}




#' Get the ID Columns for Gradescope Data
#'
#' This function identified the id columns from gradescope data
#'
#' @param gs_data Gradescope dataframe
#'
#' @examples
#' df <- tibble::tibble(
#' name = 'name',
#' email = 'email',
#' sid = '343432',
#' `PS 1_-_Raw_Score` = 1,
#' `PS 1_-_Max_points` = 1,
#' `PS 1_-_Submission_time` = '1/21/2023 10:00:00 AM',
#' `PS 1-_-Lateness_(h_m_s)` = '0:00:00'
#' )
#' 
#' @return a list of id columns
#' @importFrom stringr str_replace_all regex
#' @importFrom cli cli_alert_info
#' @export
#' 
#' 
get_id_cols <- function(df){
  #REGEX pattern: case INsensitive, then matches the extensions
  #works with untouched GS dataframe so we can match the pattern
  regex = "(?i)(_-_raw_score|_-_max_points|_-_submission_time|_-_lateness_\\(h_m_s\\))"
  
  # extract base names and excludes the extensions (max points, submission time and lateness)
  base_names <- stringr::str_replace_all(names(df),regex, "")
  
  # Count occurrences of base names
  base_name_counts <- table(base_names)
  
  # identify base names that repeat exactly 4 times
  repeating <- names(base_name_counts[base_name_counts == 4])
  
  # identify columns to keep: those not repeating 4 times
  columns_to_keep <- names(df)[!(base_names %in% repeating)]
  
  alert <- function() {
    cli::cli_div(theme = list(span.emph = list(color = "orange")))
    cli::cli_text("{.emph Important Message}")
    cli::cli_end()
    cli::cli_alert_info("The ID columns from Gradescope are {columns_to_keep}")
  }
  alert()
  
  
  
  return(columns_to_keep)
}
#' Get the ID Columns for Unprocessed Gradescope Data
#'
#' This function identified the id columns from unprocessed gradescope data
#'
#' @param gs_data unprocessed Gradescope dataframe
#' 
#' @return a list of unprocessed id columns 
#' @importFrom stringr str_replace_all regex
#' @importFrom cli cli_alert_info
#' @export
#' 
#' 
get_id_cols_unprocessed_data <- function(df, give_alert = TRUE){
    #REGEX pattern: case INsensitive, then matches the extensions
    #works with untouched GS dataframe so we can match the pattern
    regex = "(?i)( - max points| - submission time| - lateness \\(h:m:s\\))"
    
    # extract base names and excludes the extensions (max points, submission time and lateness)
    base_names <- stringr::str_replace_all(names(df),regex, "")
    
    # Count occurrences of base names
    base_name_counts <- table(base_names)
    
    # identify base names that repeat exactly 4 times
    repeating <- names(base_name_counts[base_name_counts == 4])
    
    # identify columns to keep: those not repeating 4 times
    columns_to_keep <- names(df)[!(base_names %in% repeating)]
    
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

#' Get the Assignment Names for Unprocessed Gradescope Data
#'
#' This function identified the assignments from unprocessed gradescope data
#'
#' @param gs_data unprocessed Gradescope dataframe
#' 
#' @return a list of assignments 
#' @importFrom stringr str_replace_all regex
#' @importFrom cli cli_alert_info
#' @export
#' 
#' 
get_assignments_unprocessed_data <- function(df, give_alert = TRUE){
    #REGEX pattern: case INsensitive, then matches the extensions
    #works with untouched GS dataframe so we can match the pattern
    regex = "(?i)( - max points| - submission time| - lateness \\(h:m:s\\))"
    
    # extract base names and excludes the extensions (max points, submission time and lateness)
    base_names <- stringr::str_replace_all(names(df),regex, "")
    
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

#' Process Gradescope Data
#' 
#' This function takes a gradescope dataframe and processes it completely.
#
#' @param gs_data A dataframe (csv directly from Gradescope)
#' @param wide_format if TRUE, keeps in wide format; if FALSE, pivots data
#'
#' @return Returns processed Gradescope data in wide or pivotted format
#'
#' @export
process_gs_data <- function(gs_data, wide_format = TRUE){
    process <- gs_data |>
        lower_colnames() |>
        check_data_colnames_format() |>
        drop_ungraded_assignments() |>
        process_id() |>
        process_assignments()
    
    if (!wide_format){
        process <- process |> pivot_gs()
    }
    return (process)
}
