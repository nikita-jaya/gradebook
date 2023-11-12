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

