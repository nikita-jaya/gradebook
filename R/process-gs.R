
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
#'   `section` = c("Stat20", "Stat20", "Stat20", "Stat20",
#'                 "Stat20", "Stat20", "Stat20", "Stat20"),
#'   
#'   `name` = c("John Smith", "Jane Doe", "Robert Brown", "Emily Johnson",
#'              "Michael Davis", "Linda Wilson", "James Taylor", "Patricia Anderson"),
#'   `email` = c("john.smith@berkeley.edu", "jane.doe@berkeley.edu",
#'               "robert.brown@berkeley.edu", "emily.johnson@berkeley.edu",
#'               "michael.davis@berkeley.edu", "linda.wilson@berkeley.edu",
#'               "james.taylor@berkeley.edu", "patricia.anderson@berkeley.edu"),
#'   `lab1` = c(1, 0, 0.9, 0.5, 1, 0.9, 1, 0.8),
#'   `lab1_-_Max Points` = c(1, 1, 1, 1, 1, 1, 1, 1),
#'   `lab1_-_Submission Time` = c("1/19/2023 9:25:00 AM", "0", "1/19/2023 10:00:00 AM", "0",
#'                                "1/19/2023 9:00:00 AM", "1/19/2023 9:30:00 AM", "1/19/2023 9:20:00 AM", "1/19/2023 9:15:00 AM"),
#'   `lab1_-_Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00",
#'                                 "0:00:00", "0:00:00", "0:00:00", "0:00:00"),
#'   
#'   `lab2` = c(1, 0, 0.9, 0.5, 1, 0.9, 1, 0.9),
#'   `lab2_-_Max Points` = c(1, 1, 1, 1, 1, 1, 1, 1),
#'   `lab2_-_Submission Time` = c("1/20/2023 9:25:00 AM", "0", "1/20/2023 10:00:00 AM", "1/20/2023 9:50:00 AM",
#'                                "1/20/2023 9:00:00 AM", "0", "1/20/2023 9:20:00 AM", "1/20/2023 9:30:00 AM"),
#'   `lab2_-_Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00", "0:00:00", "0:00:00", "0:00:00", "0:00:00"),
#'   
#'   `lab3` = c(0, 0, 0.9, 0.5, 1, 0.9, 1, 0.9),
#'   `lab3_-_Max Points` = c(1, 1, 1, 1, 1, 1, 1, 1),
#'   `lab3_-_Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM", "1/21/2023 9:50:00 AM", "1/21/2023 9:00:00 AM", "1/21/2023 9:30:00 AM", "1/21/2023 9:20:00 AM", "1/21/2023 9:45:00 AM"),
#'   `lab3_-_Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00", "0:00:00", "0:00:00", "0:00:00", "0:00:00"),
#'   
#'   `project1` = c(0.9, 0, 0.4, 0, 0.99, 0.9, 1, 0.9),
#'   `project1_-_Max Points` = c(1, 1, 1, 1, 1, 1, 1, 1),
#'   `project1_-_Submission Time` = c("1/22/2023 9:25:00 AM", "0", "1/22/2023 10:00:00 AM", "0", "1/22/2023 9:00:00 AM", "1/22/2023 9:30:00 AM", "1/22/2023 9:20:00 AM", "1/22/2023 9:45:00 AM"),
#'   `project1_-_Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00", "0:00:00", "0:00:00", "0:00:00", "0:00:00")
#' )
#' 
#' student_assignments_long_data <- pivot_gs(processed_data)
#' 
#' @importFrom tidyr pivot_longer replace_na
#' @importFrom tibble as_tibble

#' @export
#' 
pivot_gs <- function(processed_data, names_sep = "_-_"){
#takes gradescope data - after it has been cleaned up - and transforms using pivot_longer
#yields 8 columns:  "name", "section", "email", "sid", "assignments", 
#                   "max points","submission time", "lateness (h:m:s)"
  
  sxa <- processed_data |>
    tidyr::pivot_longer(
                  cols = -all_of(c("names", "section","email","sid")), # change the unit of obs to student x assignment
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
  
  new_names <- old_names |>
    str_replace_all("[\\s:]+", "_")  # Replace whitespace/special characters
  
  # Replace all "," with ":" in the colnames to avoid 
  # issues with selecting these assignments in creating categories.
  new_names <- str_replace_all(new_names, ",", ":")
  
  # Assigning the new column names back to processed_data
  colnames(processed_data) <- new_names
  
  return(processed_data)
}





# if (!is.null(assignments_dataframe)) {
#   assignments_dataframe$new_colnames <- str_replace_all(assignments_dataframe$new_colnames, "_-_raw_points", "")
# }

# add_categories_to_pivot <- sxa %>%
#   left_join(assignments_dataframe %>% select(new_colnames, colnames, category), by = c("assignments" = "new_colnames"))
# colnames(add_categories_to_pivot)[colnames(add_categories_to_pivot) == "lateness_(h_m_s)"] ="lateness_min"
# 
# #cat_table is the list with categories (policy$categories) which is converted to a data frame categories_df in the server.
# x <- length(cat_table)
# if (x > 0){
#   #remove assigns column from cat_table
#   cat_table <- select(cat_table, -assigns)
#   add_categories_to_pivot <- add_categories_to_pivot %>%
#     left_join(cat_table, by = c("category" = "name"))
# }

