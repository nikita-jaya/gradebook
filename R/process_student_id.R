#' Process Student IDs
#'
#' This function processes a dataset with student IDs (sids). It handles 
#' erroneous student IDs by filtering out duplicates and handling NA values. 
#' The function returns a list containing two dataframes: 
#' 1) "unique_sids" which has unique student IDs and 
#' 2) "duplicates" which contains all rows with duplicate or NA student IDs.
#'
#' @param new_data A dataframe containing a column named "sid" which holds student IDs.
#'
#' @return A list with two dataframes:
#'   - "unique_sids": A dataframe containing unique student IDs.
#'   - "duplicates": A dataframe with rows having duplicate or NA student IDs.
#'
#' @examples
#' # Example dataframe
#' data <- data.frame(
#'   sid = c(3032412514, NA, 3032412516,
#'           3032412517, 3032412518, 3032412519, 3032412520, 3032412521),
#'
#'   name = c("John Smith", "Jane Doe", "Robert Brown", "Emily Johnson",
#'            "Michael Davis", "Linda Wilson", "James Taylor", "Patricia Anderson"),
#'   email = c("john.smith@berkeley.edu", "jane.doe@berkeley.edu", "robert.brown@berkeley.edu",
#'             "emily.johnson@berkeley.edu", "michael.davis@berkeley.edu",
#'             "linda.wilson@berkeley.edu", "james.taylor@berkeley.edu",
#'             "patricia.anderson@berkeley.edu"
#'             )
#'  )
#' processed_data <- process_student_id(data)
#' 
#' @importFrom dplyr %>% filter group_by ungroup arrange summarize everything n across
#' @export

process_student_id <- function(new_data) {
  .data <- NULL
  sid <- NULL
  
  df_filtered <- new_data %>% filter(!is.na(.data$sid))
  
  duplicated_rows <- df_filtered %>%
    group_by(.data$sid) %>%
    filter(n() > 1) %>%
    ungroup()
  
  duplicates_df <- new_data %>%
    filter(is.na(.data$sid) | (.data$sid %in% duplicated_rows$.data$sid)) %>%
    arrange(.data$sid)
  
  #df with only unique sid
  unique_sids <- new_data[!(new_data$sid %in% duplicates_df$.data$sid), ]
  
  #combined df with unique and na in sid
  unique_and_na_sids <- rbind(unique_sids, filter(duplicates_df, is.na(.data$sid)))
  
  #df with duplicate sid so we can do merging using only this small df
  duplicate_sids_without_na <- filter(duplicates_df, !is.na(.data$sid))
  
  if (nrow(duplicate_sids_without_na) > 0) {
    data_uniquesids <- duplicate_sids_without_na %>%
      group_by(.data$sid) %>%
      summarize(across(
        everything(),
        ~ if (inherits(., "Period")) {
          last(na.omit(.))
        } else if (is.numeric(.)) {
          max(., na.rm = TRUE)
        } else {
          if (all(is.na(.))) {
            NA
          } else {
            last(na.omit(.))
          }
        }
      )) %>%
      ungroup()
    
    #combine dataframes into 1 correct dataframe of students
    result <- rbind(unique_and_na_sids, data_uniquesids)
  } else {
    result <- new_data
  }
  
  return(list(unique_student_ids = result, duplicates = duplicates_df))
}
