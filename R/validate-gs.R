#' Process Student IDs
#'
#' This function processes a dataset with student IDs. It handles 
#' erroneous student IDs by filtering out duplicates and handling NA values. 
#' The function returns a dataframe with unique student IDs  
#'
#' @param gs_data A dataframe (csv from Gradescope) containing a column named "sid" which holds student IDs.
#'
#' @return A dataframe "unique_ids": A dataframe containing unique student IDs.
#' 
#'
#' @examples
#' # Example dataframe
#' data <- data.frame(
#'   SID = c(3032412514, NA, 3032412516,
#'           3032412517, 3032412518, 3032412519, 3032412520, 3032412521, 3032412521),
#'
#'   name = c("John Smith", "Jane Doe", "Robert Brown", "Emily Johnson",
#'            "Michael Davis", "Linda Wilson", "James Taylor", "Patricia Anderson", "Patricia Anderson"),
#'   email = c("john.smith@berkeley.edu", "jane.doe@berkeley.edu", "robert.brown@berkeley.edu",
#'             "emily.johnson@berkeley.edu", "michael.davis@berkeley.edu",
#'             "linda.wilson@berkeley.edu", "james.taylor@berkeley.edu",
#'             "patricia.anderson@berkeley.edu", "patricia.anderson@berkeley.edu"
#'             )
#'  )
#' processed_data <- process_id(data)
#' 
#' @importFrom dplyr filter group_by ungroup arrange summarize everything n across group_split last
#' @importFrom tidyr drop_na
#' @importFrom purrr map_dfr
#' @export

process_id <- function(gs_data) {
  
  colnames(gs_data) <- tolower(colnames(gs_data))
  
  # First, remove NAs and group by sid
  grouped_data <- gs_data |>
    tidyr::drop_na(sid) |>
    dplyr::group_by(sid)
  
  # Then, split the data by group and apply merge_replicated_records
  unique_ids <- grouped_data |>
    dplyr::group_split() |>
    purrr::map_dfr(merge_replicated_records)
  
  return(unique_ids)
}

#get only duplicates
get_duplicate_ids <- function(gs_data) {
  colnames(gs_data) <= tolower(colnames(gs_data))
  gs_data |>
    dplyr::group_by(sid) |>
    dplyr::mutate(n_records = n()) |>
    dplyr::filter(n_records > 1) |>
    dplyr::distinct() |>
    dplyr::select(-n_records)
}

# internal function which merges replicated rows by several rules:
# 1) if cell is a datetime, it removes NAs and takes the last datetime
# 2) if cell is numeric, it takes the max
# 3) if cell is NA, leave as NA
merge_replicated_records <- function(single_sid_df) {
  
  if (nrow(single_sid_df) == 1) {
    return(single_sid_df)
  } else {
    new_id <- single_sid_df |>
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
      ))
    return(new_id)
  }
}

#' @export
check_colnames <- function(processed_data) {
    colnames(processed_data) <- tolower(colnames(processed_data))
    return (processed_data)
}
