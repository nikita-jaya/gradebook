#' Process Student IDs
#'
#' This function processes a dataset with student IDs. It handles 
#' erroneous student IDs by filtering out duplicates and handling NA values. 
#' The function returns a list containing two dataframes: 
#' 1) "unique_sids" which has unique student IDs and 
#' 2) "duplicates" which contains all rows with duplicate or NA student IDs.
#'
#' @param gs_data A dataframe (csv from Gradescope) containing a column named "sid" which holds student IDs.
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
#' @importFrom dplyr filter group_by ungroup arrange summarize everything n across
#' @importFrom tidyr drop_na
#' @export

#this could be a good workflow for a wrapper function
process_student_id <- function(gs_data) {
  
  
    gs2 <- gs_data |>
      #drop NA ids
        tidyr::drop_na(sid) |>
      #groupd by id
        dplyr::group_by(sid) |>
      #merge by id - apply function 
        dplyr::summarize(merge_replicated_records())
    
    return(gs2)
}

merge_replicated_records <- function(single_sid_df) {

    if (nrow(single_sid_df) == 1) {
        return(single_sid_df)
        } else {
        single_sid_df |>
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
          return(single_sid_df)
        }
    }

#get only duplicates
make_duplicate_sid_df <- function(gs_data) {
  gs_data |>
    dplyr::group_by(sid) |>
    dplyr::mutate(n_records = n()) |>
    dplyr::filter(n_records > 1) #|>
  dplyr::distinct() |>
    dplyr::select(-n_records)
}


# 
# 
# process_student_id <- function(gs_data) {
# #  .data <- NULL
# #  sid <- NULL
# 
#   
#   #old workflow
#   # filter NAs
#   #get duplicate rows
#   #get duplicates and NAs in a df
#   # get a df with unique sids
#   #only duplicates df to do merging
#   #merging
#   #combine dataframes into 1 correct dataframe of students
#   
#   df_filtered <- gs_data |>
#       tidyr::drop_na(sid)
# 
#   duplicated_rows <- df_filtered |>
#     group_by(sid) |>
#     filter(n() > 1) |>
#     ungroup()
# 
#   duplicates_df <- gs_data |>
#     filter(is.na(.data$sid) | (.data$sid %in% duplicated_rows$.data$sid)) |>
#     arrange(.data$sid)
# 
#   #df with only unique sid
#   unique_sids <- new_data[!(new_data$sid %in% duplicates_df$.data$sid), ]
# 
#   #combined df with unique and na in sid
#   unique_and_na_sids <- rbind(unique_sids, filter(duplicates_df, is.na(.data$sid)))
# 
#   #df with duplicate sid so we can do merging using only this small df
#   duplicate_sids_without_na <- filter(duplicates_df, !is.na(.data$sid))
# 
# 
# 
#     #combine dataframes into 1 correct dataframe of students
#     result <- rbind(unique_and_na_sids, data_uniquesids)
#   } else {
#     result <- new_data
#   }
# 
#   return(list(unique_student_ids = result, duplicates = duplicates_df))
# }
