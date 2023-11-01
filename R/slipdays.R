#' Slipdays
#'
#'This function processes the slipdays for each student
#'
#' @param processed_data A dataframe (csv from Gradescope) that has already been pre-processed for grading
#'
#' @return A dataframe "processed_data_after_slipdays": A dataframe that has accounted slipdays in the lateness for all students
#' 
#'
#' @examples
#' # Example dataframe
#'processed_data <- tibble(
#'  `sid` = c(3032412514, 3032122516, 3032412516),
#'  `lab1` = c(1, 0, 0.9),
#'  `lab1_-_max_points` = c(1, 1, 1),
#'  `lab1_-_submission_time` = c("1/19/2023 9:25:00 AM", "1/19/2023 10:00:00 AM","1/19/2023 9:00:00 AM"),
#'  `lab1_-_lateness_(h_m_s)` = c("73:45:00", "0:00:00", "0:00:00"),
#'  
#'  `lab2` = c(1, 0, 0.9),
#'  `lab2_-_max_points` = c(1, 1, 1),
#'  `lab2_-_submission_time` = c("1/20/2023 9:25:00 AM", "1/20/2023 10:00:00 AM", "1/20/2023 9:50:00 AM"),
#'  `lab2_-_lateness_(h_m_s)` = c("3:55:00", "0:00:00", "0:00:00"),
#'  
#'  `lab3` = c(0, 0, 0.9, 0.5),
#'  `lab3_-_max_points` = c(1, 1, 1),
#'  `lab3_-_submission_time` = c("1/21/2023 10:00:00 AM", "1/21/2023 9:50:00 AM", "1/21/2023 9:00:00 AM"),
#'  `lab3_-_lateness_(h_m_s)` = c("0:00:30", "0:00:00", "0:00:00"),
#'  
#'  `project1` = c(0.9, 0, 0.4),
#'  `project1_-_max_points` = c(1, 1, 1),
#'  `project1_-_submission_time` = c("1/22/2023 9:25:00 AM", "1/22/2023 10:00:00 AM", "1/22/2023 9:00:00 AM"),
#'  `project1_-_lateness_(h_m_s)` = c("23:45:00", "0:00:30", "0:00:00")
#')
#'
#' 
#' @importFrom dplyr filter group_by ungroup arrange summarize everything n across group_split last mutate across everything
#' @importFrom tidyr drop_na
#' @importFrom purrr map_dfr
#' @export