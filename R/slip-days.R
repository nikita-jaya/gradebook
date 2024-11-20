#' Apply Slip Days
#' 
#' This function applies any slip day policies within the policy file. 
#'
#' @param gs A Gradescope dataframe
#' @param policy A grading policy file as an R list
#'
#' @examples
#' apply_slip_days(gs = gs_demo, policy = slip_days_policy_demo)
#' 
#' @return A dataframe of the original Gradescope data with lateness updated according to the slip day policies 
#'
#' 
#' @export
apply_slip_days <- function(gs, policy){
  # if no slip day policies, return gs
  if (is.null(policy$slip_days)){
    return (gs)
  }
  # iterate through all slip day policies
  for (policy_item in policy$slip_days){
    
    # put assignments in chronological order
    # save back into policy item
    policy_item$assignments <- order_assignments(gs, policy_item)
    
    # apply slip days
    gs <- calculate_slip_days(gs, policy_item)
  }
  gs
}

#' @importFrom dplyr select mutate_at mutate group_by summarize arrange pull
#' @importFrom tidyr pivot_longer
order_assignments <- function(gs, policy_item){
  submission_cols <- paste0(policy_item$assignments, " - Submission Time")
  # chronological order of assignments is determined by 
  # median submission time of each assignment
  gs |>
    dplyr::select(all_of(submission_cols)) |>
    dplyr::mutate_at(submission_cols, as.POSIXct, format = "%m/%d/%Y %H:%M") |>
    tidyr::pivot_longer(
      everything(),
      names_to = "Assignment",
      values_to = "Submission Time"
    ) |>
    dplyr::mutate(Assignment = stringr::str_remove(Assignment, " - Submission Time")) |>
    dplyr::group_by(Assignment) |>
    dplyr::summarize(`Submission Median` = median(`Submission Time`)) |>
    dplyr::arrange(`Submission Median`) |>
    dplyr::pull(`Assignment`)
}

#' @importFrom dplyr case_when
calculate_slip_days <- function(gs, policy_item){
  gs[policy_item$name] <- policy_item$num_slip_days
  late_cols <- paste0(policy_item$assignments, " - Lateness (H:M:S)")
  for (lateness in late_cols){
    # difference = slip days - lateness
    decrease_lateness <- decrease_lateness(gs[[lateness]], gs[[policy_item$name]])
    difference <- gs[[policy_item$name]] - round_to_days(gs[[lateness]])
    #decrease lateness
    gs[lateness] <- dplyr::case_when(
      # if lateness > slipdays
      decrease_lateness > 0 ~ decrease_lateness,
      #if no lateness or slip days reduces all lateness
      .default = "00:00:00"
    )
    
    # decrement slip days counter
    gs[policy_item$name] <- dplyr::case_when(
      difference >= 0 ~ difference,
      .default = 0
    )
  }
  gs
}

#' @importFrom hms hms
decrease_lateness <- function(lateness_col, slipdays_col){
  hms::hms(convert_to_min(lateness_col) - slipdays_col*24*60) |>
    as.character()
}

round_to_days <- function(lateness){
  days <- convert_to_min(lateness)/(24*60)
  ceiling(days)
}
