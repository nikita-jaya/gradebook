#' Process Gradescope .csv
#'
#' This function processes the gradescope data before graidng.  Ungraded assignments are dropped, and 
#' eventually, this will also merge duplicated students and deal with students with no SID.
#'
#' @param gs A gradescope dataframe with students as rows and assignment information across the columns.
#' @param drop_ungraded whether or not to drop ungraded assignments
#' @param verbose whether or not to print messages
#'
#' @return dataframe
#' @importFrom readr read_csv
#' @importFrom dplyr mutate across cur_column mutate_at vars all_of ends_with
#' @export
process_gs <- function(gs, drop_ungraded = TRUE, verbose = FALSE){
  if (drop_ungraded) {
    gs <- gs |>
      drop_ungraded_assignments(verbose = verbose)
  }
  gs
}

#' @importFrom dplyr filter select
#' @importFrom purrr keep
#' @importFrom cli cli_alert_info cli_div cli_text cli_end
drop_ungraded_assignments <- function(gs, verbose = FALSE) {
  # This functions drops any assignments that have no grades for any students and replaced -Inf values
  assignments <- get_assignments(gs)
  #These are the dropped assignments with all NAs for raw-score
  dropped <- gs |> purrr::keep(~all(is.na(.x))) |> names()
  dropped <- dropped[dropped %in% assignments]
  
  alert <- function() {
    cli::cli_div(theme = list(span.emph = list(color = "orange")))
    cli::cli_text("{.emph Important Message}")
    cli::cli_end()
    if (length(dropped) != 0) {
      cli::cli_alert_info("These are your ungraded assignments: {dropped}")
    } else {
      cli::cli_alert_info("These are no ungraded assignments")
    }
  }
  if (verbose){
    alert()
  }
  gs |> select(-contains(dropped))
}
