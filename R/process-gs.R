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

drop_ungraded_assignments <- function(gs, verbose = verbose){
  gs #needs to be written
}