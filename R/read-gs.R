#' Read Gradescope .csv
#'
#' This functions reads the Gradescope .csv, checks for correct format.
#'  
#'Now it is favored to use read_files. 
#'This function is left for backwards-compatibility.
#' @param path Path to Gradescope CSV
#' @param verbose whether or not to print messages
#'
#' @return dataframe
#' @importFrom readr read_csv
#' @importFrom dplyr mutate across cur_column mutate_at vars all_of ends_with
#' @export
read_gs <- function(path, verbose = FALSE){
  # read in csv
  gs <- read_csv(path, trim_ws = FALSE) |>
    #check format
    check_data_format()
}


#'This function will read in input data and combine in the appropriate manner.
#'@param grades_path Is the path to the csv containing graded assignments.
#'@param other_file_paths Is a list containing any other filepaths desired to be read in. Each name should be a designated type of file. For example, "lateness" and "roster" and the value should be the filepath.
#'@param source Determines how to read the grade file. By default, it is auto so that Gradebook can determine what format the file is in. Alternatively, users can specify "Gradescope" or "Canvas" to force certain interpretations.
#'@return A dataframe of grades. Later support for other files will be returned as a list as well.
#'@importFrom readr read_csv
#'@export
read_files <- function(grades_path, 
                       other_file_paths = list(), 
                       source = "auto"){
  #first read in all of the files
  
  #begininng with the grades csv
  
  tryCatch({
    grades <- readr::read_csv(grades_path, trim_ws = FALSE)
    },
    error = function(e){
      stop(
      paste0("Error when attempting to read grade csv file. 
           Check that the filepath is correct. Error: ",
             e)
      )
    }
  )
  
  #now read in other supplied data 
 for (data_type in names(other_file_paths)){
   warning(paste0(data_type, 
                  " is not currently supported for non-Gradescope sources."))
 }
  
  # now determine how to read in data
  
  auto_determine <- determine_grade_source(grades)
  
  if (source == "auto"){
    #fill in later
    
    if (auto_determine == "Gradescope"){
      attr(grades, "source") <- "Gradescope"
      
      return(check_data_format(grades))
    } else if (auto_determine == "Canvas"){
      #the read_canvas_grades does all of the lifting
      return(read_canvas_grades(grades))
    } else {
      stop("Data source is unrecognized.")
    }
    
  } else if (source == "Gradescope"){
    if (source != auto_determine){
      warning(paste0(
        "Grade data will be processed as ", source, 
        "data due to value of source parameter. ",
        "However, the data appears to be from source ",
        auto_determine, "."
      ))
    }
    
    attr(grades, "source") <- "Gradescope"
    
    return(check_data_format(grades))
      
  } else if (source == "Canvas"){
    if (source != auto_determine){
      warning(paste0(
        "Grade data will be processed as ", source, 
        "data due to value of source parameter. ",
        "However, the data appears to be from source ",
        auto_determine, "."
      ))
    }
    #the read_canvas_grades does all of the lifting
    return(read_canvas_grades(grades))
    
  } else {
    stop(paste0(
      "Specified source type ", source,
      " is not currently supported."
    ))
  }
  
  
 
}

#'
#'Takes in a dataframe uploaded from Canvas. 
#'Returns a dataframe in our standardized format (ie like Gradescope).
#'
#'
#'@importFrom stringr str_extract str_match
#'@importFrom dplyr filter select slice rename_with rename left_join
#'@importFrom purrr discard
read_canvas_grades <- function(grades){
  #first determine which columns are assignments
  
  #canvas categories will not be kept
  
  assignments <- stringr::str_extract(names(grades),
                                      ".+\\s*\\(\\d+\\)") |>
                purrr::discard(is.na)
  
 
  #now reconfigure the points possible info
  max_points <- dplyr::filter(grades, Student == "    Points Possible") |>
    dplyr::select(all_of(assignments)) |>
    dplyr::slice(rep.int(1, nrow(grades))) |>
    dplyr::rename_with(function(x){
      paste0(x, " - Max Points")
    })
  #add ID column to max points to facilitate matching
  max_points$ID <- grades$ID
  
  #remove unneeded cols, add max_point column, and remove rows that are not students
  grades <- grades |>
    dplyr::select(c(assignments, "ID", "Student", 
                    "SIS User ID", "Section")) |>
    dplyr::left_join(max_points, 
                     by = dplyr::join_by(ID == ID)) |>
    dplyr::filter(!(Student %in% c("    Points Possible", "Student, Test")))
  
  #add columns for submission time and lateness
  
  sub_cols <- paste0(assignments, " - Submission Time")
  
  late_cols <- paste0(assignments, " - Lateness (H:M:S)")
  
  grades[sub_cols] <- as.POSIXct(NA)
  
  grades[late_cols] <- NA
  
  grades$`First Name` <- (stringr::str_match(grades$Student, 
                                             ".+,(.+)"))[, 2]
  
  grades$`Last Name` <- (stringr::str_match(grades$Student, 
                                            "(.+),.+"))[, 2]
  
  
  grades <- grades |>
    dplyr::select(-c("Student", "ID")) |>
    dplyr::rename(SID = `SIS User ID`,
                  Sections = Section)
  
  attr(grades, "source") <- "Canvas"
  
  check_data_format(grades)
  
}

#'This function performs the auto-determination of where the grades dataframe was sourced.
#'@param grades_df This is the input Dataframe
#'@returns A string "Canvas", "Gradescope", or "Unrecognized" of the determination.
#'@export
determine_grade_source <- function(grades_df){
  
  columns <- names(grades_df)
  
 if (all(c("Student", "ID", "SIS User ID", "SIS Login ID") %in% columns)){
   return("Canvas")
 } else if (all(c("First Name", "Last Name", "SID", "Email") %in% columns)){
   return("Gradescope")
 } else {
   return("Unrecognized")
 }
  
}



#' Check Formatting of Grades Data
#'
#' This functions checks the column names throughout the Grades data.
#' There must be an SID column and at least one assignment.
#' It also gives an alert for what id cols and assignments are in the data.
#'
#' @param gs Gradescope data frame
#' @param verbose whether or not to print messages
#'
#' @return Same gs dataframe if no errors.
#' @export
check_data_format <- function(gs, verbose = FALSE){
  
  col_names <- colnames(gs)
  
  id_cols <- get_id_cols(gs, verbose = verbose)
  
  if ( !("SID" %in% id_cols) ){
    stop("There is no SID column")
  }
  
  assignment_names <- get_assignments(gs, verbose = verbose)
  
  if (is.null(assignment_names) | length(assignment_names) == 0){
    stop("There are no assignments in this dataframe")
  }
  
  #if correct format, return same dataframe
  return (gs)
}

#' Get the ID Columns for Gradescope Data
#'
#' This function identified the id columns from gradescope data
#'
#' @param gs  Gradescope dataframe
#' @param verbose whether or not to return an alert of assignments
#' 
#' @return a list of id columns 
#' @importFrom stringr str_replace_all regex
#' @importFrom cli cli_alert_info cli_div cli_text cli_end
#' @export

get_id_cols <- function(gs, verbose = FALSE) {
  #REGEX pattern: case INsensitive, then matches the extensions
  regex <- "(?i)( - max points| - submission time| - lateness \\(h:m:s\\))"
  
  # extract base names and excludes the extensions (max points, submission time and lateness)
  base_names <- stringr::str_replace_all(names(gs), regex, "")
  
  # Count occurrences of base names
  base_name_counts <- table(base_names)
  
  # identify base names that repeat exactly 4 times
  repeating <- names(base_name_counts[base_name_counts == 4])
  
  # identify columns to keep: those not repeating 4 times
  columns_to_keep <- names(gs)[!(base_names %in% repeating)]
  
  alert <- function() {
    cli::cli_div(theme = list(span.emph = list(color = "orange")))
    cli::cli_text("{.emph Important Message}")
    cli::cli_end()
    cli::cli_alert_info("The ID columns from Gradescope are {columns_to_keep}")
  }
  
  if (verbose){
    alert()
  }
  
  return(columns_to_keep)
}

#' Get the Assignment Names for Gradescope Data
#'
#' This function identified the assignments from Gradescope data
#'
#' @param gs unprocessed Gradescope dataframe
#' @param verbose whether or not to print assignment names
#' 
#' @return vector 
#' @importFrom stringr str_replace_all regex
#' @importFrom cli cli_alert_info cli_div cli_text cli_end
#' @export

get_assignments <- function(gs, verbose = FALSE){
  #REGEX pattern: case INsensitive, then matches the extensions
  #works with untouched GS dataframe so we can match the pattern
  regex = "(?i)( - max points| - submission time| - lateness \\(h:m:s\\))"
  
  # extract base names and excludes the extensions (max points, submission time and lateness)
  base_names <- stringr::str_replace_all(names(gs), regex, "")
  
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
  
  if (verbose){
    alert()
  }
  
  return (assignment_names)
}