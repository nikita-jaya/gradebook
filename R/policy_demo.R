#' Policy Demo File
#'
#' An example policy file that mimics a hypothetical class syllabus. Note that this is a very minimal policy file,
#' and information about more specific grading criteria can be found via the vignettes.
#' 
#'
#' @format A list of two nested lists
#' \describe{
#'   \item{COURSEWIDE}{First nested list with general information about the class}
#'   \item{course_name}{Name of the course, saved as a string}
#'   \item{description}{Any relevant course description, purely for the user, also saved as a single string}
#'   \item{CATEGORIES}{Second nested list where syllabus is broken down into categories with their respective grading criteria}
#'   \item{category}{Category name}
#'   \item{aggregation}{How assignment scores are aggregated to calculate category score}
#'   \item{assignments}{List of assignment names that fall into this category}
#'   ...
#' }
#' @examples
#' policy_demo
#' 
"policy_demo"