% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read-gs.R
\name{read_gs}
\alias{read_gs}
\title{Read Gradescope CSV File}
\usage{
read_gs(path, verbose = FALSE)
}
\arguments{
\item{path}{Path to Gradescope CSV file}

\item{verbose}{Whether or not to print messages}
}
\value{
A dataframe of Gradescope CSV file, if no errors
}
\description{
This functions reads the Gradescope CSV file and checks for correct Gradescope
format. Each assignment should follow the four-column format:
\verb{Assignment Name}, \verb{Assignment Name - Max Points}, \verb{Assignment Name - Submission Time},
\verb{Assignment Name - Lateness (H:M:S)}. All other columns are designed as ID columns.
}
\examples{
path <- system.file("extdata", "gs_demo.csv", package = "gradebook")
read_gs(path = path)
}
