% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{student}
\alias{student}
\alias{student_subset_2000}
\alias{student_subset_2003}
\alias{student_subset_2006}
\alias{student_subset_2009}
\alias{student_subset_2012}
\alias{student_subset_2015}
\alias{student_subset_2018}
\alias{student_subset_2022}
\title{Processed and Sampled PISA Student Data (2000-2022)}
\format{
A tibble of the following variables
\itemize{
    \item \code{year}: Year of the PISA data. Integer.
    \item \code{country}: Country 3 character code. Note that some regions/territories are coded as "country" for ease of input. Factor.
    \item \code{school_id}: Unique school identifier for each country and year. Character.
    \item \code{student_id}: Unique student identifier within each school. Integer.
    \item \code{mother_educ}: Mother's highest level of education, from "less than ISCED1" to "ISCED 3A". Factor.
    \item \code{father_educ}: Father's highest level of education, from "less than ISCED1" to "ISCED 3A". Factor.
    \item \code{gender}: Gender of the student. Only "male" and "female" are recorded. Factor.
    Note that we call this variable gender and not sex as this term was used in the OECD PISA database.
    \item \code{computer}: Possession of computer. Only "yes" and "no" are recorded. Factor.
    \item \code{internet}: Access to internet. Only "yes" and "no" are recorded. Factor.
    \item \code{math}: Simulated score in mathematics. Numeric.
    \item \code{read}: Simulated score in reading. Numeric.
    \item \code{science}: Simulated score in science. Numeric.
    \item \code{stu_wgt}: The final survey weight score for the student score. Numeric.
    \item \code{desk}: Possession of desk to study at. Only "yes" and "no" are recorded. Factor.
    \item \code{room}: Possession of a room of your own. Only "yes" and "no" are recorded. Factor.
    \item \code{dishwasher}: Possession of a dishwasher. Only "yes" and "no" are recorded. Factor.
    Note that in 2015 and 2018, all entries are missing.
    \item \code{television}: Number of televisions.
    "0", "1", "2" are code for no, one and two TVs in the house. "3+" codes for three or more TVs. Factor.
    Note that in 2003, all entries are missing.
    \item \code{computer_n}: Number of computers.
    "0", "1", "2" are code for no, one and two computers in the house. "3+" codes for three or more computers. Factor.
    Note that in 2003, all entries are missing.
    \item \code{car}: Number of cars.
    "0", "1", "2" are code for no, one and two cars in the house. "3+" codes for three or more cars Factor.
    Note that in 2003, all entries are missing.
    \item \code{book}: Number of books. Factor.
    Note that encoding is different in the years 2000 and 2003 compared to all other years. Factor.
    Evaluate \code{table(student$book, student$year)} for a demo.
    \item \code{wealth}: Index of family wealth. Numeric.
    Note that in 2003, all entries are missing.
    \item \code{escs}: Index of economic, social and cultural status. Numeric.
    }
}
\description{
This dataset provides a clean and processed subset of the OECD PISA student data
for the years 2000-2022. The original data is sourced from
\url{https://www.oecd.org/en/about/programmes/pisa/pisa-data.html} and has been prepared for analysis.
A sampling of 50 students per country (for OECD countries) has been included for each year.
The data curation and sampling process are documented in \url{https://github.com/kevinwang09/learningtower_masonry/blob/master/Code/student_bind_rows.Rmd}
}
\examples{
library(dplyr)
data(student_subset_2000)
data(student_subset_2003)
dplyr::bind_rows(
student_subset_2000,
student_subset_2003
)
}
