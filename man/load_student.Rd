% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_student.R
\name{load_student}
\alias{load_student}
\title{load_student() function allows the user to extract the PISA student scores for any desired year
from 2000-2018}
\usage{
load_student(year = "2000")
}
\arguments{
\item{year}{is the required parameter for the function to display the
dataset the user wants to view the PISA scores for the selected year else the entire student
data will be available to the user}
}
\value{
A dataset of PISA scores of students that took the test in the selected year as per user
from the years 2000-2018
}
\description{
load_student() function was created to extract the data of student's scores in any
years from 2000-2018, the function requires any of the year as it argument or a string "all"
that will return all the PISA scores of the students from the years 2000-2018.
}
\examples{
\dontrun{
library(learningtower)
student_all <- load_student("all")
student_2000 <- load_student("2000")
}

}
