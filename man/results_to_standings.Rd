% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/results_to_standings.R
\name{results_to_standings}
\alias{results_to_standings}
\title{Convert Match Results to League Standings}
\usage{
results_to_standings(results, deductions = NULL, xTable = F)
}
\arguments{
\item{results}{A data frame containing match results.}

\item{deductions}{A data frame containing points deductions for teams (default is NULL).}

\item{xTable}{A logical indicating whether to include expected table (xTable) in standings (default is FALSE).}
}
\value{
A data frame containing league standings with columns for rank, team, matches played, wins, draws, losses, goals for, goals against, goal difference, points, expected goals, expected goals against, expected goal difference
}
\description{
This function processes match results to generate league standings, including the option to create an expected table based on expected goals (xG).
}
\details{
This function calculates the league standings based on actual match results and, optionally, based on expected goals. It handles points deductions and ranks teams accordingly.
}
\examples{
\dontrun{
  results <- data.frame(...) # Your match results data here
  deductions <- data.frame(...) # Your points deductions data here
  standings <- results_to_standings(results, deductions, xTable = TRUE)
}

}
