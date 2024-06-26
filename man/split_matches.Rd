% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/split_matches.R
\name{split_matches}
\alias{split_matches}
\title{Split Matches into Results, Fixtures, Team Ratings, and Standings}
\usage{
split_matches(matches, deductions, date = Sys.Date(), xG_factor, xTable = F)
}
\arguments{
\item{matches}{A data frame containing match data.}

\item{deductions}{A data frame containing points deductions for teams.}

\item{date}{The date for filtering matches (default is the current date).}

\item{xG_factor}{A numeric factor used in calculating team ratings.}

\item{xTable}{A logical indicating whether to include expected table (xTable) in standings (default is FALSE).}
}
\value{
A list containing four components:
\describe{
\item{results}{A data frame of match results.}
\item{fixtures}{A data frame of upcoming fixtures.}
\item{team_ratings}{A data frame of team ratings.}
\item{standings}{A data frame of league standings.}
}
}
\description{
This function processes match data and splits it into various components including results, fixtures, team ratings, and standings.
}
\details{
This function calls several helper functions to transform the match data:
\itemize{
\item \code{matches_to_results} - Converts matches to results based on the given date.
\item \code{matches_to_fixtures} - Extracts upcoming fixtures based on the given date.
\item \code{matches_to_team_ratings} - Calculates team ratings using the given xG_factor.
\item \code{results_to_standings} - Computes league standings, including optional expected table (xTable).
}
}
\examples{
\dontrun{
  matches <- data.frame(...) # Your match data here
  deductions <- data.frame(...) # Your points deductions here
  date <- Sys.Date()
  xG_factor <- 1.2
  results <- split_matches(matches, deductions, date, xG_factor, xTable = TRUE)
}

}
