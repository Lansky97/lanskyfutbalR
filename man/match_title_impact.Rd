% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/match_title_impact.R
\name{match_title_impact}
\alias{match_title_impact}
\title{Analyze Match Impact on Title Race}
\usage{
match_title_impact(
  projection,
  game_id = 10,
  race_teams = c("Arsenal", "Manchester City", "Liverpool")
)
}
\arguments{
\item{projection}{A list containing the simulation results, including data frames \code{simmed_final_standings} and \code{simmed_results}.}

\item{game_id}{An integer specifying the game ID for which to analyze the match impact (default is 10).}

\item{race_teams}{A character vector of teams to include in the race (default is c("Arsenal", "Manchester City", "Liverpool")).}
}
\value{
A list containing:
\item{results}{A data frame with the final odds for different outcomes (home win, away win, draw) on the title race for the specified teams.}
\item{home_team}{A character vector with the home team for the specified game ID.}
\item{away_team}{A character vector with the away team for the specified game ID.}
}
\description{
This function analyzes the impact of a specific match on the title race by calculating the final odds for different outcomes (home win, away win, draw) for a given game ID.
}
\details{
This function retrieves the home and away teams for a specified game ID, filters the simulation trials based on the match outcome (home win, away win, draw), calculates the final odds for each outcome, and returns a data frame with the odds for the title race.
}
\examples{
\dontrun{
  projection <- list(simmed_final_standings = data.frame(trial = rep(1:100, each = 20),
                                                         team = rep(letters[1:20], 100),
                                                         champion = runif(2000),
                                                         UCL = runif(2000),
                                                         UEFA = runif(2000),
                                                         REL = runif(2000)),
                     simmed_results = data.frame(gameID = c(1, 1, 2, 2),
                                                 trial = c(1, 2, 1, 2),
                                                 home_win = c(1, 0, 0, 1),
                                                 draw = c(0, 1, 0, 0),
                                                 away_win = c(0, 0, 1, 0)))
  game_id <- 10
  race_teams <- c("Arsenal", "Manchester City", "Liverpool")
  match_impact_results <- match_title_impact(projection, game_id, race_teams)
  print(match_impact_results)
}

}
