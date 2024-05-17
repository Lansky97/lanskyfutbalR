#' Split Matches into Results, Fixtures, Team Ratings, and Standings
#'
#' This function processes match data and splits it into various components including results, fixtures, team ratings, and standings.
#'
#' @param matches A data frame containing match data.
#' @param deductions A data frame containing points deductions for teams.
#' @param date The date for filtering matches (default is the current date).
#' @param xG_factor A numeric factor used in calculating team ratings.
#' @param xTable A logical indicating whether to include expected table (xTable) in standings (default is FALSE).
#'
#' @return A list containing four components:
#' \describe{
#'   \item{results}{A data frame of match results.}
#'   \item{fixtures}{A data frame of upcoming fixtures.}
#'   \item{team_ratings}{A data frame of team ratings.}
#'   \item{standings}{A data frame of league standings.}
#' }
#'
#' @details This function calls several helper functions to transform the match data:
#' \itemize{
#'   \item \code{matches_to_results} - Converts matches to results based on the given date.
#'   \item \code{matches_to_fixtures} - Extracts upcoming fixtures based on the given date.
#'   \item \code{matches_to_team_ratings} - Calculates team ratings using the given xG_factor.
#'   \item \code{results_to_standings} - Computes league standings, including optional expected table (xTable).
#' }
#'
#' @examples
#' \dontrun{
#'   matches <- data.frame(...) # Your match data here
#'   deductions <- data.frame(...) # Your points deductions here
#'   date <- Sys.Date()
#'   xG_factor <- 1.2
#'   results <- split_matches(matches, deductions, date, xG_factor, xTable = TRUE)
#' }
#'
#' @export
#'
split_matches = function(matches, deductions, date = Sys.Date(), xG_factor, xTable = F){

  results = matches %>%
              matches_to_results(date = date)

  fixtures = matches %>%
              matches_to_fixtures(date = date)

  team_ratings = matches %>%
                  matches_to_team_ratings(date = date,
                                          xG_factor = xG_factor)

  standings = results %>%
                results_to_standings(deductions, xTable = xTable)

  list(results = results,
       fixtures = fixtures,
       team_ratings = team_ratings,
       standings = standings)

}
