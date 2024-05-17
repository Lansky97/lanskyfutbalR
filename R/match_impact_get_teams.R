#' Get Teams for a Given Game ID
#'
#' This function retrieves the home and away teams for a specified game ID from the simulation results.
#'
#' @param projection A list containing the simulation results, including a data frame `simmed_results` with columns for gameID, home_team, and away_team.
#' @param game_id An integer specifying the game ID for which to retrieve the teams.
#'
#' @return A list containing two elements:
#' \item{home_team}{A character vector with the home team for the specified game ID.}
#' \item{away_team}{A character vector with the away team for the specified game ID.}
#'
#' @details This function filters the `simmed_results` data frame to find the unique home and away teams for the given game ID. It returns a list with the home and away teams.
#'
#' @examples
#' \dontrun{
#'   projection <- list(simmed_results = data.frame(gameID = c(1, 1, 2, 2),
#'                                                  home_team = c("Team A", "Team A", "Team B", "Team B"),
#'                                                  away_team = c("Team C", "Team C", "Team D", "Team D")))
#'   game_id <- 1
#'   teams <- match_impact_get_teams(projection, game_id)
#'   print(teams$home_team)
#'   print(teams$away_team)
#' }
#'
#' @export
match_impact_get_teams <- function(projection, game_id) {
  teams <- projection$simmed_results %>%
    dplyr::filter(gameID == game_id) %>%
    dplyr::select(home_team, away_team) %>%
    dplyr::distinct()

  list(
    home_team = teams %>% dplyr::pull(home_team),
    away_team = teams %>% dplyr::pull(away_team)
  )
}
