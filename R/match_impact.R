#' Analyze Match Impact on Races
#'
#' This function analyzes the impact of a specific match on the specified race by calculating the final odds for different outcomes (home win, away win, draw) for a given game ID.
#'
#' @param projection A list containing the simulation results, including data frames `simmed_final_standings` and `simmed_results`.
#' @param game_id An integer specifying the game ID for which to analyze the match impact (default is 10).
#' @param race_teams A character vector of teams to include in the race (default is c("Arsenal", "Manchester City")).
#' @param race_place A character string specifying the race place to filter by (options are "champion", "UCL", "UEFA", "REL"). Default is "champion".
#'
#' @return A data frame containing the final odds for different outcomes (home win, away win, draw) on the specified race place for the given teams.
#'
#' @details This function retrieves the home and away teams for a specified game ID, filters the simulation trials based on the match outcome (home win, away win, draw), calculates the final odds for each outcome, and returns a data frame with the odds for the specified race place.
#'
#' @examples
#' \dontrun{
#'   projection <- list(simmed_final_standings = data.frame(trial = rep(1:100, each = 20),
#'                                                          team = rep(letters[1:20], 100),
#'                                                          champion = runif(2000),
#'                                                          UCL = runif(2000),
#'                                                          UEFA = runif(2000),
#'                                                          REL = runif(2000)),
#'                      simmed_results = data.frame(gameID = c(1, 1, 2, 2),
#'                                                  trial = c(1, 2, 1, 2),
#'                                                  home_win = c(1, 0, 0, 1),
#'                                                  draw = c(0, 1, 0, 0),
#'                                                  away_win = c(0, 0, 1, 0)))
#'   game_id <- 10
#'   race_teams <- c("Arsenal", "Manchester City")
#'   race_place <- "champion"
#'   match_impact_results <- match_impact(projection, game_id, race_teams, race_place)
#'   print(match_impact_results)
#' }
#'
#' @importFrom tidyr pivot_wider
#'
#' @export
match_impact = function(projection, game_id = 10, race_teams = c("Arsenal", "Manchester City"), race_place = "champion") {

teams <- match_impact_get_teams(projection, game_id)
home_team <- teams$home_team
away_team <- teams$away_team

home_wins <- match_impact_get_trials(projection, game_id, "home_win")
away_wins <- match_impact_get_trials(projection, game_id, "away_win")
draws <- match_impact_get_trials(projection, game_id, "draw")

home_win_results <- match_impact_get_final_odds(projection, home_wins,paste0(home_team, "_win"), race_teams, race_place)
away_win_results <- match_impact_get_final_odds(projection, away_wins, paste0(away_team, "_win"), race_teams, race_place)
draw_results <- match_impact_get_final_odds(projection, draws, "draw", race_teams, race_place)


results <- dplyr::bind_rows(home_win_results, away_win_results, draw_results) %>%
  dplyr::mutate(result = gsub("_", " ", result)) %>%
  tidyr::pivot_wider(names_from = result, values_from = !!rlang::sym(race_place)) %>%
  dplyr::mutate(race = race_place)

results

}
