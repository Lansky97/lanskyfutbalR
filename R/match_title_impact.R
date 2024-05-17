#' Analyze Match Impact on Title Race
#'
#' This function analyzes the impact of a specific match on the title race by calculating the final odds for different outcomes (home win, away win, draw) for a given game ID.
#'
#' @param projection A list containing the simulation results, including data frames `simmed_final_standings` and `simmed_results`.
#' @param game_id An integer specifying the game ID for which to analyze the match impact (default is 10).
#' @param race_teams A character vector of teams to include in the race (default is c("Arsenal", "Manchester City", "Liverpool")).
#'
#' @return A list containing:
#' \item{results}{A data frame with the final odds for different outcomes (home win, away win, draw) on the title race for the specified teams.}
#' \item{home_team}{A character vector with the home team for the specified game ID.}
#' \item{away_team}{A character vector with the away team for the specified game ID.}
#'
#' @details This function retrieves the home and away teams for a specified game ID, filters the simulation trials based on the match outcome (home win, away win, draw), calculates the final odds for each outcome, and returns a data frame with the odds for the title race.
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
#'   race_teams <- c("Arsenal", "Manchester City", "Liverpool")
#'   match_impact_results <- match_impact(projection, game_id, race_teams)
#'   print(match_impact_results)
#' }
#'
#' @export
match_impact = function(projection, game_id = 10, race_teams = c("Arsenal", "Manchester City", "Liverpool")) {

teams = projection$simmed_results %>%
                dplyr::filter(gameID == game_id) %>%
                dplyr::select(home_team, away_team) %>%
                dplyr::distinct()

home_team = teams %>% dplyr::pull(home_team)
away_team = teams %>% dplyr::pull(away_team)

home_wins = projection$simmed_results %>%
  dplyr::filter(gameID == game_id) %>%
  dplyr::filter(home_win == 1) %>%
  dplyr::pull(trial)

away_wins = projection$simmed_results %>%
  dplyr::filter(gameID == game_id) %>%
  dplyr::filter(away_win == 1) %>%
  dplyr::pull(trial)

draws = projection$simmed_results %>%
  dplyr::filter(gameID == game_id) %>%
  dplyr::filter(draw == 1) %>%
  dplyr::pull(trial)

home_win_mean_final_projection = projection$simmed_final_standings %>%
  dplyr::filter(trial %in% home_wins) %>%
  get_final_projection()

home_win_sim_standings = projection$simmed_final_standings %>%
  dplyr::filter(trial %in% home_wins)

home_win_final_odds = get_projection_odds(home_win_mean_final_projection, home_win_sim_standings, trials = length(home_wins))

away_win_mean_final_projection = projection$simmed_final_standings %>%
  dplyr::filter(trial %in% away_wins) %>%
  get_final_projection()

away_win_sim_standings = projection$simmed_final_standings %>%
  dplyr::filter(trial %in% away_wins)

away_win_final_odds = get_projection_odds(away_win_mean_final_projection, away_win_sim_standings, trials = length(away_wins))

draw_mean_final_projection = projection$simmed_final_standings %>%
  dplyr::filter(trial %in% draws) %>%
  get_final_projection()

draw_sim_standings = projection$simmed_final_standings %>%
  dplyr::filter(trial %in% draws)

draw_final_odds = get_projection_odds(draw_mean_final_projection, draw_sim_standings, trials = length(draws))

home_win = home_win_final_odds %>%
  dplyr::filter(team %in% race_teams) %>%
  dplyr::ungroup() %>%
  dplyr::select(team, champion) %>%
  dplyr::mutate(result = paste0(home_team,"_win"))

away_win = away_win_final_odds %>%
  dplyr::filter(team %in% race_teams) %>%
  dplyr::ungroup() %>%
  dplyr::select(team, champion) %>%
  dplyr::mutate(result = paste0(away_team,"_win"))

draw = draw_final_odds %>%
  dplyr::filter(team %in% race_teams) %>%
  dplyr::ungroup() %>%
  dplyr::select(team, champion) %>%
  dplyr::mutate(result = "draw")

results = dplyr::bind_rows(home_win, away_win, draw) %>%
    dplyr::mutate(result = gsub(" ", "_", result)) %>%
    tidyr::pivot_wider(names_from = result, values_from = champion)

outputs = list(results = results,
               home_team = home_team,
               away_team = away_team)

return(outputs)

}


