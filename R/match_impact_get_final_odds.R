#' Get Final Odds for a Specific Match Outcome
#'
#' This function calculates the final odds for a specific match outcome based on the filtered simulation trials and provided race criteria.
#'
#' @param projection A list containing the simulation results, including data frames `simmed_final_standings` and `simmed_results`.
#' @param trials A numeric vector containing the trial numbers for the specified match outcome.
#' @param result_label A character string to label the result (e.g., "home_win", "away_win", "draw").
#' @param race_teams A character vector of teams to include in the race.
#' @param race_place A character string specifying the race place to filter by (options are "champion", "UCL", "UEFA", "REL"). Default is "champion".
#'
#' @return A data frame containing the final odds for the specified match outcome, including columns for team, race place, and result label.
#'
#' @details This function filters the `simmed_final_standings` data frame to include only the specified trials. It then calculates the final odds based on the mean final projections and returns a data frame with the odds for the specified race place.
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
#'   trials <- c(1, 2)
#'   result_label <- "home_win"
#'   race_teams <- letters[1:5]
#'   race_place <- "champion"
#'   final_odds <- match_impact_get_final_odds(projection, trials, result_label, race_teams, race_place)
#'   print(final_odds)
#' }
#'
#' @export
match_impact_get_final_odds <- function(projection, trials, result_label, race_teams, race_place = "champion") {
  mean_final_projection <- projection$simmed_final_standings %>%
    dplyr::filter(trial %in% trials) %>%
    get_final_projection()

  sim_standings <- projection$simmed_final_standings %>%
    dplyr::filter(trial %in% trials)

  final_odds <- get_projection_odds(mean_final_projection, sim_standings, trials = length(trials))

  final_odds %>%
    dplyr::filter(team %in% race_teams) %>%
    dplyr::ungroup() %>%
    dplyr::select(team, !!rlang::sym(race_place)) %>%
    dplyr::mutate(result = result_label)
}
