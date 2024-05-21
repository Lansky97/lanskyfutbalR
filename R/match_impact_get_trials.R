#' Get Trials for a Specific Match Outcome
#'
#' This function retrieves the trial numbers for a specified match outcome (e.g., home win, away win, draw) for a given game ID from the simulation results.
#'
#' @param projection A list containing the simulation results, including a data frame `simmed_results` with columns for gameID, trial, home_win, draw, and away_win.
#' @param game_id An integer specifying the game ID for which to retrieve the trials.
#' @param outcome A character string specifying the match outcome to filter by (e.g., "home_win", "draw", "away_win").
#'
#' @return A numeric vector containing the trial numbers where the specified outcome occurred for the given game ID.
#'
#' @details This function filters the `simmed_results` data frame to find the trials where the specified match outcome occurred for the given game ID. It returns a vector of trial numbers.
#'
#' @examples
#' \dontrun{
#'   projection <- list(simmed_results = data.frame(gameID = c(1, 1, 2, 2),
#'                                                  trial = c(1, 2, 1, 2),
#'                                                  home_win = c(1, 0, 0, 1),
#'                                                  draw = c(0, 1, 0, 0),
#'                                                  away_win = c(0, 0, 1, 0)))
#'   game_id <- 1
#'   outcome <- "home_win"
#'   trials <- match_title_get_trials(projection, game_id, outcome)
#'   print(trials)
#' }
#'
#' @importFrom dplyr pull
#' @importFrom rlang sym !!
#'
#' @export
match_impact_get_trials <- function(projection, game_id, outcome) {
  projection$simmed_results %>%
    dplyr::filter(gameID == game_id) %>%
    dplyr::filter(!!rlang::sym(outcome) == 1) %>%
    dplyr::pull(trial)
}
