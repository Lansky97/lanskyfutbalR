#' Calculate Match Odds from Simulated Results
#'
#' This function calculates the odds for each match based on the simulated results from multiple trials.
#'
#' @param simmed_results A data frame containing the simulated results from multiple trials, including columns for game ID, date, game week (GW), home team, away team, home win, draw, and away win.
#' @param trials An integer specifying the number of trials run.
#'
#' @return A data frame with the calculated odds for each match, including columns for game ID, date, game week (GW), home team, away team, home win odds, draw odds, and away win odds.
#'
#' @details This function calculates the probability of each match outcome (home win, draw, away win) based on the simulation results. It then converts these probabilities into odds.
#'
#' @examples
#' \dontrun{
#'   simmed_results <- data.frame(gameID = rep(1:10, each = 100),
#'                                Date = rep(Sys.Date(), 1000),
#'                                GW = rep(1:10, each = 100),
#'                                home_team = sample(letters[1:20], 1000, replace = TRUE),
#'                                away_team = sample(letters[1:20], 1000, replace = TRUE),
#'                                home_win = sample(0:1, 1000, replace = TRUE),
#'                                draw = sample(0:1, 1000, replace = TRUE),
#'                                away_win = sample(0:1, 1000, replace = TRUE))
#'   trials <- 100
#'   match_odds <- get_match_odds(simmed_results, trials)
#' }
#'
#' @export
get_match_odds = function(simmed_results, trials) {

  simmed_results %>%
    dplyr::group_by(gameID) %>%
    dplyr::mutate(dplyr::across(.cols = c(home_win, draw, away_win),
                                .fns = ~round(sum(.x) / trials,2)))%>%
    dplyr::ungroup() %>%
    dplyr::select(gameID, Date, GW,home_team,
                  away_team, home_win,draw, away_win)%>%
    dplyr::distinct() %>%
    dplyr::mutate(home_odds = round(1/home_win,2),
                  draw_odds = round(1/draw,2),
                  away_odds = round(1/away_win,2))

}
