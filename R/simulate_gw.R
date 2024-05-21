#' Simulate Game Week Fixtures
#'
#' This function simulates the outcomes of a given game week's fixtures based on expected goals.
#'
#' @param gw_fixtures A data frame containing the fixtures for the game week, including columns for expected goals (`home_exp` and `away_exp`).
#'
#' @return A data frame with simulated match results including scores, win/draw indicators, and expected win/draw indicators.
#'
#' @details This function uses a Poisson distribution to simulate the number of goals scored by the home and away teams based on their expected goals. It then determines the match outcome (win, draw, or loss) for both actual and expected goals.
#'
#' @examples
#' \dontrun{
#'   gw_fixtures <- data.frame(home_team = c("Team A", "Team B"),
#'                             away_team = c("Team C", "Team D"),
#'                             home_exp = c(1.5, 2.0),
#'                             away_exp = c(1.0, 1.5))
#'   simulated_results <- simulate_gw(gw_fixtures)
#' }
#'
#' @importFrom stats rpois
#' @export
simulate_gw = function(gw_fixtures){

  gw_fixtures %>%
    dplyr::mutate(home_score = sapply(home_exp, function(x) stats::rpois(1,x)),
                  away_score = sapply(away_exp, function(x) stats::rpois(1,x))) %>%
    dplyr::mutate(home_win = dplyr::if_else(home_score > away_score, 1, 0),
                         draw = dplyr::if_else(home_score == away_score,1,0),
                         away_win = dplyr::if_else(home_score < away_score,1,0),
                         home_xWin = dplyr::if_else(home_exp > away_exp, 1, 0),
                         xDraw = dplyr::if_else(home_exp == away_exp,1,0),
                         away_xWin = dplyr::if_else(home_exp < away_exp,1,0))
}
