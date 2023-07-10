#' Title
#'
#' @param gw_fixtures
#'
#' @return
#' @export
#'
#' @examples
simulate_gw = function(gw_fixtures){

  gw_fixtures %>%
    dplyr::mutate(home_score = sapply(home_exp, function(x) rpois(1,x)),
                  away_score = sapply(away_exp, function(x) rpois(1,x))) %>%
    dplyr::mutate(home_win = dplyr::if_else(home_score > away_score, 1, 0),
                         draw = dplyr::if_else(home_score == away_score,1,0),
                         away_win = dplyr::if_else(home_score < away_score,1,0),
                         home_xWin = dplyr::if_else(home_exp > away_exp, 1, 0),
                         xDraw = dplyr::if_else(home_exp == away_exp,1,0),
                         away_xWin = dplyr::if_else(home_exp < away_exp,1,0))
}
