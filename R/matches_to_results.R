#' convert list of matches to list of results to be played
#'
#' @param matches
#' @param date
#'
#' @return
#' @export
#'
#' @examples
matches_to_results = function(matches, date = Sys.Date()){

  matches %>%
    #dtplyr::lazy_dt()%>%
    dplyr::filter(!is.na(HomeGoals), Date < date) %>%
    dplyr::arrange(Date) %>%
    dplyr::mutate(gameID = dplyr::row_number(),
                  GW = as.numeric(Wk)) %>%
    dplyr::select(gameID, Date, GW,
                  home_team = Home,  away_team = Away,
                  home_exp = Home_xG, away_exp = Away_xG,
                  home_score = HomeGoals,away_score = AwayGoals) %>%
    dplyr::mutate(home_win = dplyr::if_else(home_score > away_score, 1, 0),
                  draw = dplyr::if_else(home_score == away_score,1,0),
                  away_win = dplyr::if_else(home_score < away_score,1,0),
                  home_xWin = dplyr::if_else(home_exp > away_exp, 1, 0),
                  xDraw = dplyr::if_else(home_exp == away_exp,1,0),
                  away_xWin = dplyr::if_else(home_exp < away_exp,1,0))
  #dplyr::as_tibble()

}
