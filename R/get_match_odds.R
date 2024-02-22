#' get match odds from simmed results
#'
#' @param simmed_results
#' @param trials
#'
#' @return
#' @export
#'
#' @examples
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
