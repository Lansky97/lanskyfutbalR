#' convert list of matches to list of fixtures to be played
#'
#' @param matches
#' @param date
#'
#' @return
#' @export
#'
#' @examples
matches_to_fixtures = function(matches, date = Sys.Date()){

  matches %>%
   #dtplyr::lazy_dt()%>%
      dplyr::filter(Date >= date) %>%
      dplyr::mutate(GW = as.integer(Wk)) %>%
      dplyr::arrange(GW) %>%
      dplyr::mutate(gameID = dplyr::row_number())%>%
      dplyr::select(gameID, Date, GW, home_team = Home,away_team = Away)
    #dplyr::as_tibble()

}
