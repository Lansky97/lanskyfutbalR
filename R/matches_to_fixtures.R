#' Convert Matches to Fixtures
#'
#' This function processes match data to generate a data frame of upcoming fixtures from a specified date.
#'
#' @param matches A data frame containing match data.
#' @param date The date for filtering matches (default is the current date).
#'
#' @return A data frame of upcoming fixtures including game ID, date, game week, and team names.
#'
#' @details This function filters the matches to include only those that are scheduled to be played on or after the specified date. It then arranges the fixtures by game week, assigns game IDs, and selects relevant columns for the output.
#'
#' @examples
#' \dontrun{
#'   matches <- data.frame(...) # Your match data here
#'   fixtures <- matches_to_fixtures(matches, date = Sys.Date())
#' }
#'
#' @export
matches_to_fixtures = function(matches, date = Sys.Date()){

  matches %>%
      dplyr::filter(Date >= date) %>%
      dplyr::mutate(GW = as.integer(Wk)) %>%
      dplyr::arrange(GW) %>%
      dplyr::mutate(gameID = dplyr::row_number())%>%
      dplyr::select(gameID, Date, GW, home_team = Home,away_team = Away)

}
