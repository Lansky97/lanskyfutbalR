#' Convert Matches to Results
#'
#' This function processes match data to generate a data frame of match results up to a specified date.
#'
#' @param matches A data frame containing match data.
#' @param date The date for filtering matches (default is the current date).
#'
#' @return A data frame of match results including game ID, date, game week, team names, expected goals, actual scores, and win/draw indicators.
#'
#' @details This function filters the matches to include only those that have already been played (i.e., matches with non-missing scores and dates before the specified date). It then arranges the matches by date, assigns game IDs and game weeks, and includes additional columns for expected and actual outcomes.
#'
#' @examples
#' \dontrun{
#'   matches <- data.frame(...) # Your match data here
#'   results <- matches_to_results(matches, date = Sys.Date())
#' }
#'
#' @export
matches_to_results = function(matches, date = Sys.Date()){

  matches %>%
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

}
