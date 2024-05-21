#' Update Team Ratings Based on Game Week Results
#'
#' This function updates the team ratings based on the results of the game week.
#'
#' @param current_team_ratings A data frame containing the current team ratings.
#' @param gw_results A data frame containing the results of the game week.
#' @param xG_factor A numeric factor used to smooth the goals with expected goals (xG).
#' @param smooth A logical indicating whether to apply smoothing to the goals (default is TRUE).
#' @param hot A logical indicating whether to update the team ratings with the current game week results (default is TRUE).
#'
#' @return A data frame with updated team ratings, including expected goals (expG), expected goals against (expGA), and league average expected goals (league_expG) for each team at home and away venues.
#'
#' @details This function updates the team ratings based on the results of the current game week. It applies smoothing if specified, and updates the cumulative goals and games played for each team. The league average expected goals are also recalculated.
#'
#' @examples
#' \dontrun{
#'   current_team_ratings <- data.frame(team = c("Team A", "Team B"),
#'                                      venue = rep(c("home", "away"), each = 2),
#'                                      expG = runif(4, 1, 2),
#'                                      expGA = runif(4, 1, 2),
#'                                      league_expG = runif(4, 1, 2),
#'                                      games_played = sample(1:10, 4),
#'                                      Total_smoothed_goals = runif(4, 10, 20),
#'                                      Total_smoothed_goalsA = runif(4, 10, 20))
#'   gw_results <- data.frame(gameID = 1:2, Date = Sys.Date(), GW = 1,
#'                            home_team = c("Team A", "Team B"),
#'                            away_team = c("Team C", "Team D"),
#'                            home_score = c(2, 1),
#'                            away_score = c(1, 2),
#'                            home_exp = c(1.5, 2.0),
#'                            away_exp = c(1.0, 1.5))
#'   updated_team_ratings <- update_team_ratings(current_team_ratings, gw_results,
#'                           xG_factor = 0.6, smooth = TRUE, hot = TRUE)
#' }
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select mutate group_by ungroup left_join right_join across if_else
#' @importFrom tidyr pivot_longer
#'
#' @export
update_team_ratings = function(current_team_ratings, gw_results, xG_factor,smooth, hot){

  if(hot){
  gw_smoothed = gw_results %>%
                  dplyr::select(-c(home_win, draw, away_win,
                                   home_xWin, xDraw, away_xWin)) %>%
                  tidyr::pivot_longer(-c(gameID, Date, GW),
                        names_to = c('venue','.value'),
                        names_sep = '_') %>%
                  dplyr::mutate(smoothed_goals = if(smooth){ (1-xG_factor)*score + xG_factor*exp} else{ score}) %>%
                  dplyr::group_by(gameID) %>%
                  dplyr::mutate(smoothed_goalsA = sum(smoothed_goals) - smoothed_goals) %>%
                  dplyr::ungroup()%>%
                  dplyr::select(venue, team, smoothed_goals,smoothed_goalsA)

  updated_team_rating = current_team_ratings %>%
    dplyr::left_join(gw_smoothed,  by = c('venue','team')) %>%
    dplyr::mutate(games_played = dplyr::if_else(is.na(smoothed_goals), games_played, games_played + 1L),
                  Total_smoothed_goals = dplyr::if_else(is.na(smoothed_goals), Total_smoothed_goals, Total_smoothed_goals + smoothed_goals),
                  Total_smoothed_goalsA = dplyr::if_else(is.na(smoothed_goals),Total_smoothed_goalsA, Total_smoothed_goalsA + smoothed_goalsA)) %>%
    dplyr::mutate(expG = dplyr::if_else(games_played == 0, expG, Total_smoothed_goals / games_played),
                  expGA = dplyr::if_else(games_played == 0, expGA, Total_smoothed_goalsA / games_played))%>%
    dplyr::group_by(venue) %>%
    dplyr::mutate(league_expG = mean(expG)) %>%
    dplyr::mutate(dplyr::across(.cols = c(expG, expGA),
                         .fns = ~dplyr::if_else(games_played == 0, mean(.x), .x)))%>%
    dplyr::ungroup() %>%
    dplyr::select(-smoothed_goals, -smoothed_goalsA)

  }

  if(!hot){

    updated_team_rating = current_team_ratings

    }

  updated_team_rating

}
