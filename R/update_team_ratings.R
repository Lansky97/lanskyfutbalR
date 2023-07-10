#' Title
#'
#' @param current_team_ratings
#' @param gw_results
#' @param xG_factor
#' @param smooth
#' @param hot
#'
#' @return
#' @export
#'
#' @examples
update_team_ratings = function(current_team_ratings, gw_results, xG_factor,smooth, hot){

  if(hot){
  gw_smoothed = gw_results %>%
                  tidyr::pivot_longer(-c(gameID, Date, GW),
                        names_to = c('venue','.value'),
                        names_sep = '_') %>%
                  dplyr::mutate(smoothed_goals = if(smooth){ (1-xG_factor)*score + xG_factor*exp} else{ score},
                                                                score) %>%
                  dplyr::group_by(gameID) %>%
                  dplyr::mutate(smoothed_goalsA = sum(smoothed_goals) - smoothed_goals) %>%
                  dplyr::ungroup()%>%
                  dplyr::select(venue, team, smoothed_goals,smoothed_goalsA)

  current_team_ratings %>%
    dplyr::left_join(gw_smoothed,  by = c('venue','team')) %>%
    dplyr::mutate(games_played = dplyr::if_else(is.na(smoothed_goals), games_played, games_played + 1L),
                  Total_smoothed_goals = dplyr::if_else(is.na(smoothed_goals), Total_smoothed_goals, Total_smoothed_goals + smoothed_goals),
                  Total_smoothed_goalsA = dplyr::if_else(is.na(smoothed_goals),Total_smoothed_goalsA, Total_smoothed_goalsA + smoothed_goalsA)) %>%
    dplyr::mutate(expG = Total_smoothed_goals / games_played,
                  expGA = Total_smoothed_goalsA / games_played)%>%
    dplyr::group_by(venue) %>%
    dplyr::mutate(league_expG = mean(expG)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-smoothed_goals, -smoothed_goalsA)

  }

  if(!hot){

    current_team_ratings

    }

}
