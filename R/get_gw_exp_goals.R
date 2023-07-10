#' Title
#'
#' @param gw_fixtures
#' @param team_ratings
#' @param league_adj
#'
#' @return
#' @export
#'
#' @examples
get_gw_exp_goals = function(gw_fixtures, team_ratings, league_adj){

  gw_team_ratings = team_ratings %>%
                      dplyr::select(team, venue, expG, expGA, league_expG) %>%
                      dplyr::filter(
                      (team %in% gw_fixtures$home_team & venue == "home")|
                      (team %in% gw_fixtures$away_team & venue == "away"))

  gw_exp_goals = gw_fixtures %>%
                  tidyr::pivot_longer(c(home_team, away_team),
                        names_to = c('venue', '.value'),
                        names_sep = '_') %>%
                  dplyr::left_join(gw_team_ratings,
                                   by = c('team', 'venue')) %>%
                  tidyr::pivot_wider(id_cols = c(gameID, Date, GW),
                                     names_from = venue,
                                     names_glue = '{venue}_{.value}',
                                      values_from = c(team, expG, expGA, league_expG))

  if(league_adj){

  gw_exp_goals = gw_exp_goals %>%
                  dplyr::mutate(home_expG = sqrt(home_expG * home_league_expG),
                                home_expGA = sqrt(home_expGA * away_league_expG),
                                away_expG = sqrt(away_expG * away_league_expG),
                                away_expGA = sqrt(away_expGA * home_league_expG))

  }


  gw_exp_goals%>%
    dplyr::mutate(home_exp = sqrt(home_expG * away_expGA),
           away_exp = sqrt(away_expG * home_expGA)) %>%
      dplyr::select(gameID, Date, GW, home_team, away_team,home_exp, away_exp)


}
