#' Title
#'
#' @param fixtures
#' @param team_ratings
#' @param league_adj
#' @param xG_factor
#' @param smooth
#' @param hot
#' @param results
#'
#' @return
#' @export
#'
#' @examples
simulate_season = function(fixtures, results,team_ratings, league_adj,xG_factor, smooth, hot){

  season_results = vector('list', length(unique(fixtures$GW)))

  names(season_results) = unique(fixtures$GW)

  for (gw in unique(fixtures$GW)){

    season_results[[paste(gw)]] = fixtures %>%
                                    dplyr::filter(GW == gw) %>%
                                    get_gw_exp_goals(team_ratings = team_ratings,
                                                     league_adj = league_adj) %>%
                                    simulate_gw()

    team_ratings = update_team_ratings(current_team_ratings = team_ratings,
                                        gw_results = season_results[[paste(gw)]],
                                        xG_factor =xG_factor,
                                        smooth = smooth,
                                        hot = hot)

  }

  season_results = season_results %>%
                    dplyr::bind_rows()

  season_standings = season_results %>%
                      dplyr::bind_rows(results) %>%
                      dplyr::mutate(gameID = dplyr::row_number()) %>%
                      results_to_standings(xTable = F)

  list(results = season_results,
       ratings = team_ratings,
       standings = season_standings)

}
