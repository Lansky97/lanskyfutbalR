#' Calculate Expected Goals for Game Week Fixtures
#'
#' This function calculates the expected goals for each fixture in a given game week based on team ratings and optional league adjustments.
#'
#' @param gw_fixtures A data frame containing the fixtures for the game week, including columns for home and away teams.
#' @param team_ratings A data frame containing team ratings, including expected goals (expG), expected goals against (expGA), and league average expected goals (league_expG) for each team at home and away venues.
#' @param league_adj A logical indicating whether to apply league-wide adjustments to the expected goals calculations (default is FALSE).
#'
#' @return A data frame with the calculated expected goals for both home and away teams in each fixture, including game ID, date, and game week.
#'
#' @details This function joins the fixtures with team ratings, applies optional league adjustments, and calculates the expected goals for each team in each fixture.
#'
#' @examples
#' \dontrun{
#'   gw_fixtures <- data.frame(gameID = 1:2, Date = Sys.Date(), GW = 1,
#'                             home_team = c("Team A", "Team B"),
#'                             away_team = c("Team C", "Team D"))
#'   team_ratings <- data.frame(team = c("Team A", "Team B", "Team C", "Team D"),
#'                              venue = rep(c("home", "away"), each = 4),
#'                              expG = runif(8, 1, 2),
#'                              expGA = runif(8, 1, 2),
#'                              league_expG = runif(8, 1, 2))
#'   gw_exp_goals <- get_gw_exp_goals(gw_fixtures, team_ratings, league_adj = TRUE)
#' }
#'
#' @export
get_gw_exp_goals = function(gw_fixtures, team_ratings, league_adj){

  #TODO: THINK ABOUT gameID
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
