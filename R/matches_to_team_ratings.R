#' convert matches to team ratings based on xG factor of played games
#'
#' @param xG_factor
#' @param matches
#' @param date
#'
#' @return
#' @export
#'
#' @examples
matches_to_team_ratings = function(matches, date = Sys.Date(), xG_factor = 0.6){

output =  matches %>%
  #dtplyr::lazy_dt() %>%
    dplyr::filter(!is.na(HomeGoals), Date < date) %>%
    dplyr::select(Home, HomeGoals,Home_xG, Away,AwayGoals, Away_xG)%>%
    dplyr::rename(home_team = Home,
                  home_goals = HomeGoals,
                  home_xG = Home_xG,
                  away_team = Away,
                  away_goals = AwayGoals,
                  away_xG = Away_xG) %>%
    dplyr::mutate(gameId = row_number())%>%
    tidyr::pivot_longer(-gameId,
                 names_to = c("venue",".value"),
                 names_sep = "_")%>%
    dplyr::group_by(gameId) %>%
    dplyr::mutate(xG = if_else(is.na(xG), goals, xG))%>%
    dplyr::mutate(gA = sum(goals)- goals, xGA = sum(xG) - xG)%>%
    dplyr::ungroup()%>%
    dplyr::mutate(smoothed_goals = (1-xG_factor)*goals + xG_factor*xG,
                  smoothed_goalsA = (1-xG_factor)*gA + xG_factor*xGA) %>%
    dplyr::group_by(venue,team)%>%
    dplyr::mutate(expG = round(mean(smoothed_goals), 2),
                  expGA = round(mean(smoothed_goalsA),2)) %>%
    dplyr::mutate(Total_smoothed_goals = sum(smoothed_goals),
                  Total_smoothed_goalsA = sum(smoothed_goalsA),
                  games_played = n())%>%
    dplyr::select(team, venue,expG, expGA, games_played,
                  Total_smoothed_goals, Total_smoothed_goalsA)%>%
    dplyr::ungroup()%>%
    dplyr::distinct()%>%
    dplyr::group_by(venue) %>%
    dplyr::mutate(league_expG = mean(expG))%>%
    dplyr::ungroup() %>%
    dplyr::arrange(team, venue)
  #dplyr::as_tibble()

tidyr::expand_grid(team = unique(matches$Home),
            venue = c("home", "away")) %>%
  dplyr::full_join(output, by = c("team", "venue"))%>%
  dplyr::mutate(dplyr::across(.cols = c(games_played,Total_smoothed_goals, Total_smoothed_goalsA),
                .fns = ~dplyr::if_else(is.na(expG), 0, .x)))%>%
  dplyr::group_by(venue) %>%
  dplyr::mutate(dplyr::across(.cols = c(expG, expGA, league_expG),
                .fns = ~dplyr::if_else(is.na(.x), mean(.x, na.rm = T), .x))) %>%
  dplyr::ungroup()



}
