match_impact = function(projection, game_id = 10, race_teams = c("Arsenal", "Manchester City", "Liverpool")) {

teams = projection$simmed_results %>%
                dplyr::filter(gameID == game_id) %>%
                dplyr::select(home_team, away_team) %>%
                dplyr::distinct()

home_team = teams %>% dplyr::pull(home_team)
away_team = teams %>% dplyr::pull(away_team)

home_wins = projection$simmed_results %>%
  dplyr::filter(gameID == game_id) %>%
  dplyr::filter(home_win == 1) %>%
  dplyr::pull(trial)

away_wins = projection$simmed_results %>%
  dplyr::filter(gameID == game_id) %>%
  dplyr::filter(away_win == 1) %>%
  dplyr::pull(trial)

draws = projection$simmed_results %>%
  dplyr::filter(gameID == game_id) %>%
  dplyr::filter(draw == 1) %>%
  dplyr::pull(trial)

home_win_mean_final_projection = projection$simmed_final_standings %>%
  dplyr::filter(trial %in% home_wins) %>%
  get_final_projection()

home_win_sim_standings = projection$simmed_final_standings %>%
  dplyr::filter(trial %in% home_wins)

home_win_final_odds = get_projection_odds(home_win_mean_final_projection, home_win_sim_standings, trials = length(home_wins))

away_win_mean_final_projection = projection$simmed_final_standings %>%
  dplyr::filter(trial %in% away_wins) %>%
  get_final_projection()

away_win_sim_standings = projection$simmed_final_standings %>%
  dplyr::filter(trial %in% away_wins)

away_win_final_odds = get_projection_odds(away_win_mean_final_projection, away_win_sim_standings, trials = length(away_wins))

draw_mean_final_projection = projection$simmed_final_standings %>%
  dplyr::filter(trial %in% draws) %>%
  get_final_projection()

draw_sim_standings = projection$simmed_final_standings %>%
  dplyr::filter(trial %in% draws)

draw_final_odds = get_projection_odds(draw_mean_final_projection, draw_sim_standings, trials = length(draws))

home_win = home_win_final_odds %>%
  dplyr::filter(team %in% race_teams) %>%
  dplyr::ungroup() %>%
  dplyr::select(team, champion) %>%
  dplyr::mutate(result = paste0(home_team,"_win"))

away_win = away_win_final_odds %>%
  dplyr::filter(team %in% race_teams) %>%
  dplyr::ungroup() %>%
  dplyr::select(team, champion) %>%
  dplyr::mutate(result = paste0(away_team,"_win"))

draw = draw_final_odds %>%
  dplyr::filter(team %in% race_teams) %>%
  dplyr::ungroup() %>%
  dplyr::select(team, champion) %>%
  dplyr::mutate(result = "draw")

results = dplyr::bind_rows(home_win, away_win, draw) %>%
    dplyr::mutate(result = gsub(" ", "_", result)) %>%
    tidyr::pivot_wider(names_from = result, values_from = champion)

outputs = list(results = results,
               home_team = home_team,
               away_team = away_team)

return(outputs)

}


