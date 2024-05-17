#' run projection for given matches
#'
#' @param trials
#' @param matches
#' @param date
#' @param league_adj
#' @param xG_factor
#' @param smooth
#' @param hot
#' @param europe
#' @param rel
#' @param deductions
#'
#' @return
#' @export
#'
#' @examples
#'
#'

run_projection = function(trials, matches, deductions, date = Sys.Date(), league_adj, xG_factor, smooth, hot, europe = 6, rel = 18){

  #TODO: remove matches parameter so it goes online itself
  matches_split = matches %>%
    split_matches(date = date,
                  deductions = deductions,
                  xG_factor = xG_factor,
                  xTable = F)

  fixtures = matches_split$fixtures
  team_ratings = matches_split$team_ratings
  standings = matches_split$standings
  results = matches_split$results

  sim_summary = run_season_trials(trials = trials,
                                  fixtures = fixtures,
                                  results = results,
                                  deductions = deductions,
                                  team_ratings = team_ratings,
                                  league_adj = league_adj,
                                  xG_factor = xG_factor,
                                  smooth = smooth,
                                  hot = hot)

  mean_final_projections = get_final_projection(sim_summary$standings,
                                                mode = 'mean')

  mean_projection_odds = get_projection_odds(final_projections = mean_final_projections,
                                             sim_standings = sim_summary$standings,
                                             trials = trials,
                                             europe = europe,
                                             rel = rel)

  median_final_projections = get_final_projection(sim_summary$standings,
                                                  mode = 'median')

  median_projection_odds = get_projection_odds(final_projections = median_final_projections,
                                               sim_standings = sim_summary$standings,
                                               trials = trials,
                                               europe = europe,
                                               rel = rel)

  fixtures_with_odds = sim_summary$results %>%
    get_match_odds(trials = trials)

  params = tibble::tibble(trials = trials,
                  date = date,
                  league_adj = league_adj,
                  xG_factor = xG_factor,
                  smooth = smooth,
                  hot  = hot,
                  europe = europe,
                  rel = rel)

  list(match_odds = fixtures_with_odds,
       starting_team_ratings = team_ratings,
       starting_standings = standings,
       simmed_results = sim_summary$results,
       simmed_final_ratings = sim_summary$ratings,
       simmed_final_standings = sim_summary$standings,
       mean_final_projections = mean_final_projections,
       mean_projection_odds =  mean_projection_odds,
       median_final_projections = median_final_projections,
       median_projection_odds =  median_projection_odds,
       params = params,
       deductions = deductions)

}
