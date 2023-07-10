#' run simulation of n random seasons of given remaining fixtures
#'
#' @param trials
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
run_season_trials = function(trials, fixtures,results, team_ratings, league_adj,xG_factor,smooth,hot){

  sim_results = vector('list', trials)
  sim_ratings = vector('list', trials)
  sim_standings = vector('list', trials)

  names(sim_results) = seq(1:trials)
  names(sim_ratings) = seq(1:trials)
  names(sim_standings) = seq(1:trials)

  for (trial in 1:trials) {

    sim_summary = simulate_season(fixtures = fixtures,
                                  results = results,
                                  team_ratings = team_ratings,
                                  league_adj = league_adj,
                                  xG_factor = xG_factor,
                                  smooth = smooth,
                                  hot = hot)

    sim_results[[paste(trial)]] = sim_summary$results %>%
                                    dplyr::mutate(trial = trial)

    sim_ratings[[paste(trial)]] = sim_summary$ratings %>%
                                    dplyr::mutate(trial = trial)

    sim_standings[[paste(trial)]] = sim_summary$standings %>%
                                      dplyr::mutate(trial = trial)



  }

  sim_results = sim_results %>%
                  dplyr::bind_rows()

  sim_ratings = sim_ratings %>%
                  dplyr::bind_rows()

  sim_standings = sim_standings %>%
                    dplyr::bind_rows()

  list(results = sim_results,
       ratings = sim_ratings,
       standings = sim_standings)

}
