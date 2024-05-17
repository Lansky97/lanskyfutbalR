#' Run Multiple Season Simulations
#'
#' This function runs multiple trials of season simulations based on given fixtures, initial results, points deductions, and team ratings.
#'
#' @param trials An integer specifying the number of trials to run.
#' @param fixtures A data frame containing the fixtures for the season, including columns for game week (GW), home team, and away team.
#' @param results A data frame containing the initial results of the season, if any.
#' @param deductions A data frame containing points deductions for teams.
#' @param team_ratings A data frame containing the initial team ratings, including expected goals (expG), expected goals against (expGA), and league average expected goals (league_expG) for each team at home and away venues.
#' @param league_adj A logical indicating whether to apply league-wide adjustments to the expected goals calculations (default is FALSE).
#' @param xG_factor A numeric factor used to smooth the goals with expected goals (xG).
#' @param smooth A logical indicating whether to apply smoothing to the goals (default is TRUE).
#' @param hot A logical indicating whether to update the team ratings with the current game week results (default is TRUE).
#'
#' @return A list containing:
#' \item{results}{A data frame with the simulated match results for all trials.}
#' \item{ratings}{A data frame with the updated team ratings after all trials.}
#' \item{standings}{A data frame with the final league standings after all trials.}
#'
#' @details This function runs the specified number of trials of season simulations. For each trial, it simulates the entire season, updates the team ratings, and compiles the results. The results, updated ratings, and final standings for all trials are combined and returned.
#'
#' @examples
#' \dontrun{
#'   fixtures <- data.frame(GW = rep(1:38, each = 10),
#'                          home_team = sample(letters[1:20], 380, replace = TRUE),
#'                          away_team = sample(letters[1:20], 380, replace = TRUE))
#'   results <- data.frame() # Initial results can be empty
#'   deductions <- data.frame(team = c("Team A", "Team B"), points_deducted = c(3, 1))
#'   team_ratings <- data.frame(team = rep(letters[1:20], each = 2),
#'                              venue = rep(c("home", "away"), 20),
#'                              expG = runif(40, 1, 2),
#'                              expGA = runif(40, 1, 2),
#'                              league_expG = runif(40, 1, 2),
#'                              games_played = sample(1:10, 40, replace = TRUE),
#'                              Total_smoothed_goals = runif(40, 10, 20),
#'                              Total_smoothed_goalsA = runif(40, 10, 20))
#'   simulations <- run_season_trials(trials = 100, fixtures, results, deductions, team_ratings,
#'                                    league_adj = TRUE, xG_factor = 0.6, smooth = TRUE, hot = TRUE)
#' }
#'
#' @export
run_season_trials = function(trials, fixtures, results, deductions, team_ratings, league_adj,xG_factor,smooth,hot){

  sim_results = vector('list', trials)
  sim_ratings = vector('list', trials)
  sim_standings = vector('list', trials)

  names(sim_results) = seq(1:trials)
  names(sim_ratings) = seq(1:trials)
  names(sim_standings) = seq(1:trials)

  for (trial in 1:trials) {

    sim_summary = simulate_season(fixtures = fixtures,
                                  results = results,
                                  deductions = deductions,
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
