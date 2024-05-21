#' Run Season Projection
#'
#' This function runs multiple trials of season projections based on given matches, deductions, and team ratings, and calculates various projections and odds.
#'
#' @param trials An integer specifying the number of trials to run.
#' @param matches A data frame containing the match data.
#' @param deductions A data frame containing points deductions for teams.
#' @param date The date for filtering matches (default is the current date).
#' @param league_adj A logical indicating whether to apply league-wide adjustments to the expected goals calculations (default is FALSE).
#' @param xG_factor A numeric factor used to smooth the goals with expected goals (xG).
#' @param smooth A logical indicating whether to apply smoothing to the goals (default is TRUE).
#' @param hot A logical indicating whether to update the team ratings with the current game week results (default is TRUE).
#' @param europe An integer specifying the cutoff rank for UEFA competitions (default is 6).
#' @param rel An integer specifying the cutoff rank for relegation (default is 18).
#'
#' @return A list containing:
#' \item{match_odds}{A data frame with the calculated match odds for each fixture.}
#' \item{starting_team_ratings}{A data frame with the initial team ratings.}
#' \item{starting_standings}{A data frame with the initial league standings.}
#' \item{simmed_results}{A data frame with the simulated match results for all trials.}
#' \item{simmed_final_ratings}{A data frame with the updated team ratings after all trials.}
#' \item{simmed_final_standings}{A data frame with the final league standings after all trials.}
#' \item{mean_final_projections}{A data frame with the final projected standings using mean calculations.}
#' \item{mean_projection_odds}{A data frame with the calculated odds based on mean projections.}
#' \item{median_final_projections}{A data frame with the final projected standings using median calculations.}
#' \item{median_projection_odds}{A data frame with the calculated odds based on median projections.}
#' \item{params}{A tibble with the parameters used for the projection.}
#' \item{deductions}{A data frame with the points deductions applied.}
#'
#' @details This function splits the match data into fixtures, team ratings, standings, and results. It then runs multiple season trials, calculates final projections and odds using both mean and median methods, and compiles the results.
#'
#' @examples
#' \dontrun{
#'   matches <- data.frame(GW = rep(1:38, each = 10),
#'                         home_team = sample(letters[1:20], 380, replace = TRUE),
#'                         away_team = sample(letters[1:20], 380, replace = TRUE),
#'                         Date = Sys.Date() + rep(1:38, each = 10),
#'                         HomeGoals = sample(0:5, 380, replace = TRUE),
#'                         AwayGoals = sample(0:5, 380, replace = TRUE),
#'                         Home_xG = runif(380, 0.5, 2.5),
#'                         Away_xG = runif(380, 0.5, 2.5))
#'   deductions <- data.frame(team = c("Team A", "Team B"), points_deducted = c(3, 1))
#'   projections <- run_projection(trials = 100, matches, deductions, date = Sys.Date(),
#'                                 league_adj = TRUE, xG_factor = 0.6, smooth = TRUE, hot = TRUE,
#'                                 europe = 6, rel = 18)
#' }
#'
#' @importFrom tibble tibble
#' @export
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
