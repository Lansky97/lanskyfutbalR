#' Simulate an Entire Season
#'
#' This function simulates the entire season based on given fixtures, initial results, points deductions, and team ratings.
#'
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
#' \item{results}{A data frame with the simulated match results for the entire season.}
#' \item{ratings}{A data frame with the updated team ratings after the season simulation.}
#' \item{standings}{A data frame with the final league standings after the season simulation.}
#'
#' @details This function simulates each game week in the season sequentially. For each game week, it calculates the expected goals, simulates the match outcomes, updates the team ratings, and compiles the results. At the end of the season, it combines the simulated results with the initial results (if any) to produce the final league standings.
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
#'   simulation <- simulate_season(fixtures, results, deductions, team_ratings,
#'                                 league_adj = TRUE, xG_factor = 0.6, smooth = TRUE, hot = TRUE)
#' }
#'
#' @importFrom dplyr filter bind_rows row_number
#' @export
simulate_season = function(fixtures, results, deductions, team_ratings, league_adj,xG_factor, smooth, hot){

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
                      results_to_standings(deductions = deductions, xTable = F)

  list(results = season_results,
       ratings = team_ratings,
       standings = season_standings)

}
