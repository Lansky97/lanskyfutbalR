#' splits matches table to results, standings and remaining fixtures as at date
#'
#' @param matches
#' @param date
#' @param xTable
#' @param xG_factor
#' @param deductions
#'
#' @return
#' @export
#'
#' @examples
split_matches = function(matches, deductions, date = Sys.Date(), xG_factor, xTable = F){

  results = matches %>%
              matches_to_results(date = date)

  fixtures = matches %>%
              matches_to_fixtures(date = date)

  team_ratings = matches %>%
                  matches_to_team_ratings(date = date,
                                          xG_factor = xG_factor)

  standings = results %>%
                results_to_standings(deductions, xTable = xTable)

  list(results = results,
       fixtures = fixtures,
       team_ratings = team_ratings,
       standings = standings)

}
