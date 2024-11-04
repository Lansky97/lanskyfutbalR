#' Generate Points Deductions Table
#'
#' This function creates a table of points deductions for teams based on the provided results and deductions data.
#'
#' @param results A data frame containing match results.
#' @param deductions A data frame containing points deductions for teams, with columns 'team' and 'points_deducted'. If NULL, no deductions are applied (default is NULL).
#'
#' @return A data frame containing teams and their respective points deductions.
#'
#' @details This function generates a table of unique teams from the match results and merges it with the points deductions data. If no deductions are provided, it initializes the points deductions to zero.
#'
#' @examples
#' \dontrun{
#'   results <- data.frame(...) # Your match results data here
#'   deductions <- data.frame(team = c("Team A", "Team B"), points_deducted = c(3, 1))
#'   deduction_table <- points_deductions_table(results, deductions)
#' }
#'
#' @export
points_deductions_table = function(results, deductions){

  home_teams = unique(results$home_team)
  away_teams = unique(results$away_team)

  teams = unique(home_teams, away_teams)

  if(is.null(deductions)){
    deductions = tibble::tibble(team = teams, points_deducted = 0)
  }

  deduction_table = tibble::tibble(team = teams) %>%
    dplyr::left_join(deductions, by = "team") %>%
    dplyr::mutate(points_deducted = dplyr::if_else(is.na(points_deducted), 0, points_deducted))


  return(deduction_table)
}
