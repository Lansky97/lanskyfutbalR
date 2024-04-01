#' Generate points deduction table
#'
#' @param results
#' @param deductions
#'
#' @return
#' @export
#'
#' @examples
points_deductions_table = function(results, deductions){

  home_teams = unique(results$home_team)
  away_teams = unique(results$away_team)

  teams = unique(home_teams, away_teams)

  if(is.null(deductions)){
    deduction_table = tibble::tibble(team = teams, points_deducted = 0)
  }

  deduction_table = tibble::tibble(team = teams) %>%
    dplyr::left_join(deductions, by = "team") %>%
    dplyr::mutate(points_deducted = dplyr::if_else(is.na(points_deducted), 0, points_deducted))


  return(deduction_table)
}
