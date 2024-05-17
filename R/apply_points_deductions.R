#' Apply Points Deductions to League Standings
#'
#' This function applies points deductions to the league standings based on a provided deductions table.
#'
#' @param standings A data frame containing the league standings, including a column for team names and points (Pts).
#' @param deductions A data frame containing the points deductions for teams, with columns for team names and points deducted (points_deducted).
#'
#' @return A data frame with the updated league standings after applying the points deductions. The points (Pts) column is adjusted based on the deductions, and the points_deducted column is removed.
#'
#' @details This function merges the league standings with the points deductions table and adjusts the points (Pts) for each team accordingly. If a team has no points deductions, their points remain unchanged.
#'
#' @examples
#' \dontrun{
#'   standings <- data.frame(team = c("Team A", "Team B", "Team C"),
#'                           Pts = c(50, 45, 40))
#'   deductions <- data.frame(team = c("Team B", "Team C"),
#'                            points_deducted = c(3, 1))
#'   updated_standings <- apply_points_deductions(standings, deductions)
#'   print(updated_standings)
#' }
#'
#' @export
apply_points_deductions = function(standings, deductions){

 standings%>%
    dplyr::left_join(deductions, by = "team") %>%
    dplyr::mutate(Pts = dplyr::if_else(is.na(points_deducted), Pts, Pts - points_deducted)) %>%
    dplyr::select(-points_deducted)

}
