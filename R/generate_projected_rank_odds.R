#' Generate Projected Rank Odds Table
#'
#' This function generates a table displaying the projected rank odds for each team using the output from the season projection simulations.
#'
#' @param projection_outputs A list containing the outputs from the `run_projection` function, including mean projection odds.
#'
#' @return A gt table displaying the projected rank odds for each team.
#'
#' @details This function creates a visually styled table showing the projected rank odds for each team based on the simulation results. It includes the probability of each team finishing in each rank position, formatted as percentages.
#'
#' @examples
#' \dontrun{
#'   projection_outputs <- run_projection(trials = 100, matches, deductions, date = Sys.Date(),
#'                                        league_adj = TRUE, xG_factor = 0.6, smooth = TRUE, hot = TRUE,
#'                                        europe = 6, rel = 18)
#'   rank_odds_table <- generate_projected_rank_odds(projection_outputs)
#'   print(rank_odds_table)
#' }
#'
#' @export
generate_projected_rank_odds = function(projection_outputs){

diags = function(x) {gt::cells_body(columns = !!rlang::sym(x), rows = x)}
ranks = names(projection_outputs$mean_projection_odds)[7:26]

projection_outputs$mean_projection_odds %>%
  dplyr::select(-champion, -UCL, -UEFA, -MID, -REL)%>%
  dplyr::relocate(Rk, .before = team) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(dplyr::across(-c(Rk, team), ~dplyr::if_else(.x == 0,"-",scales::label_percent(accuracy = 0.1)(.x))))%>%
  gt::gt(rowname_col = "Rk")%>%
  gt::tab_header(
    title = glue::glue("Premier League Table Projections as at {projection_outputs$params %>% dplyr::pull(date)}"),
    subtitle= glue::glue("Projections based on {projection_outputs$params %>% dplyr::pull(trials)} simulations of remaining fixtures"))%>%
  gt::opt_align_table_header(align = "center")%>%
  gt::tab_source_note("Data: fbref") %>%
  gt::tab_footnote("Team ratings for simulations based on xG and actual goals scored.")%>%
  gt::tab_footnote("@xGwhisperer", placement = "auto") %>%
  gt::tab_style(locations = lapply(ranks, diags),
  style =gt::cell_fill("lightgreen"))

}
