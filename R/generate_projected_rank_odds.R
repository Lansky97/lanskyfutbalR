#' Generate Projected rank odds tabke
#'
#' @param projection_outputs
#'
#' @return
#' @export
#'
#' @examples
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
