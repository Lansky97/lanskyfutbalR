#' Generate projection table for projection table
#'
#' @param projection_ouputs
#'
#' @return
#' @export
#'
#' @examples
generate_projected_table = function(projection_outputs){

projection_outputs$mean_final_projections %>%
    dplyr::select(Rk, team, Pts, GD) %>%
    dplyr::left_join(projection_outputs$mean_projection_odds %>%
                              dplyr::select(Rk, team, Title = champion, CL = UCL, EL = UEFA, REL), by = c("Rk","team")) %>%
    gt::gt(rowname_col = "Rk")%>%
    gt::tab_header(
      title = glue::glue("Premier League Table Projections as at {projection_outputs$params %>% dplyr::pull(date)}"),
      subtitle= glue::glue("Projections based on {projection_outputs$params %>% dplyr::pull(trials)} simulations of remaining fixtures"))%>%
    gt::opt_align_table_header(align = "center")%>%
    gt::tab_source_note("Data: fbref") %>%
    gt::tab_footnote(glue::glue("Team ratings for simulations based on xG and actual goals scored.xG is weighted {100 * as.numeric(projection_outputs$params %>% dplyr::pull(xG_factor))}% in the ratings."))%>%
    gt::tab_footnote("@xGwhisperer", placement = "right")%>%
    gt::tab_style(locations = gt::cells_body(
      columns = gt::everything(),
      rows = "1"
    ),
    style =list(gt::cell_fill("darkgreen"), gt::cell_text("white"))
    )%>%
    gt::tab_style(locations = gt::cells_body(
      columns = gt::everything(),
      rows = c("2","3","4")
    ),
    style =gt::cell_fill("lightgreen")
    )%>%
    gt::tab_style(locations = gt::cells_body(
      columns = gt::everything(),
      rows = c("5","6")
    ),
    style =gt::cell_fill("skyblue")
    )%>%
    gt::tab_style(locations = gt::cells_body(
      columns = gt::everything(),
      rows = c("7")
    ),
    style =gt::cell_fill("lightblue")
    )%>%
    gt::tab_style(locations = gt::cells_body(
      columns = gt::everything(),
      rows = c("18","19","20")
    ),
    style = gt::cell_fill("pink")
    ) %>%
  gt::fmt(
    columns = c("Title","CL", "EL", "REL"),
    fns = function(x) dplyr::if_else(x == 0, "-", scales::percent(x, accuracy = 0.1))
  )

}
