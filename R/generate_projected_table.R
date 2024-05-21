#' Generate Projected League Table
#'
#' This function generates a projected league table using the output from the season projection simulations.
#'
#' @param projection_outputs A list containing the outputs from the `run_projection` function, including mean final projections and mean projection odds.
#'
#' @return A gt table displaying the projected league standings with additional projection odds.
#'
#' @details This function creates a visually styled table showing the projected league standings. It includes columns for rank (Rk), team, points (Pts), goal difference (GD), and probabilities for winning the title, qualifying for the Champions League (CL), qualifying for the Europa League (EL), and relegation (REL). The table is styled with color coding for different ranks and includes a header and footnotes.
#'
#' @examples
#' \dontrun{
#'   projection_outputs <-
#'   run_projection(trials = 100, matches, deductions, date = Sys.Date(),
#'                  league_adj = TRUE, xG_factor = 0.6, smooth = TRUE,
#'                  hot = TRUE, europe = 6, rel = 18)
#'
#'   projected_table <- generate_projected_table(projection_outputs)
#'   print(projected_table)
#' }
#'
#' @importFrom gt gt tab_header opt_align_table_header tab_source_note tab_footnote tab_style cell_fill cell_text
#' @importFrom gt cells_body everything fmt
#' @importFrom scales percent
#' @importFrom glue glue
#'
#' @export
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
