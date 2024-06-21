#' Generate Match Impact Table
#'
#' This function generates a formatted table showing the impact of a match on various race outcomes (e.g., league championship, Champions League qualification, etc.).
#'
#' @param projection A data frame containing the projection data.
#' @param game_id An integer specifying the game ID.
#' @param race_teams A character vector specifying the teams involved in the race.
#' @param race_place A character string specifying the race place category (e.g., "champion", "UCL", "UEFA", "REL").
#'
#' @return A `gt` table object displaying the match impact.
#'
#' @details
#' The function calculates the match impact based on the projection data and formats the results into a `gt` table.
#' It highlights the maximum values in the numeric columns and includes a header with the match details and a footnote.
#'
#' The subtitle of the table provides information on the race place category:
#' - "champion": Odds to win the league given result.
#' - "UCL": Odds to make Champions League given result.
#' - "UEFA": Odds to make Europe given result.
#' - "REL": Odds to be relegated given result.
#'
#' The table is styled with `gt::tab_style()` to highlight the highest values in each numeric column with a light green background.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' projection_data <- your_projection_data_frame
#' game_id <- 123
#' race_teams <- c("TeamA", "TeamB")
#' race_place <- "champion"
#'
#' generate_match_impact_table(projection_data, game_id, race_teams, race_place)
#' }
generate_match_impact_table <- function(projection, game_id, race_teams, race_place) {

impact <- match_impact(projection,
                            game_id = game_id,
                            race_teams = race_teams,
                            race_place = race_place) %>%
          dplyr::select(-race)

match_teams <- match_impact_get_teams(projection, game_id)

subtitle_text <-  dplyr::case_when(
  race_place == "champion" ~ "Odds to win the league given result",
  race_place == "UCL" ~ "Odds to make champions Leaggue given result",
  race_place == "UEFA" ~ "Odds to makE europe given result",
  race_place == "REL" ~ "Odds to be relegated given result"
)

numeric_columns <- sapply(impact, is.numeric)

max_row_index <- function(col) {
  which.max(impact[[col]])
}

impact_table <- impact %>%
  gt::gt() %>%
  gt::fmt(
    columns = names(numeric_columns[numeric_columns]),
    fns = function(x) ifelse(x == 0, "-", scales::percent(x))
  ) %>%
  gt::tab_style(locations = gt::cells_body(
    columns = 2,
    rows = max_row_index(2)
  ),
  style =gt::cell_fill("lightgreen")
  ) %>%
  gt::tab_style(locations = gt::cells_body(
    columns = 3,
    rows =max_row_index(3)
  ),
  style =gt::cell_fill("lightgreen")
  ) %>%
  gt::tab_style(locations = gt::cells_body(
    columns =4,
    rows = max_row_index(4)
  ),
  style =gt::cell_fill("lightgreen")
  )%>%
  gt::tab_header(
    title = glue::glue("{match_teams$home_team} v {match_teams$away_team}  - Match impact"),
    subtitle = glue::glue(subtitle_text)
    )%>%
  gt::opt_align_table_header(align = "center")%>%
  gt::tab_footnote("@xGwhisperer", placement = "right")

return(impact_table)

}
