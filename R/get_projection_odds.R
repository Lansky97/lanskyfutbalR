#' Calculate Projection Odds from Final Projections
#'
#' This function calculates the odds of different outcomes (e.g., champion, UCL, UEFA, relegation) based on the final projections and simulation standings.
#'
#' @param final_projections A data frame containing the final projected standings, including columns for team and rank (Rk).
#' @param sim_standings A data frame containing the simulation standings from multiple trials.
#' @param trials An integer specifying the number of trials run.
#' @param europe An integer specifying the cutoff rank for UEFA competitions (default is 6).
#' @param rel An integer specifying the cutoff rank for relegation (default is 18).
#'
#' @return A data frame with the calculated odds of different outcomes for each team, including columns for team, rank (Rk), and odds for champion, UCL, UEFA, MID, and REL.
#'
#' @details This function calculates the odds of different outcomes for each team based on the ranks achieved in the simulation standings. It then summarizes these odds for each team.
#'
#' @examples
#' \dontrun{
#'   final_projections <- data.frame(team = letters[1:20], Rk = 1:20)
#'   sim_standings <- data.frame(team = rep(letters[1:20], each = 100),
#'                               Rk = sample(1:20, 2000, replace = TRUE))
#'   trials <- 10
#'   projection_odds <- get_projection_odds(final_projections, sim_standings, trials)
#' }
#'
#' @importFrom dplyr case_when
#' @export
get_projection_odds = function(final_projections, sim_standings, trials, europe = 6, rel =18){

  final_ranks = final_projections %>%
    dplyr::select(team, Rk)

  sim_standings %>%
    dplyr::group_by(team, Rk)%>%
    dplyr::mutate(chance = dplyr::n()/trials) %>%
    dplyr::select(team, Rk, chance) %>%
    dplyr::ungroup()%>%
    dplyr::mutate(chance_keep = chance,
                  flag = dplyr::case_when(
                            Rk ==1 ~ "champion",
                            Rk <=4 ~ "UCL",
                            Rk <= europe ~ "UEFA",
                            Rk >= rel ~ "REL",
                            TRUE ~ "MID")) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = flag,
                values_from = chance,
                values_fill = 0) %>%
    tidyr::pivot_wider(names_from = Rk,
                values_from = chance_keep,
                values_fill = 0) %>%
    dplyr::mutate(UCL = champion + UCL) %>%
    dplyr::left_join(final_ranks, by= 'team')%>%
    dplyr::group_by(team) %>%
    dplyr::mutate(dplyr::across(.cols = -Rk,
                                .fns = ~sum(.x))) %>%
    dplyr::distinct() %>%
    dplyr::arrange(Rk)

}
