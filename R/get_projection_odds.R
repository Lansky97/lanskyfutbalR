
#' get projection_odds
#'
#' @param final_projections
#' @param sim_standings
#' @param europe
#' @param rel
#'
#' @return
#' @export
#'
#' @examples
get_projection_odds = function(final_projections, sim_standings, trials, europe = 6, rel =18){

  final_ranks = final_projections %>%
    dplyr::select(team, Rk)

  sim_standings %>%
    dplyr::group_by(team, Rk)%>%
    dplyr::mutate(chance = n()/trials) %>%
    dplyr::select(team, Rk, chance) %>%
    dplyr::ungroup()%>%
    dplyr::mutate(chance_keep = chance,
                  flag = case_when(
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
