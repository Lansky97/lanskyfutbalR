#' get final projections from simulation standings
#'
#' @param sim_standings
#' @param mode
#'
#' @return
#' @export
#'
#' @examples
get_final_projection = function(sim_standings, mode = 'mean'){

  if(mode == 'mean'){
    final_standings = sim_standings %>%
      dplyr::group_by(team) %>%
      dplyr::mutate(dplyr::across(.cols = c(Pts, G, GA),
                                  .fns = ~round(mean(.x), 0))) %>%
      dplyr::mutate(dplyr::across(.cols = c(W, D, L, xG, xGA),
                                  .fns = ~round(mean(.x), 1))) %>%
      dplyr::select(-c(Rk, trial)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(GD = G - GA,
                    xGD = xG - xGA) %>%
      dplyr::distinct() %>%
      dplyr::arrange(dplyr::desc(Pts),
                     dplyr::desc(GD),
                     dplyr::desc(G)) %>%
      dplyr::mutate(Rk = dplyr::row_number()) %>%
      dplyr::select(Rk, team, W, D, L,
             G, GA, GD, Pts, xG, xGA)
  }

  if(mode == 'median'){
#TODO: choose median trial
    final_standings = sim_standings %>%
      dplyr::group_by(team) %>%
      dplyr::mutate(dplyr::across(.cols = c(Pts, G, GA),
                                    .fns = ~round(median(.x), 0))) %>%
      dplyr::mutate(dplyr::across(.cols = c(W, D, L, xG, xGA),
                                  .fns = ~round(median(.x), 1))) %>%
      dplyr::select(-c(Rk, trial)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(GD = G - GA,
                    xGD = xG - xGA) %>%
      dplyr::distinct() %>%
      dplyr::arrange(dplyr::desc(Pts),
                     dplyr::desc(GD),
                     dplyr::desc(G)) %>%
      dplyr::mutate(Rk =dplyr::row_number()) %>%
      dplyr::select(Rk, team, W, D, L,
                    G, GA, GD, Pts, xG, xGA)

  }

  final_standings

}
