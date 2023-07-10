#' get final projections from simulation standings
#'
#' @param sim_standings
#' @param mode
#'
#' @return
#' @export
#'
#' @examples
get_final_projection = function(sim_standings, mode){

  if(mode == 'mean'){
    final_standings = sim_standings %>%
      group_by(team) %>%
      mutate(across(.cols = c(Pts, G, GA),
                    .fns = ~round(mean(.x), 0))) %>%
      mutate(across(.cols = c(W, D, L, xG, xGA),
                    .fns = ~round(mean(.x), 1))) %>%
      select(-c(Rk, trial)) %>%
      ungroup() %>%
      mutate(GD = G - GA,
             xGD = xG - xGA) %>%
      distinct() %>%
      arrange(desc(Pts),
              desc(GD),
              desc(G)) %>%
      mutate(Rk = row_number()) %>%
      select(Rk, team, W, D, L,
             G, GA, GD, Pts, xG, xGA)
  }

  if(mode == 'median'){

    final_standings = sim_standings %>%
      group_by(team) %>%
      mutate(across(.cols = c(Pts, G, GA),
                    .fns = ~round(median(.x), 0))) %>%
      mutate(across(.cols = c(W, D, L, xG, xGA),
                    .fns = ~round(median(.x), 1))) %>%
      select(-c(Rk, trial)) %>%
      ungroup() %>%
      mutate(GD = G - GA,
             xGD = xG - xGA) %>%
      distinct() %>%
      arrange(desc(Pts),
              desc(GD),
              desc(G)) %>%
      mutate(Rk = row_number()) %>%
      select(Rk, team, W, D, L,
             G, GA, GD, Pts, xG, xGA)

  }

  final_standings

}
