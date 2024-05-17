#' Get Final Projection from Simulation Standings
#'
#' This function calculates the final projected standings from the simulation standings using either the mean or median of the simulation results.
#'
#' @param sim_standings A data frame containing the simulation standings from multiple trials, including columns for points (Pts), goals (G), goals against (GA), wins (W), draws (D), losses (L), expected goals (xG), and expected goals against (xGA).
#' @param mode A character string specifying whether to use the 'mean' or 'median' for the final projection (default is 'mean').
#'
#' @return A data frame with the final projected standings, including columns for rank (Rk), team, wins (W), draws (D), losses (L), goals (G), goals against (GA), goal difference (GD), points (Pts), expected goals (xG), and expected goals against (xGA).
#'
#' @details This function calculates the final projected standings by taking the mean or median of the relevant columns from the simulation standings. It then ranks the teams based on points, goal difference, and goals.
#'
#' @examples
#' \dontrun{
#'   sim_standings <- data.frame(team = rep(letters[1:20], each = 100),
#'                               Pts = sample(30:90, 2000, replace = TRUE),
#'                               G = sample(30:90, 2000, replace = TRUE),
#'                               GA = sample(30:90, 2000, replace = TRUE),
#'                               W = sample(10:30, 2000, replace = TRUE),
#'                               D = sample(5:15, 2000, replace = TRUE),
#'                               L = sample(5:15, 2000, replace = TRUE),
#'                               xG = runif(2000, 30, 90),
#'                               xGA = runif(2000, 30, 90),
#'                               Rk = rep(1:20, each = 100),
#'                               trial = rep(1:100, times = 20))
#'   final_projection <- get_final_projection(sim_standings, mode = 'mean')
#' }
#'
#' @export
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
