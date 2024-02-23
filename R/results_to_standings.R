#' convert list of results to current standings
#'
#' @param results
#' @param xTable
#'
#' @return
#' @export
#'
#' @examples
results_to_standings = function(results, xTable = F){

  standings = results %>%
                tidyr::pivot_longer(-c(gameID, Date, GW, draw, xDraw),
                                       names_to = c('venue', '.value'),
                                       names_sep = '_') %>%
                dplyr::group_by(gameID) %>%
                dplyr::mutate(expA = sum(exp)-exp,
                              scoreA = sum(score) - score)%>%
                dplyr::ungroup()%>%
                dplyr::mutate(loss = dplyr::if_else(draw == 0 & win == 0,1,0),
                              xLoss = dplyr::if_else(xDraw == 0 & xWin == 0,1,0)) %>%
                dplyr::group_by(team, venue) %>%
                dplyr::mutate(dplyr::across(.cols = c(exp, expA,score, scoreA, xWin,win, xDraw, draw, xLoss, loss),
                                     .fns = ~sum(.x),
                                     .names = "total_{.col}")) %>%
                dplyr::mutate(MP = dplyr::n())%>%
                dplyr::ungroup()%>%
                dplyr::select(venue,team, MP,
                              W = total_win, D = total_draw, L = total_loss,
                              xW = total_xWin, xD = total_xDraw, xL = total_xLoss,
                              xG = total_exp, xGA = total_expA,
                              G = total_score, GA = total_scoreA) %>%
                dplyr::distinct() %>%
                dplyr::mutate(GD = G - GA, xGD = xG - xGA,
                              Pts = 3*W + D, xPts = 3*xW + xD) %>%
                dplyr::group_by(team) %>%
                dplyr::mutate(dplyr::across(.cols = -c(venue),
                                     .fns = ~sum(.x))) %>%
                dplyr::ungroup() %>%
                dplyr::select(-venue) %>%
                dplyr::distinct()


  if(!xTable){
  standings = standings  %>%
                dplyr::arrange(dplyr::desc(Pts),
                               dplyr::desc(GD),
                               dplyr::desc(G)) %>%
                dplyr::mutate(Rk = dplyr::row_number()) %>%
                dplyr::select(Rk, team, MP,
                              W, D, L, G, GA,
                              GD, Pts, xG, xGA, xGD)

  }

  if(xTable){
    standings = standings %>%
                  dplyr::arrange(dplyr::desc(xPts),
                                 dplyr::desc(xGD),
                                 dplyr::desc(xG)) %>%
                  dplyr::mutate(Rk =dplyr::row_number()) %>%
                  dplyr::select(Rk, team, MP,
                                xW, xD, xL, xG, xGA,
                                xGD, xPts, G, GA, GD)

  }

  standings

}
