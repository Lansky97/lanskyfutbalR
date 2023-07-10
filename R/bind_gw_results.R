#' Title
#'
#' @param gw_results_list
#'
#' @return
#' @export
#'
#' @examples
bind_gw_results = function(gw_results_list){

  sim_results = gw_results_list %>%
                  dplyr::bind_rows()

  sim_results

}
