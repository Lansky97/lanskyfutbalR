#' update standings with points deductions
#'
#' @param standings
#' @param deductions
#'
#' @return
#' @export
#'
#' @examples
apply_points_deductions = function(standings, deductions){

 standings%>%
    dplyr::left_join(deductions, by = "team") %>%
    dplyr::mutate(Pts = dplyr::if_else(is.na(points_deducted), Pts, Pts - points_deducted)) %>%
    dplyr::select(-points_deducted)

}
