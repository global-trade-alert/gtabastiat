#' Get the intersection of two sets of xpath nodes
#'
#' @param ns1 first node set
#' @param ns2 second node set
#'
#' @return common nodes to the two sets
#' @export
#'
#' @examples
bt_xpath_nodes_intersect = function(ns1, ns2){
  return(paste0(
    ns1, "[count(.| ", ns2, ")=count(", ns2, ")]"
  ))
}
