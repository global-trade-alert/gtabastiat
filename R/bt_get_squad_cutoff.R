#' Returns current cutoff value used by the BT squad
#'
#' Like a standard getter function.
#'
#' @return the cutoff value used by the squad to determine relevance
#'
bt_get_squad_cutoff = function(){
  load(squad.classifier)
  return(cutoff)

}
