# Roxygen documentation

#' Bastiat, evaluate this pasted command.
#'
#' Shortcut for "eval(parse(text=paste(x)))".
#'
#' @return Whatever you sought to evaluate/execute.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

b_eval = function(x) {
  return(eval(parse(text=gsub("(^.*?)=","\\1<<-",x))))
}
