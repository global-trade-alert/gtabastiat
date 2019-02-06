# Roxygen documentation

#' Bastiat, pool variables from two models.
#'
#' Create a model based on two old ones (plus some randomness).
#'
#' @return A vector of variable names.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

b_var_pool <- function(x, y) {
  nr.vars=round(mean(c(length(x), length(y))),0)

  common.vars=intersect(x,y)
  uncommon.vars=c(x,y)[c(x,y)%in% common.vars==F]

  uncommon.choice=sample(1:length(uncommon.vars),nr.vars-length(common.vars), replace=F)

  return(c(common.vars,uncommon.vars[uncommon.choice]))
}
