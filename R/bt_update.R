# Roxygen documentation

#' Get the latest version from GitHub.
#'
#' Syncs your local library with our latest GTA GitHub release.
#'
#' @return Be up to date with our latest functions.
#' @references www.globaltradealert.org
#' @author Global Trade Alert

bt_update = function(x){
  devtools::install_github("global-trade-alert/gtabastiat", force=T)
  library("gtabastiat")
  print("You are up to date.")
}
