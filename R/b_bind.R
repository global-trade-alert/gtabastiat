# Roxygen documentation

#' Bastiat, rbind two data frames.
#'
#' Shortcut to rbind two data frames with possibly diverging column names. Divergent columns are filled with NA's in the frame where it is absent.
#'
#' @return a data frame.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

b_bind <- function(x, y) {
  x.diff <- setdiff(colnames(x), colnames(y))
  y.diff <- setdiff(colnames(y), colnames(x))

  x[, c(as.character(y.diff))] <- NA
  y[, c(as.character(x.diff))] <- NA

  x <- as.data.frame(x)
  y <- as.data.frame(y)

  return(rbind(x, y))
}
