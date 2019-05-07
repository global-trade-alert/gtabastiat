# Roxygen documentation

#' Bastiat, please reset yourself.
#'
#' Takes out the garbage, un & reloads packages
#'
#' @return Bastiat in mint condition.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------


b_reset <- function() {
  detach("rvest", unload=T, force=T)
  detach("httr", unload=T, force=T)
  detach("RCurl", unload=T, force=T)
  detach("XML", unload=T, force=T)
  detach("data.table", unload=T, force=T)
  detach("xlsx", unload=T, force=T)
  detach("mailR", unload=T, force=T)
  detach("stringr", unload=T, force=T)
  detach("tabulizer", unload=T, force=T)
  detach("htmltab", unload=T, force=T)
  detach("splitstackshape", unload=T, force=T)
  detach("webdriver", unload=T, force=T)
  detach("lubridate", unload=T, force=T)
  detach("textcat", unload=T, force=T)
  detach("flipTime", unload=T, force=T)
  detach("tidytext", unload=T, force=T)
  detach("gtabastiat", unload=T, force=T)
  detach("googleLanguageR", unload=T, force=T)

  gc()
  library("rvest")
  library("httr")
  library("RCurl")
  library("XML")
  library("data.table")
  library("xlsx")
  library("mailR")
  library("stringr")
  library("tabulizer")
  library("htmltab")
  library("splitstackshape")
  library("webdriver")
  library("lubridate")
  library("textcat")
  library("flipTime")
  library("tidytext")
  library("gtabastiat")
  library("googleLanguageR")
}

