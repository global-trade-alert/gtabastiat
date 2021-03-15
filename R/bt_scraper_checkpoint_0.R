#' Check a scraper has initialised
#'
#' Ensures:
#' pjs initialised with browser object called 'remDr'
#' main.url properly initialised
#' stock.data.file is specified
#'
#' @return 'OK!' if all is well, error if not.
#' @export
#'
#' @examples
bt_scraper_checkpoint_0 = function(){


  check.remDr = exists("remDr")
  check.pjs = exists("pjs")
  check.urls = (exists("main.url") | exists("mutiple.urls"))
  check.stock.data.file = grepl(".+\\.Rdata$", stock.data.file)


  checks = c(check.remDr = check.remDr,
             check.pjs = check.pjs,
             check.urls = check.urls,
             check.stock.data.file = check.stock.data.file)


  if(all(checks)){print("OK!")
  }else{
    stop(paste(names(checks)[which(!checks)], "failed!", collapse = "\n"))
  }


}
