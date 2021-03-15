#' Check the initial scrape worked properly
#'
#' This function ensures:
#' getArticles() exists
#' table.main exists
#' the table.main has the 5 vars it should, correctly named and in the correct format
#' (this is particularly pertinent for the act.date)
#'
#' the required vars are:
#' "act.date", "act.title.en", "act.description.en", "act.url", "act.id"
#'
#' @return 'OK!' if all is well, error if not.
#' @export
#'
#' @examples
bt_scraper_checkpoint_1 = function(){


  check.getArticles = exists("getArticles")
  check.table.main = exists("table.main")

  #checking vars in table
  required.vars.en = c("act.date", "act.title.en", "act.description.en", "act.url", "act.id")
  required.vars.ll = c("act.date", "act.title.ll", "act.description.ll", "act.url", "act.id")


  check.table.main.vars = (all(required.vars.en %in% colnames(table.main)) |
                             all(required.vars.ll %in% colnames(table.main)))



  check.date.format = tryCatch(expr = {
    as.Date(table.main$act.date[i])
    all(! is.na(table.main$act.date)) &
     all(! is.null(table.main$act.date)) &
     all(! sapply(table.main$act.date, length) < 1)
  },
  error = function(e){FALSE},
  warning = function(w){FALSE}
  )

  checks = c(check.getArticles = check.getArticles,
             check.table.main = check.table.main,
             check.table.main.vars = check.table.main.vars,
             check.date.format = check.date.format)


  if(all(checks)){print("OK!")
  }else{
    stop(paste(names(checks)[which(!checks)], "failed!", collapse = "\n"))
  }


}
