#' checks scraped data is ready for upload
#'
#' Ensures:
#' update.table exists
#' the 17 required vars are there
#' the date is correctly formatted as a date
#' the BIDs are unique
#' the titles are unique
#' the URLs are unique
#'
#' 17 required vars are:
#' "act.date", "act.title.en", "act.description.en","act.title.ll", "act.description.ll",
#' "act.url", "act.id", "country", "country.lead","email.language","act.values", "background.url",
#' "acting.agency", "relevant", "classify","collection.date", "bid"
#'
#'
#' @param ignore.url set this as T if you know your source only has one URL shared for all leads.
#'
#' @return 'OK!' if all is well, error if not.
#' @export
#'
#' @examples
bt_scraper_checkpoint_2 = function(ignore.url = F){


  check.update.table = exists("update.table")


  #17 essential vars
  required.vars = c("act.date", "act.title.en", "act.description.en",
                    "act.title.ll", "act.description.ll",
                    "act.url", "act.id", "country", "country.lead",
                    "email.language","act.values", "background.url",
                    "acting.agency", "relevant", "classify",
                    "collection.date", "bid")

  check.update.table.vars = all(required.vars %in% colnames(update.table))
  if(!check.update.table.vars){
    missing.vars = required.vars[which(!required.vars %in% colnames(update.table))]
    missing.vars = paste("missing: ", paste(missing.vars, collapse = ", "))
    }

  check.date.format = tryCatch(expr = {
    as.Date(update.table$act.date[1])
    TRUE
  },
  error = function(e){FALSE},
  warning = function(w){FALSE}
  )


  check.unique.bids = ! any(duplicated(update.table$bid))
  check.unique.titles = ! any(duplicated(update.table$act.title.en))


  check.unique.urls = (! any(duplicated(update.table$act.url))) | ignore.url



  checks = c(check.update.table = check.update.table,
             check.update.table.vars = check.update.table.vars,
             check.date.format = check.date.format,
             check.unique.bids = check.unique.bids,
             check.unique.titles = check.unique.titles,
             check.unique.urls = check.unique.urls)


  if(all(checks)){print("OK!")
  }else{
    if(! exists("missing.vars")){
      missing.vars = ""
    }
    error.text = paste(names(checks)[which(!checks)], "failed!", collapse = "\n")
    stop(paste(error.text, missing.vars))
  }


}
