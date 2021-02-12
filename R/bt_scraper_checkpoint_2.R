bt_scraper_checkpoint_2 = function(){


  check.update.table = exists("update.table")


  #17 essential vars
  required.vars = c("act.date", "act.title.en", "act.description.en",
                    "act.title.ll", "act.description.ll",
                    "act.url", "act.id", "country", "country.lead",
                    "email.language","act.values", "background.url",
                    "acting.agency", "relevant", "classify",
                    "collection.date", "bid")

  check.update.table.vars = all(required.vars %in% colnames(update.table))

  check.date.format = tryCatch(expr = {
    as.Date(update.table$act.date[1])
    TRUE
  },
  error = function(e){FALSE},
  warning = function(w){FALSE}
  )


  check.unique.bids = ! any(duplicated(update.table$bid))
  check.unique.titles = ! any(duplicated(update.table$act.title.en))
  check.unique.urls = ! any(duplicated(update.table$act.url))



  checks = c(check.update.table = check.update.table,
             check.update.table.vars = check.update.table.vars,
             check.date.format = check.date.format,
             check.unique.bids = check.unique.bids,
             check.unique.titles = check.unique.titles,
             check.unique.urls = check.unique.urls)


  if(all(checks)){print("OK!")
  }else{
    stop(paste(names(checks)[which(!checks)], "failed!", collapse = "\n"))
  }


}
