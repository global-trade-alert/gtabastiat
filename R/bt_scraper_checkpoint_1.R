bt_scraper_checkpoint_1 = function(){


  check.getArticles = exists("getArticles")
  check.table.main = exists("table.main")

  #checking vars in table
  required.vars.en = c("act.date", "act.title.en", "act.description.en", "act.url", "act.id")
  required.vars.ll = c("act.date", "act.title.ll", "act.description.ll", "act.url", "act.id")


  check.table.main.vars = (all(required.vars.en %in% colnames(table.main)) |
                             all(required.vars.ll %in% colnames(table.main)))

  checks = c(check.getArticles = check.getArticles,
             check.table.main = check.table.main,
             check.table.main.vars = check.table.main.vars)


  if(all(checks)){print("OK!")
  }else{
    stop(paste(names(checks)[which(!checks)], "failed!", collapse = "\n"))
  }


}
