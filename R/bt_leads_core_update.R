# Roxygen documentation

#' Get the latest version from GitHub.
#'
#' Syncs your local library with our latest GTA GitHub release.
#'
#' @return Be up to date with our latest functions.
#' @references www.globaltradealert.org
#' @author Global Trade Alert

bt_leads_core_update = function(update.df){

  ## removing URLs already recorded
  query="SELECT act_url
  FROM bt_leads_core"

  lc.urls=unique(gta_sql_get_value(query))
  rm(query)

  lc.update=subset(update.df, ! act.url %in% lc.urls)

  if(nrow(lc.update)==0){
    print("All leads recorded already.")

    } else{

      ## restricting to necessary columns
      lc.cols=names(gta_sql_get_value("SELECT *
                                   FROM bt_leads_core
                                   LIMIT 1;"))

      lc.update=lc.update[,lc.cols]

      ## adding leads-checker columns
      lc.update$relevance.probability=NA
      lc.update$relevance.probability[lc.update$classify==0 & lc.update$relvant==1]=1
      lc.update$relevance.probability[lc.update$classify==0 & lc.update$relvant==0]=0

      lc.update$bin.check=as.numeric(lc.update$relevant==1)
      lc.update$bin.recovered=0

      ## upload
      gta_sql_append_table(append.table = "leads.core",
                                   append.by.df = "lc.update")
      rm(lc.update)

      print(past0(nrow(lc.update), " new records added."))

    }



}
