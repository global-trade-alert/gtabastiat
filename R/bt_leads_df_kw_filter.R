#' Keep/discard leads containing specified keywords
#' Use bt_update_core_kw_filter() for setting the relevant/classify instead
#'
#'
#' @param leads.df dataframe to be filtered
#' @param key.words character vector of words/phrases to filter (regex)
#' @param positive if F, entries containing the key.words will be discarded.
#' @param filter.cols which columns to use for filtering
#'
#' @return filtered dataframe
#' @export
#'
#' @examples bt_leads_df_kw_filter(update.table, c("(import|export) tariff", "illegal trade practices"), filter.cols = "act.title.en")
bt_leads_df_kw_filter = function(leads.df,
                                 key.words,
                                 positive = T,
                                 filter.cols = c("act.title.en", "act.description.en"),
                                 case.sensitive = F){

  filtered = leads.df[0,]


  if(positive){
    for(word in key.words){
      for(filter.column in filter.cols){

        filtered = rbind(filtered,
                         subset(leads.df, grepl(word,
                                                unlist(leads.df[filter.column]),
                                                ignore.case = !case.sensitive)
                         )
        )

      }
    }
  } else {
    for(word in key.words){
      for(filter.column in filter.cols){

        filtered = rbind(filtered,
                         subset(leads.df, !grepl(word,
                                                 unlist(leads.df[filter.column]),
                                                 ignore.case = !case.sensitive)
                         )
        )

      }
    }
  }

  filtered=subset(filtered, !duplicated(unlist(filtered[filter.cols[1]])))


  return(filtered)


}
