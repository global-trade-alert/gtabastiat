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
                                 filter.cols = c("act.title.en", "act.description.en", "act.title.ll", "act.description.ll"),
                                 case.sensitive = F){



  message("this can be a bit slow for dfs containing long character vectors")


  filter.cols = filter.cols[which(filter.cols %in% colnames(leads.df))]

  if(is.na(filter.cols)[1]){
    stop("filter cols provided were not found in the target datafame")
  }

  if(positive){
    #add matches constructively
    filtered = leads.df[0,]
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
    print(paste("retained", (nrow(leads.df) - nrow(filtered)), "rows"))
  } else {
    #negative, i.e.
    #remove matches each iteration

    filtered = leads.df

    for(word in key.words){
      for(filter.column in filter.cols){

        #=TRUE for entries containing the negative words so we want to remove these
        match.vect = !grepl(word,
                            unlist(filtered[filter.column]),
                            ignore.case = !case.sensitive)

        filtered = filtered[match.vect, ]




      }
    }
    print(paste("removed", (nrow(leads.df) - nrow(filtered)), "rows"))
  }

  filtered=subset(filtered, !duplicated(unlist(filtered[filter.cols[1]])))


  return(filtered)


}
