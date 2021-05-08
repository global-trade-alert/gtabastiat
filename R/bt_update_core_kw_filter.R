#' Set relevant/classify based on keywords
#' Use bt_leads_df_kw_filter() keeping/discarding leads based on keywords
#'
#'
#' @param update.core dataframe to be filtered
#' @param key.words character vector of words/phrases to filter (regex)
#' @param positive if F, entries containing the key.words will be discarded.
#' @param filter.cols which columns to use for filtering
#' @param only.keep.matches if True, sets all relevant/classify to 0 first
#'
#' @return filtered dataframe
#' @export
#'
#' @examples bt_update_core_kw_filter(update.core, c("(import|export) tariff", "illegal trade practices"), filter.cols = "act.title.en")
bt_update_core_kw_filter = function(update.core,
                                    key.words,
                                    positive = T,
                                    filter.cols = c("act.title.en", "act.description.en"),
                                    case.sensitive = F,
                                    only.keep.matches=T){

  if(positive & only.keep.matches){
   update.core$relevant = 0
   update.core$classify = 0
  }

  filtered = update.core

  for(word in key.words){
    for(filter.column in filter.cols){

      match.vect = grepl(word,
                         update.core[filter.column],
                         ignore.case = !case.sensitive)

      if(positive){
        filtered$relevant[match.vect] <- 1 -> filtered$classify[match.vect]
      }else{
        filtered$relevant[match.vect] <- 0 -> filtered$classify[match.vect]
      }
    }

    }

    return(filtered)


}
