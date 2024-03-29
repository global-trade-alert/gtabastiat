#' Set relevant/classify based on keywords
#' Use bt_leads_df_kw_filter() keeping/discarding leads based on keywords
#'
#'
#' @param update.core dataframe to be filtered
#' @param key.words character vector of words/phrases to filter (regex)
#' @param positive if F, entries containing the key.words will be discarded.
#' @param filter.cols which columns to use for filtering
#' @param only.keep.matches if True, sets all relevant/classify to 0 first
#' @param save.match.words if True, add a column "key.words" containing all the words that generated matches
#' @param assign.relevance if True, assign the relevant and classify 0/1 to the returned df. default is TRUE
#'
#' @return filtered dataframe
#' @export
#'
#' @examples bt_update_core_kw_filter(update.core, c("(import|export) tariff", "illegal trade practices"), filter.cols = "act.title.en")
bt_update_core_kw_filter = function(update.core,
                                    key.words,
                                    positive = T,
                                    filter.cols = c("act.title.en", "act.description.en", "act.title.ll", "act.description.ll"),
                                    case.sensitive = F,
                                    only.keep.matches=T,
                                    save.match.words = F,
                                    assign.relevance = T){


  library(data.table)

  filtered = update.core

  filter.cols = filter.cols[filter.cols %in% colnames(filtered)]
  message(paste("Filtering on:", paste(filter.cols, collapse = ", ")))

  filtered$match.words = NA

  if(positive & only.keep.matches){
    filtered$relevant = 0
    filtered$classify = 0
  }


  for(word in key.words){
    for(filter.column in filter.cols){

      match.vect = grepl(word,
                         unlist(update.core[filter.column]),
                         ignore.case = !case.sensitive)

      if(assign.relevance){
        if(positive){
          filtered$relevant[match.vect] <- 1 -> filtered$classify[match.vect]

          #save the word for context/meta-analysis

          #create a vector to index rows that matched and do NOT already contain the word
          add.words.vect = (match.vect & !grepl(word, filtered$match.words))
          filtered$match.words[add.words.vect] = paste(filtered$match.words[add.words.vect], word, sep = ", ")


        }else{
          filtered$relevant[match.vect] <- 0 -> filtered$classify[match.vect]
        }
      }
    }

  }

  filtered$match.words[!is.na(filtered$match.words)] = gsub(pattern = "NA, ",
                                                            replacement = "",
                                                            x = filtered$match.words[!is.na(filtered$match.words)])

  setnames(filtered, "match.words", "key.words")

  if(!save.match.words){
    filtered$key.words = NULL
  }

  return(filtered)


}
