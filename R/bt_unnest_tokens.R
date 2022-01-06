#' Make a df of tokens from each supplied document with per doc frequency
#'
#' Inlcudes bigrams and trigrams based on those words.
#'
#' @param doc.id
#' @param text
#'
#' @return
#' @export
#'
#' @examples
bt_unnest_tokens=function(doc.id, text){

  library(udpipe)

  #prepare input
  x.train.tfidf = document_term_frequencies(x = text,
                                            document = doc.id)

  #filter the tokens
  x.train.tfidf = subset(x.train.tfidf, nchar(term)>1)
  #add any more pireuta here

  #n-grams
  x.train.tfidf$token.2gram = txt_nextgram(x = x.train.tfidf$term, n = 2)
  x.train.tfidf$token.3gram= txt_nextgram(x = x.train.tfidf$term, n = 3)
  #x.train.tfidf$token.4gram= txt_nextgram(x = x.train.tfidf$term, n = 4)
  #etc...

  x.train.tfidf = document_term_frequencies(x = x.train.tfidf,
                                            document = "doc_id",
                                            term = c("term", "token.2gram", "token.3gram"))
  return(x.train.tfidf)
}
