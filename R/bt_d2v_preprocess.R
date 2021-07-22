#' Prepare a doc2vec matrix using a pretrained word2vec model.
#'
#'
#' @param model.w2v pretrained word embeddings
#' @param doc_id the doc ids, probably the BID with Bastiat
#' @param text character vector of words separated by spaces
#' @param as.df return as a dataframe? if F, returns a matrix
#'
#' @return
#' @export
#'
#' @examples
bt_d2v_preprocess = function(model.w2v, doc_id, text, as.df = T){

  library(word2vec)
  #results = sapply(text, function(text) str_split(string = text, pattern = " ", simplify = T))
  #names(results) = doc_id

  names(text) = doc_id

  result = doc2vec(model.w2v, newdata = text)

  if(as.df){
    return(as.data.frame(result))
  }else{
    #returns a matrix
    return(result)

  }
}
