#' Preprocess a document corpus into fixed-length vectors of integers, returned
#' as a data.frame or matrix. Error thrown if you don't have dedicated nVidia
#' GPU, this can be ignored.
#'
#' Requires Keras to work.
#'
#' @param num_words Desired size of vocabulary.
#' @param max_length Desired length of each doc. Shorter will be chopped. Longer
#'   will be zero-padded.
#' @param text The document corpus.
#' @param as.df Do you want a dataframe? If false, a matrix is returned.
#'
#' @return sparse document TD matrix, as a dataframe, or matrix if as.df=F
#' @references www.globaltradealert.org
#' @Author Callum Campbell for Global Trade Alert
#'   

bt_td_matrix_preprocess = function(num_words=15000, max_length=100, text, as.df=T){
  
  #keras is very good at this. for reference:
  #https://rdrr.io/cran/keras/man/text_tokenizer.html
  
  
  library(keras)
  
  tokeniser = text_tokenizer(num_words = num_words) %>% fit_text_tokenizer(text)
  
  text_seqs = texts_to_sequences(tokeniser, text)
  padded = text_seqs %>% pad_sequences(maxlen = max_length)
  
  if(as.df){
    return(as.data.frame(padded))
  } else {
    return(padded)
  }
}