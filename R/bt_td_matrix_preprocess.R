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

  #load tokeniser. load_text_tokeniser has to setwd() first for it to work annoyingly
  library(keras)
  current.wd = getwd()
  setwd("content/0 core/Mrs Hudson/")

  mrs.hudson.tokeniser.list = list.files()[grepl("Mrs Hudson tokeniser", list.files())]
  mrs.hudson.tokeniser.file.name = mrs.hudson.tokeniser.list[length(mrs.hudson.tokeniser.list)]
  mrs.hudson.tokeniser = load_text_tokenizer(file = mrs.hudson.tokeniser.file.name)

  setwd(current.wd)

  if(!exists("mrs.hudson.tokeniser")){
    stop("Mrs Hudson's text tokeniser not loaded! Text cannot be tokenised for preprocessing.")
  }

  text_seqs = texts_to_sequences(mrs.hudson.tokeniser, text)
  padded = text_seqs %>% pad_sequences(maxlen = max_length)

  if(as.df){
    return(as.data.frame(padded))
  } else {
    return(padded)
  }
}
