#' Basic preprocessing of text for classification/training
#'
#' Takes text and returns it lowercase, minus punctuation and stopwords.
#'
#'
#' @param text string or character vector
#' @param stop.rm remove stopwords. set to F for e.g. acting.agency
#' @return
#' @export
#'
#' @examples
bt_text_preprocess = function(text, stop.rm = T){

  library(tm)

  # return(text %>%
  #          removePunctuation() %>%
  #          tolower() %>%
  #          removeWords(tm::stopwords(kind = "en")) %>%
  #          gsub(pattern = "\\s+", replacement = " ")
  # )

  library(reticulate)

  ftfy=reticulate::import("ftfy")

  #must be a loop as ftfy can't do vectors afaik
  print("fixing mojibake...")
  for(i in 1:length(text)){
    text[i] = ftfy$fix_text(text[i])
  }

  return(text %>%
           gsub(pattern = "< ?br[\\s\\S]+", replacement = "", perl = T) %>%
           removePunctuation() %>%
           tolower() %>%
           removeWords(tm::stopwords(kind = "en")) %>%
           gsub(pattern = "[‒–—]", replacement = "") %>%
           gsub(pattern = "[^a-z]", replacement = " ") %>%
           gsub(pattern = "\\s+", replacement = " ") %>%
           trimws()

  )




}

