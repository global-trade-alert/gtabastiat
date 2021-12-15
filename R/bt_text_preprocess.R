#' Basic preprocessing of text for classification/training
#'
#' Takes text and returns it lowercase, minus punctuation and stopwords.
#'
#'
#' @param text string or character vector
#'
#' @return
#' @export
#'
#' @examples
bt_text_preprocess = function(text){

  library(tm)

  # return(text %>%
  #          removePunctuation() %>%
  #          tolower() %>%
  #          removeWords(tm::stopwords(kind = "en")) %>%
  #          gsub(pattern = "\\s+", replacement = " ")
  # )

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

