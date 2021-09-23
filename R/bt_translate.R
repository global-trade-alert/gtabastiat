#' Wrapper for google translate to prevent overtranslation
#'
#' after some analysis, it was determined that 200-300 is the best amount of
#' characters to translate to make accurate predictions, see
#' projects/translation stats for some graphs and stuff.
#'
#' @param string string to be translated
#' @param trunc amount of characters to limit it to
#'
#' @return exactly the same as googleLanguageR::gl_translate
bt_translate = function(string, trunc = 275){

  library(stringr)
  library(googleLanguageR)

  tr.string = str_trunc(string, 275, ellipsis = "")

  result=gl_translate(as.character(tr.string))

  return(result)



}
