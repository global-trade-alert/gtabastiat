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
  library(digest)
  library(glue)

  tr.string = str_trunc(string, 275, ellipsis = "")

  tr.result=gl_translate(as.character(tr.string))

  source_lang = tr.result$detectedSourceLanguage
  tr_text = tr.result$text %>% gsub(pattern = "'", replacement = "\\\\'")
  tr_hash = digest(tr.string) #this should == tr.result$text
  num_chars = nchar(tr.string)


# db upload ---------------------------------------------------------------


  source("setup/keys/ric.R")
  con=dbConnect(drv = RMariaDB::MariaDB(),
                host = db.host,
                username = db.user,
                password = db.password,
                dbname = db.name)


  tr.sql = glue("INSERT INTO bt_translation_log (source_text, source_lang, text_hash, num_chars)
  VALUES(N'{tr_text}', '{source_lang}', '{tr_hash}', {num_chars})")

  test = dbExecute(con, statement = tr.sql)

  dbDisconnect(con)



  return(tr.result)



}
