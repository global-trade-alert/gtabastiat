#' Guesses the date from a string
#'
#' Feed me a string with a date and I'll try to return a date formatted value.
#'
#' The target string should contain a day value as a 1- or 2-digit number, a
#' year as a 4-digit number and a month as a word (e.g. 'December').
#'
#' The month guessing uses bt_guess_month() and thus supports all the languages
#' which that function can use.
#'
#' I have not seen any instances where the date is stored with the year as two
#' digits. If this is problematic I can implement something to catch these
#' cases.
#'
#'
#' @param tgt.string a string containing a potential date in the appropriate
#'   format
#'
#' @return date in date format
#'
#' @examples
#' bt_guess_month("23eme Aout, 2021")
#' #returns "2021-08-23"
bt_guess_date = function(tgt.string){

  library(stringr)

  tryCatch(expr = {
  tgt.day = str_extract(tgt.string, "\\d{1,2}(?=\\D)")
  },error = function(e){
    stop(message = "error extracting day value")
  }
  )

  tryCatch(expr = {
    tgt.month = bt_guess_month(tgt.string)
  },error = function(e){
    stop(message = "error extracting month value")
  }
  )

  tryCatch(expr = {
    tgt.year = str_extract(tgt.string, "\\d{4}")
  },error = function(e){
    stop(message = "error extracting year value")
  }
  )

  return(paste(tgt.year, tgt.month, tgt.day, sep = '-') %>% as.Date(origin = "1970-01-01"))




}
