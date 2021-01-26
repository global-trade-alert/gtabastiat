#' Guesses month index of several languages.
#' Currently German, French, Spanish, Russian, Bahasa and Portuguese.
#'
#' NB CJK languages NB: dates are 1月，2月，3月。。。12月 so can use str_extract(string, "\\d{1,2}") or equivalent
#'
#' @param tgt.string string containing month in supported languages
#'
#' @return integer number of guessed month
#'
#'
#' @references www.globaltradealert.org
#' @Author Callum Campbell for Global Trade Alert.
#'

bt_guess_month = function(tgt.string){

  # months. feel free to add more languages

  #NB CJK languages NB: dates are 1月，2月，3月。。。12月 so can use str_extract(string, "\\d{1,2}") or equivalent
  #use a regex . instead of diacritic chars - otherwise you will get problems.

  #unbelievably the months names used to be saved here in the code, but the
  #russian ones were causing encoding errors when trying to install the package
  #from github.

  #even with the russian bit commented out, it still caused an error as the glyphs themselves were corrupting something

  #for reference, the structure of a language's months var should be like this:
  #german = c("[Jj]anuar", "[Ff]ebruar", "[Mm].rz", "[Aa]pril", "[Mm]ai", "[Jj]uni", "[Jj]uli", "[Aa]ugust", "[Ss]eptember", "[Oo]ktober", "[Nn]ovember", "[Dd]ezember")

  #nb upper/lowercase allowances, and replacement of any diacritics with regex '.' (diacritics often cause corruption)

  load(file="R help files/month_names.Rdata")




  Encoding(russian) <- "UTF-8" #required for russian to work

  months.master = paste(german, french, spanish, russian, portuguese, italian, sep = ")|(")

  months.master = paste0("(", months.master, ")")

  #this looks weird because the syntax of sapply() and grepl() mean that the 'y'
  #variable in the sapply() is used as the 'x' variable in the grepl()

  return(sapply(months.master,
                        function(x, y) grepl(pattern = x, x = y,
                                             ignore.case = T,
                                             perl = T),
                        y=tgt.string) %>%
    as.logical() %>%
    which()
  )


}
