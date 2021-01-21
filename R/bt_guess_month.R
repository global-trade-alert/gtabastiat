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

  german = c("[Jj]anuar", "[Ff]ebruar", "[Mm].rz", "[Aa]pril", "[Mm]ai", "[Jj]uni", "[Jj]uli", "[Aa]ugust", "[Ss]eptember", "[Oo]ktober", "[Nn]ovember", "[Dd]ezember")
  french= c("[Jj]anvier", "[Ff].vrier", "[Mm]ars", "[Aa]vril", "[Mm]ai", "[Jj]uin", "[Jj]uillet", "[Aa]o.t", "[Ss]eptembre", "[Oo]ctobre", "[Nn]ovembre", "[Dd].cembre")
  spanish = c("[Ee]nero", "[Ff]ebrero", "[Mm]arzo", "[As]bril", "[Mm]ayo", "[Jj]unio", "[Jj]ulio", "[Aa]gosto", "[Ss]eptiembre", "[Oo]ctubre", "[Nn]oviembre", "[Dd]iciembre")
  russian =  c("[Яя]нвар", "[Фф]еврал", "[Мм]арт", "[Аа]прел", "[Мм]ая", "[Ии]юн", "[Ии]юл", "[Аа]вгуст", "[Сс]ентябр", "[Оо]ктябр", "[Нн]оябр", "[Дд]екабр")
  bahasa = c("[Jj]anuari", "[Ff]ebruari", "[Mm]aret", "[Aa]pril", "[Mm]ei", "[Jj]uni", "[Jj]uli", "[Aa]gustus", "[Ss]eptember", "[Oo]ktober", "[Nn]ovember", "[Dd]esember")
  portuguese = c("[Jj]aneiro", "[Ff]evereiro", "[Mm]ar.o", "[Aa]bril", "[Mm]aio", "[Jj]unho", "[Jj]ulho", "[Aa]gosto", "[Ss]etembro", "[Oo]utubro", "[Nn]ovembro", "[Dd]ezembro")
  italian = c("gennaio", "febbraio", "marzo", "aprile", "maggio", "giugno", "luglio", "agosto", "settembre", "ottobre", "novembre", "dicembre")



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
