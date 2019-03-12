# Roxygen documentation

#' Bastiat, please process my text to find the GTA keywprds.
#'
#' @return A data frame with variables for each keyword dimension.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

b_process_keywords <- function(bid=NULL,
                              text=NULL

) {

  text.to.process=data.frame(bid=bid,
                             text=text,
                             stringsAsFactors = F)

  keys=gtabastiat::gta.keywords

  negative=unique(keys$key[keys$type=="negative"])
  positive=unique(keys$key[keys$type=="positive"])

  text.to.process$pos.word=0
  text.to.process$pos.word.char=0

  ## y/n; chars
  for(i in 1:length(positive)){
    word=positive[i]
    text.to.process$pos.word=text.to.process$pos.word+str_count(text.to.process$text, word)
    text.to.process$pos.word.char=text.to.process$pos.word.char+str_count(text.to.process$text, word)*nchar(word)
    # print(i/length(positive))
  }
  text.to.process$pos.word.char=text.to.process$pos.word.char/nchar(enc2native(text.to.process$text))


  text.to.process$neg.word=0
  text.to.process$neg.word.char=0


  for(i in 1:length(negative)){
    word=negative[i]
    text.to.process$neg.word=text.to.process$neg.word+str_count(text.to.process$text, word)
    text.to.process$neg.word.char=text.to.process$neg.word+str_count(text.to.process$text, word)*nchar(word)
    # print(i/length(negative))
  }
  text.to.process$neg.word.char=text.to.process$neg.word.char/nchar(enc2native(text.to.process$text))



  return(text.to.process)

}
