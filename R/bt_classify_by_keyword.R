# Roxygen documentation

#' Bastiat, please estimate a new squad classifier based on the best detectives.
#'
#' @return Several data frames about how the detective did.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

bt_classify_by_keyword = function(text.to.check=NULL,
                                  text.id=NULL,
                                  flavour="negative"){

  ## removing leads that include negative trigger words
  keys=gtabastiat::gta.keywords

  check.df=data.frame(order.nr=1:length(text.id),
                      id=text.id,
                      text=text.to.check,
                      agency=stringr::str_extract(text.id, "[A-Z]+-[A-Z]+"),
                      found.key=F,
                      stringsAsFactors = F)

  for(key.source in unique(check.df$agency)[unique(check.df$agency) %in% keys$source[keys$type==flavour]]){

    search.df=subset(check.df, agency==key.source)

    out=paste(subset(keys, type==flavour & source==key.source)$key, collapse="|")
    search.df$found.key=grepl(out, search.df$text, ignore.case=T)

    check.df=rbind(subset(check.df, agency!=key.source),
                   search.df)
  }

  check.df=check.df[order(check.df$order.nr),]

  return(check.df$found.key)

}
