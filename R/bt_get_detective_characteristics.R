# Roxygen documentation

#' Bastiat, please retrieve the characteristics of this detective.
#'
#' @return A list of characteristics.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

bt_get_detective_characteristics <- function(detective.name=NULL,
                                             detective.number=NULL,
                                             detective.log="content/0 core/Classifier statistics & history.Rdata") {

  load(detective.log)

  if(is.null(detective.name) & is.null(detective.number)){stop("Please specify detective.")}

  if(is.null(detective.number)){
    detective.number=max(subset(model, name==detective.name)$detective.no)
  }

  if(is.null(detective.name)){
    detective.name=max(subset(model, detective.no==detective.number)$name)
  }

  variables=unique(unlist(strsplit(as.character(subset(model, name==detective.name & detective.no==detective.number)$my.vars), ";")))
  kw=any(c("pos.word","pos.word.char", "neg.word", "neg.word.char") %in% variables)
  td="is.td" %in% variables

  the.model=subset(model, name==detective.name & detective.no==detective.number)$estimation.method
  dt.incl=subset(model, name==detective.name & detective.no==detective.number)$dtm.incl
  dt.mtrc=subset(model, name==detective.name & detective.no==detective.number)$dtm.metric
  nr.of.terms=subset(model, name==detective.name & detective.no==detective.number)$dtm.terms

  if("acting.agency" %in% variables){
    aa=as.character(training$acting.agency)
  } else {
    aa=NULL
  }


  if("has.value" %in% variables){
    av=training$act.values
  } else {
    av=NULL
  }

  output.list<- list("detective.name"=detective.name,
                     "detective.number"=detective.number,
                     "variables"=variables,
                     "vars.incl.acting.agency"=aa,
                     "vars.incl.act.value"=av,
                     "vars.incl.keywords"=kw,
                     "vars.incl.td"=td,
                     "dtmatrix.included"=dt.incl,
                     "dtmatrix.metric"=dt.mtrc,
                     "dtmatrix.term.count"=nr.of.terms,
                     "estimation.model"=the.model)

  return(output.list)
}
