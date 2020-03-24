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

  if(detective.name=="incumbent"){detective.name=model$name[nrow(model)]}

  if(is.null(detective.number)){
    detective.number=max(subset(model, name==detective.name)$detective.no)
  }

  if(is.null(detective.name)){
    detective.name=subset(model, detective.no==detective.number)$name
  }

  variables=unique(unlist(strsplit(as.character(subset(model, name==detective.name & detective.no==detective.number)$my.vars), ";")))

  if("acting.agency" %in% variables){
    aa=TRUE
  } else {
    aa=FALSE
  }

  output.list<- list("detective.name"=detective.name,
                     "detective.number"=detective.number,
                     "variables"=variables,
                     "vars.incl.acting.agency"=aa,
                     "vars.incl.keywords"=any(c("pos.word","pos.word.char", "neg.word", "neg.word.char") %in% variables),
                     "vars.incl.td"="is.td" %in% variables,
                     "dtmatrix.included"=subset(model, name==detective.name & detective.no==detective.number)$dtm.incl,
                     "dtmatrix.metric"=subset(model, name==detective.name & detective.no==detective.number)$dtm.metric,
                     "dtmatrix.term.count"=subset(model, name==detective.name & detective.no==detective.number)$dtm.terms,
                     "estimation.model"=subset(model, name==detective.name & detective.no==detective.number)$estimation.method)

  return(output.list)
}
