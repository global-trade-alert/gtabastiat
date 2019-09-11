# Roxygen documentation

#' Bastiat, have the detective predice this data.
#'
#' @return Several data frames about how the detective did.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

b_detective_prediction = function(detective=NULL,
                                  classifier.location=NULL,
                                  prediction.data=NULL){

  if(is.null(prediction.data)){
    load("data/classifier/prediction.data data.Rdata")
    prediction.data=training
    rm(training)
  }


  print(paste("Starting",detective))

  ## loading the estimator
  load(classifier.location)


  load("content/0 core/Classifier statistics & history.Rdata")
  d.no=max(subset(model, name==detective)$detective.no)
  my.vars=unique(unlist(strsplit(as.character(subset(model, name==detective & detective.no==d.no)$my.vars), ";")))
  dt.incl=subset(model, name==detective & detective.no==d.no)$dtm.incl
  dt.mtrc=subset(model, name==detective & detective.no==d.no)$dtm.metric
  nr.of.terms=subset(model, name==detective & detective.no==d.no)$dtm.terms

  ## preparing data
  if("acting.agency" %in% my.vars){
    aa=prediction.data$acting.agency
  }


  ## processing the text
  train=b_create_model_variables(bid=prediction.data$bid,
                                 text=prediction.data$text,
                                 train.share = .82,
                                 detective.number=999,
                                 acting.agency=NULL,
                                 variables=my.vars,
                                 dtm.incl=dt.incl,
                                 dtm.metric=dt.mtrc,
                                 dtm.terms=nr.of.terms,
                                 keywords=T,
                                 is.td=T)

  train=merge(train, prediction.data[,c("bid", "acting.agency")],by="bid")



  train.bid=train$bid

  train=train[,c(my.vars)]


  ## prediction
  prediction.result= data.frame(detective=detective,
                                bid=train.bid,
                                prediction=predict(incumbent.classifier, train)$pred[,1], stringsAsFactors = F)

  rm(incumbent.classifier)

  print(paste("Finish",detective))
  return(prediction.result)

}

