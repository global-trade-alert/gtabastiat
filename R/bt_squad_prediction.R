# Roxygen documentation

#' Bastiat, please use the squad classifier to identify relevant text.
#'
#' @return A data frame with the text id and the classification result.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

bt_squad_prediction = function(prediction.data.id=NULL,
                               prediction.data.text=NULL,
                               prediction.acting.agency=NULL,
                               train.share=.82,
                               squad.classifier="content/0 core/Bastiat squad classifier.Rdata"
                                 ){

  ## getting squad member predictions
  squad.predictions=data.frame()

  load(squad.classifier)
  rm(classifier, cutoff)
  for(squad.member in bt.squad$member.name){

    load(bt.squad$classifier.location[bt.squad$member.name==squad.member])
    rm(classifier, cutoff)
    squad.predictions=rbind(squad.predictions,
                            bt_detective_prediction(detective=squad.member,
                                                    classifier.location=bt.squad$classifier.location[bt.squad$member.name==squad.member],
                                                    prediction.data.id=prediction.data.id,
                                                    prediction.word.score=word.score,
                                                    prediction.data.text=prediction.data.text,
                                                    prediction.acting.agency=prediction.acting.agency,
                                                    train.share=train.share))

  }


  ## squad variable selection here
  # currently only use prediction probability, not whether it is classified as relevant.
  squad.predictions$relevant=NULL

  squad.predictions=merge(squad.predictions,
                          unique(data.frame(bid=prediction.data.id,
                                            evaluation=prediction.data.evaluation,
                                            stringsAsFactors = F)),
                          by="bid", all.x=T)

  squad.predictions=reshape(squad.predictions, idvar = c("bid", "evaluation"), timevar = "detective", direction="wide")

  # Preidcting using squad classifier


  predict.bid=squad.predictions$estimation.data$bid

  predict.data=squad.predictions[,setdiff(names(squad.predictions),c("bid","evaluation"))]


  ## prediction
  load(squad.classifier)
  prediction.result= data.frame(text.id=predict.bid,
                                relevant=as.numeric(predict(classifier, predict.data)$pred[,1]>=cutoff),
                                stringsAsFactors = F)

  return(prediction.result)

}

