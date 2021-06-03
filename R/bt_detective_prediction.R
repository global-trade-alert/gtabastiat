# Roxygen documentation

#' Bastiat, have the detective predice this data.
#'
#' @return A data frame with the predictions.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

bt_detective_prediction = function(detective=NULL,
                                   classifier.location=NULL,
                                   prediction.data.id=NULL,
                                   prediction.word.score=NULL,
                                   prediction.data.text=NULL,
                                   prediction.acting.agency=NULL,
                                   train.share=.82){


  print(paste("Starting",detective))

  ## loading the estimator
  detective.characteristics=bt_get_detective_characteristics(d.name=detective)
  print("Got characteristics")

  load(classifier.location)

  #dbg
  # bid=prediction.data.id
  # word.score=prediction.word.score
  # text=prediction.data.text
  # acting.agency = prediction.acting.agency
  # detective.name=detective
  # for.training=F

  detective.data=bt_create_estimation_data(bid=prediction.data.id,
                                           word.score=prediction.word.score,
                                           text=prediction.data.text,
                                           acting.agency = prediction.acting.agency,
                                           train.share = train.share,
                                           detective.name=detective,
                                           for.training=F)

  print("Prepared data")



  predict.bid=detective.data$estimation.data$bid

  predict.data=detective.data$estimation.data[,c(detective.data$estimation.variables)]


  ## prediction
  prediction.result= data.frame(detective=detective,
                                bid=predict.bid,
                                prediction=predict(classifier, predict.data)$pred[,1],
                                stringsAsFactors = F)

  prediction.result$prediction[is.na(prediction.result$prediction)]=0
  prediction.result$relevant=as.numeric(prediction.result$prediction>cutoff)

  print(paste("Finish",detective))
  return(prediction.result)

}

