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
                                   prediction.data.evaluation=NULL,
                                   prediction.data.text=NULL,
                                   prediction.acting.agency=NULL,
                                   train.share=.82){


  print(paste("Starting",detective))

  ## loading the estimator
  detective.characteristics=bt_get_detective_characteristics(detective.name=detective)
  print("Got characteristics")


  detective.data=bt_create_estimation_data(bid=prediction.data.id,
                                           evaluation=prediction.data.evaluation,
                                           text=prediction.data.text,
                                           acting.agency = prediction.acting.agency,
                                           train.share = train.share,
                                           detective.name=detective)

  print("Prepared data")

  load(classifier.location)




  predict.bid=detective.data$estimation.data$bid

  predict.data=detective.data$estimation.data[,c(detective.data$variables)]


  ## prediction
  prediction.result= data.frame(detective=detective,
                                bid=predict.bid,
                                prediction=predict(classifier, predict.data)$pred[,1],
                                stringsAsFactors = F)

  prediction.result$relevant=as.numeric(prediction.result$prediction>cutoff)

  print(paste("Finish",detective))
  return(prediction.result)

}

