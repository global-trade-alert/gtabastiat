# Roxygen documentation

#' Bastiat, please re-estimate this detective.
#'
#' @return Several data frames about how the detective did.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

bt_estimate_detective = function(detective.name=NULL,
                                 robustness.turns=0,
                                 train.share=.82,
                                 save.stats=T,
                                 save.classifier=T,
                                 training.data="data/classifier/training data.Rdata"
                                 ){


  load(training.data)
  for(detective in detective.name){
    print(paste("Starting",detective))

    detective.data=bt_create_estimation_data(bid=training$bid,
                                          evaluation=training$evaluation,
                                          text=training$text,
                                          acting.agency = training$acting.agency,
                                          train.share = train.share,
                                          detective.name=detective
                                          )

    print("Prepared data")

    detective.classifier=bt_estimate_classifier(training.data=detective.data$estimation.data,
                                             training.id="bid",
                                             training.y="evaluation",
                                             estimation.model=detective.data$detective.characteristics$estimation.model,
                                             robustness.turns=robustness.turns,
                                             train.share=train.share,
                                             update.log=save.stats,
                                             save.classifier=save.classifier,
                                             detective.name=detective.data$detective.characteristics$detective.name)


    print(paste("Finish",detective))
    print(paste("Score: ",detective.classifier$performance$score,"; Score adjusted: ",detective.classifier$performance$score.adjusted, sep=""))
  }
}

