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

    detective.characteristics=bt_get_detective_characteristics(detective.name=detective)
    print("Got characteristics")

    detective.data=bt_create_estimation_data(bid=training$bid,
                                          evaluation=training$evaluation,
                                          text=training$text,
                                          train.share = train.share,
                                          variables=detective.characteristics$variables,
                                          dtm.incl=detective.characteristics$dtmatrix.included,
                                          dtm.metric=detective.characteristics$dtmatrix.metric,
                                          dtm.terms=detective.characteristics$dtmatrix.term.count,
                                          acting.agency=detective.characteristics$vars.incl.acting.agency,
                                          act.values=detective.characteristics$vars.incl.act.value,
                                          is.td=detective.characteristics$vars.incl.td,
                                          keywords=detective.characteristics$vars.incl.keywords)

    print("Prepared data")

    detective.classifier=bt_estimate_classifier(training.data=detective.data$estimation.data,
                                             training.id="bid",
                                             training.y="evaluation",
                                             estimation.model=detective.characteristics$estimation.model,
                                             robustness.turns=robustness.turns,
                                             train.share=train.share,
                                             update.log=save.stats,
                                             save.classifier=save.classifier,
                                             detective.name=detective.characteristics$detective.name)


    print(paste("Finish",detective))
    print(paste("Score: ",detective.classifier$performance$score,"; Score adjusted: ",detective.classifier$performance$score.adjusted, sep=""))
  }
}

