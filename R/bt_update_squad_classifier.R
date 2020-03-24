bt_update_squad_classifier=function(update.training.data=T,
                                    squad.detecive.level=.7,
                                    update.detective.classifiers=T,
                                    detective.log="content/0 core/Classifier statistics & history.Rdata"){

  ## updating training data
  if(update.training.data){
    print("Updating training data ... ")
    bt_update_training_data()
    print("Updating training data ... complete.")
  }


  ## gathering detectives
  load(detective.log)
  detectives=c("incumbent", setdiff(unique(subset(model, detective.no %in% subset(stats, date>="2019-03-08" & (score>=squad.detecive.level|score.adjusted>=squad.detecive.level))$detective.no)$name),
                                    c("Wallander","Tin Tin",model$name[nrow(model)])))

  ## Updating detecitve classifiers if necessary
  if(update.detective.classifiers){

    for(sheriff in detectives){
      print(paste("Updating", sheriff))
      bt_estimate_detective(detective.name=sheriff)
    }

  }

  ## Updating squad classifier
  print("Updating squad classifier ... ")
  bt_estimate_squad(detectives=detectives)
  print("Updating squad classifier ... complete.")

}
