#' Generates predictions for relevance using the most recent Mrs Hudson model.
#'
#' @param leads.core.news A dataframe of leads in the standard format. Requires
#'   acting.agency, act.title.en, act.description.en
#'
#' @return A vector of Mrs. Hudson's predictions.
#'
#' @references www.globaltradealert.org
#' @Author Callum Campbell for Global Trade Alert.
#'   
bt_evaluate_news_leads = function(leads.core.news){
  
  if(any(!grepl("NEWS-", leads.core.news$bid))){
    stop("Mrs Hudson is trained to evaluate news leads only. It looks like some of your input leads are not news leads.")
  }
  
  library(randomForest)
  
  classifiers = paste0("content/0 core/", list.files(path = "content/0 core"))
  
  mrs.hudson.model.file = classifiers[grepl("Mrs Hudson", classifiers)][1]

  load(file = mrs.hudson.model.file)
  
  acting.agency = leads.core.news[,match("acting.agency", colnames(leads.core.news))]
  act.title.en = leads.core.news[,match("act.title.en", colnames(leads.core.news))]
  act.description.en = leads.core.news[,match("act.description.en", colnames(leads.core.news))]
  
  x.predict = bt_td_matrix_preprocess(text = paste(acting.agency,
                                         act.title.en,
                                         act.description.en))
  
  print("Showing the leads to Mrs Hudson...")
  predictRF = predict(mrs.hudson.model, newdata=x.predict)
  print("Mrs Hudson has assessed the leads.")
  
  #for some reason casting directly to numeric gives 1s and 2s
  return(as.logical(predictRF) %>% as.numeric())
   
}