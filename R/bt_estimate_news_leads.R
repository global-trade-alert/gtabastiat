#' Generates predictions for relevance using the most recent Mrs Hudson model.
#'
#' @param leads.core.news A dataframe of leads in the standard format. Requires
#'   acting.agency, act.title.en, act.description.en
#' @param keep.results.ratio what fraction of the results you want to keep. 1 =
#'   keep all, 0 = discard all. the ones with lowest relevance prob will be
#'   discarded.
#' @param binary.prediction return results as a binary prediction or as their
#'   raw 0 ~ 1 confidence values.
#'
#' @return A vector of Mrs. Hudson's predictions.
#'
#' @references www.globaltradealert.org
#' @Author Callum Campbell for Global Trade Alert.
#'
bt_estimate_news_leads = function(leads.core.news, keep.results.ratio = 0.95, binary.prediction = T){

  if(any(!grepl("NEWS-", leads.core.news$bid))){
    stop("Mrs Hudson is trained to evaluate news leads only. It looks like some of your input leads are not news leads.")
  }

  library(randomForest)

  #load the most recent model and the tokeniser
  classifiers = paste0("content/0 core/Mrs Hudson/", list.files(path = "content/0 core/Mrs Hudson/"))

  #load model
  mrs.hudson.model.list = classifiers[grepl("Mrs Hudson model", classifiers)]
  mrs.hudson.model.file.name = mrs.hudson.model.list[length(mrs.hudson.model.list)]
  load(mrs.hudson.model.file.name)

  #construct text
  acting.agency = leads.core.news[,match("acting.agency", colnames(leads.core.news))]
  act.title.en = leads.core.news[,match("act.title.en", colnames(leads.core.news))]
  act.description.en = leads.core.news[,match("act.description.en", colnames(leads.core.news))]

  #make the td matrix for prediction
  x.predict = bt_td_matrix_preprocess(text = paste(acting.agency,
                                         act.title.en,
                                         act.description.en))

  print("Showing the leads to Mrs Hudson...")
  predictRF = predict(mrs.hudson.model, newdata=x.predict, type = "prob")
  print("Mrs Hudson has assessed the leads.")


  predictRF = as.data.frame(predictRF)

  if(binary.prediction){

    confidence.quantile = quantile(predictRF$`TRUE`, 1-keep.results.ratio)
    return(sapply(predictRF$`TRUE`, function(x, y) ifelse(x > y, 1, 0), y=confidence.quantile))

  }else{
    return(predictRF$`TRUE`)
  }
  #below was for 'response' prediction type, now is superseded
  #for some reason casting directly to numeric gives 1s and 2s
  #return(as.logical(predictRF) %>% as.numeric())

}
