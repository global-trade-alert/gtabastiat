#' Generates predictions for relevance using the most recent Mrs Hudson model.
#'
#' @param leads.core.news A dataframe of leads in the standard format. Requires
#'   acting.agency, act.title.en, act.description.en
#' @param keep.results.ratio what fraction of the results you want to keep. 1 =
#'   keep all, 0 = discard all. the ones with lowest relevance prob will be
#'   discarded. If set to 1, the conf.cutoff value will be used to determine the
#'   binary.prediction.
#' @param binary.prediction return results as a binary prediction or as their
#'   raw 0 ~ 1 confidence values.
#' @param conf.cutoff threshold score for a doc to qualify as 'relevant', lower
#'   tends to mean more recall and less precision
#' @param return.both return both binary and raw.score in a list.
#'
#' @return A vector of Mrs. Hudson's predictions.
#'
#' @references www.globaltradealert.org
#' @Author Callum Campbell for Global Trade Alert.
#'
bt_estimate_news_leads = function(leads.core.news,
                                  keep.results.ratio = 1,
                                  binary.prediction = T,
                                  return.both = F,
                                  conf.cutoff = 0.2){

  if(any(!grepl("NEWS-", leads.core.news$bid))){
    warning("Mrs Hudson is trained to evaluate news leads only. It looks like some of your input leads are not news leads.")
  }

  library(randomForest)

  #list relevant files
  current.wd = getwd()
  wd.pref = str_extract(getwd(), ".+GTA data team Dropbox")
  mrs.hudson.data.path = paste0(wd.pref, "/Bastiat/content/0 core/Mrs Hudson/")
  classifiers = paste0(mrs.hudson.data.path, list.files(path = mrs.hudson.data.path))

  #load model
  mrs.hudson.model.list = classifiers[grepl("Mrs Hudson model", classifiers)]
  mrs.hudson.model.file.name = mrs.hudson.model.list[length(mrs.hudson.model.list)]
  load(mrs.hudson.model.file.name)

  #construct text
  acting.agency = leads.core.news[,match("acting.agency", colnames(leads.core.news))]
  act.title.en = leads.core.news[,match("act.title.en", colnames(leads.core.news))]
  act.description.en = leads.core.news[,match("act.description.en", colnames(leads.core.news))]

  cl.text = paste(acting.agency,
                  act.title.en,
                  act.description.en) %>% bt_text_preprocess()


  if(mrs.h.gen.method == "tokeniser"){
  #make the td matrix for prediction
  x.predict = bt_td_matrix_preprocess(text = cl.text, tokeniser = tokeniser)
  }

  if(mrs.h.gen.method == "d2v"){
    library(word2vec)
    #load embeddings
    mrs.hudson.w2v.list = classifiers[grepl("Mrs Hudson w2v", classifiers)]
    mrs.hudson.w2v.fname = mrs.hudson.w2v.list[length(mrs.hudson.w2v.list)]
    mrs.h.w2v = read.word2vec(file = mrs.hudson.w2v.fname, normalize = T)

    x.predict = bt_d2v_preprocess(mrs.h.w2v,
                                  doc_id = leads.core.news[,match("bid", colnames(leads.core.news))],
                                  text = cl.text)
  }

  print("Showing the leads to Mrs Hudson...")
  predictRF = predict(mrs.hudson.model, newdata=x.predict, type = "prob")
  print("Mrs Hudson has assessed the leads.")


  predictRF = as.data.frame(predictRF)

  #this is a mess because of dependencies

  if(binary.prediction){

    confidence.quantile = quantile(predictRF$`TRUE`, 1-keep.results.ratio)
    return(sapply(predictRF$`TRUE`, function(x, y) ifelse(x > y, 1, 0), y=confidence.quantile))

  }else if(!return.both){

    return(predictRF$`TRUE`)

  }else{
    if(keep.results.ratio < 1){
    confidence.quantile = quantile(predictRF$`TRUE`, 1-keep.results.ratio)
    binary.prediction.result = sapply(predictRF$`TRUE`, function(x, y) ifelse(x > y, 1, 0), y=confidence.quantile)
    }else{
      binary.prediction.result = predictRF$`TRUE` > conf.cutoff
    }

   return(
     list(binary.prediction.result = binary.prediction.result,
               raw.score = predictRF$`TRUE`)
   )
  }
  #below was for 'response' prediction type, now is superseded
  #for some reason casting directly to numeric gives 1s and 2s
  #return(as.logical(predictRF) %>% as.numeric())

}
