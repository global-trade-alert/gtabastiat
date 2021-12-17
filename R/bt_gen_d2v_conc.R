#' Generate concatenated matrix of doc2vec vectors for Mrs Hudson
#'
#' Vectorises text using saved training colnames. preprocesses the text as well.
#'
#' @param leads.core
#'
#' @return
#' @export
#'
#' @examples
bt_gen_d2v_conc = function(leads.core){


  #list relevant files
  current.wd = getwd()
  wd.pref = str_extract(getwd(), ".+GTA data team Dropbox")
  mrs.h.data.path = paste0(wd.pref, "/Bastiat/data/Mrs Hudson/")
  classifiers = paste0(mrs.h.data.path, list.files(path = mrs.h.data.path))

  #get cols used to generate embeddings
  mrs.h.chc.embs = classifiers[grepl("mrsh_conc_hypermodel_training_cols", classifiers)]

  most.recent.mrs.h.chc.embs = mrs.h.chc.embs[length(mrs.h.chc.embs)]
  load(most.recent.mrs.h.chc.embs)

  mrs.h.col.vectors = list()

  for(col in training.cols){

    col.idx = match(col, colnames(leads.core))


    if(col == "acting.agency"){
      target.text = text = leads.core[,col.idx] %>% bt_text_preprocess(stop.rm = F)
    }else{
      target.text = text = leads.core[,col.idx] %>% bt_text_preprocess()
    }

    x.train = bt_d2v_col_preprocess(d2v.col = col, doc_id = leads.core$bid, text = target.text)
    mrs.h.col.vectors[[col]] = x.train

  }

  #conc them all into a big aß matrix
  mrs.h.conc.vectors = do.call(cbind, mrs.h.col.vectors) %>%
    as.data.frame()

}
