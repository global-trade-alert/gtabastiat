#' Prepare a doc2vec matrix using a pretrained word2vec model.
#'
#' To be used on text data from a particular feature column (e.g. title, description or AA)
#'
#'
#' @param model.w2v pretrained word embeddings
#' @param doc_id the doc ids, probably the BID with Bastiat
#' @param text character vector of words separated by spaces
#' @param as.df return as a dataframe? if F, returns a matrix
#'
#' @return
#' @export
#'
#' @examples
bt_d2v_col_preprocess = function(d2v.col, doc_id, text, as.df = T){

  library(word2vec)
  #results = sapply(text, function(text) str_split(string = text, pattern = " ", simplify = T))
  #names(results) = doc_id

  #list relevant files
  current.wd = getwd()
  wd.pref = str_extract(getwd(), ".+GTA data team Dropbox")
  mrs.h.data.path = paste0(wd.pref, "/Bastiat/data/Mrs Hudson/")
  classifiers = paste0(mrs.h.data.path, list.files(path = mrs.h.data.path))

  #load embedding models
  #ONLY THE INDIVIDUAL COLNAME EMBEDDINGS WILL BE NAMED WITH THE COLNAME
  #the models are saved as an .Rdata list
  mrs.h.col.emb.list = classifiers[grepl(d2v.col, classifiers)]
  mrs.h.col.emb.fname = mrs.h.col.emb.list[length(mrs.h.col.emb.list)]

  #load(mrs.h.col.emb.fname)

  w2v.emb = read.word2vec(file = mrs.h.col.emb.fname, normalize = T)

  names(text) = doc_id

  result = doc2vec(w2v.emb, newdata = text)

  if(any(is.na(result))){
    na.check=which(is.na(result), arr.ind=T) %>%
      row.names() %>%
      unique()

    warning(paste0("d2v_preprocess: ", length(na.check)," NAs generated. Maybe due to: new AA (please retrain the models); bad chars getting through (make sure the lead text is in English); something more serious (put on some coffee and good luck)."))
  }

  if(as.df){
    setwd(current.wd)
    return(as.data.frame(result))
  }else{
    #returns a matrix
    setwd(current.wd)
    return(result)
  }
}
