#' Get tf-idf scores for terms two categories of document and create master list
#'
#'
#' @param unnested.token.df product of bt_unnest_tokens()
#' @param rlv.doc.ids the ids of the relevant (category 1) documents (others will be irrelevant/cat 2)
#'
#' @return
#' @export
#'
#' @examples
bt_generate_2cat_tf_idf = function(unnested.token.df, rlv.doc.ids){
  library(udpipe)
  x.train.tfidf = unnested.token.df

  #separate into relevant and irrelevant

  x.train.tfidf.rlv = subset(x.train.tfidf, doc_id %in% rlv.doc.ids) %>%
    document_term_frequencies_statistics()
  x.train.tfidf.irv = subset(x.train.tfidf, ! doc_id %in% rlv.doc.ids) %>%
    document_term_frequencies_statistics()

  tf.idf.master.rlv = data.frame(token = x.train.tfidf.rlv$term,
                                 tf.idf.rlv = x.train.tfidf.rlv$tf_idf,
                                 bm25.rlv = x.train.tfidf.rlv$bm25,
                                 stringsAsFactors = F) %>%
    subset(!duplicated(token))

  tf.idf.master.irv = data.frame(token = x.train.tfidf.irv$term,
                                 tf.idf.irv = x.train.tfidf.irv$tf_idf,
                                 bm25.irv = x.train.tfidf.irv$bm25,
                                 stringsAsFactors = F) %>%
    subset(!duplicated(token))

  tf.idf.master = merge(tf.idf.master.rlv, tf.idf.master.irv, by = "token", all = T)

  tf.idf.master[is.na(tf.idf.master)] = 0

  return(tf.idf.master)


}
