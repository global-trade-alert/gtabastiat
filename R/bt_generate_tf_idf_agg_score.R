#' Get the tf-idf-type scores per document using master vocab list
#'
#' @param tf.idf.master.path
#' @param doc.id
#' @param text
#'
#' @return
#' @export
#'
#' @examples bt_generate_tf_idf_agg_score(tf.idf.file.name,
#' doc.id = leads.core.b221$bid,
#' text = paste(bt_text_preprocess(leads.core.b221$hint.title),
#'              bt_text_preprocess(leads.core.b221$hint.description)))
bt_generate_tf_idf_agg_score = function(tf.idf.master.path, doc.id, text){

  tf.idf.unnested = bt_unnest_tokens(doc.id = doc.id, text = text)

  load(tf.idf.master.path) #should load an object called tf.idf.master

  if(!exists("tf.idf.master")){
    stop("tf-idf master file not loaded!")
  }

  tf.idf.unnested = merge(tf.idf.unnested, tf.idf.master, all.x = T, by.x = "term", by.y = "token")


  #this is super clunky, but should be OK - replace with aggregate/apply later
  result = data.frame(doc.id = character(),
                      tf.idf.rlv = numeric(),
                      tf.idf.irv = numeric(),
                      bm25.rlv = numeric(),
                      bm25.irv = numeric())

  print("Generating document-aggregated tf-idf and bm25 scores from master vocabulary...")
  pb = txtProgressBar(min = 0,
                      max = length(unique(doc.id)),
                      char = "~",
                      style = 3)

  for(doc in unique(doc.id)){
    this.doc = subset(tf.idf.unnested, doc_id == doc)

    this.doc.agg = data.frame(doc.id = doc,
                              tf.idf.rlv = sum(this.doc$tf.idf.rlv, na.rm = T),
                              tf.idf.irv = sum(this.doc$tf.idf.irv, na.rm = T),
                              bm25.rlv = sum(this.doc$bm25.rlv, na.rm = T),
                              bm25.irv = sum(this.doc$bm25.irv, na.rm = T),
                              stringsAsFactors = F)



    result = rbind(result, this.doc.agg)
    setTxtProgressBar(pb, nrow(result))

  }
  close(pb)

  return(result)

}
