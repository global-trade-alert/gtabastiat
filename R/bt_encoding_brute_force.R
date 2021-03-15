#' Try conversion to every possible encoding
#'
#' Tries converting the target string from every single encoding into UTF8.
#'
#' Determines once and for all if conversion is the problem, or the original is
#' simply corrupted.
#'
#' Probably requires human looking at the results to see which (if any) of the
#' encodings produced a salient result.
#'
#' @param tgt.string the string to convert
#' @param print.all print all the results. a human will probably have to read
#'   this to determine the answer.
#'
#' @return a list of all the results
#'
#' @references www.globaltradealert.org
#' @Author Callum Campbell for Global Trade Alert.


bt_encoding_brute_force = function(tgt.string, print.all = F){

 library(stringi)

  encodings = stri_enc_list()
  result = vector("list", length(encodings))
  names(result) = names(encodings)

  for(i in 1:length(encodings)){

    enc.guesses = c()

    for(j in 1:length(encodings[[i]])){
    enc.guesses = c(enc.guesses,
                    stri_encode(str = tgt.string,
                                to = "UTF-8",
                                from = encodings[[i]][j])
                    )
    }


    result[[i]] = enc.guesses

  }

  if(print.all){
    for(i in 1:length(result)){print(result[i])}
  }

  return(result)



}
