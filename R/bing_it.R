# Roxygen documentation

#' Bastiat, please translate using bing (legacy function).
#'
#' @return Use MSFT's translation API.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------

bing_it = function (text, origin, target){

  if(origin %in% c("yue", "zh-Hans", "zh-Hant","ja")){
    params = paste("?from=",origin,'&to=',target, '&text=',gsub(" ","%20",text), sep="")
    result=GET(url=msft.host,
               path=paste(msft.path, params, sep=""),
               add_headers('Ocp-Apim-Subscription-Key'=msft.key))

  }else {
    params = paste("?from=",origin,'&to=',target, '&text=',URLencode(text), sep="")
    result=GET(url=msft.host,
               path=paste(msft.path, params, sep=""),
               add_headers('Ocp-Apim-Subscription-Key'=msft.key))

  }

  ## result
  return(gsub("<|>","",str_extract(httr::content(result,type="text", encoding="UTF-8"),">.*?<")))

}

