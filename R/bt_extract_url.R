# Roxygen documentation

#' Bastiat, please extract all URLs you find in the following string.


# Function infos and parameters  --------------------------------------------

bt_extract_url = function(string,
                          get.tld=F){
  regex_url = gtabastiat::regex_url

  urls=unique(unlist(stringr::str_extract_all(string, regex_url)))

  urls=gsub(",$","",urls)

  if(length(urls)==0){urls=NA}

  if(get.tld){tld=str_extract(urls,"((https?://)|(www\\.))[A-Za-z\\.\\-_0-9]+")}

  return(urls)

}
