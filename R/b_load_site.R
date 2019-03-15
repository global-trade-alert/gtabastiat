# Roxygen documentation

#' Bastiat, please wait until the following XPATH appears on the site.
#' @param xpath Provide the XPATH for which the function should wait.
#'
#' @return Pauses the algorithm until XPATH appears.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------
b_load_site=function(xpath){
  html <- htmlParse(remDr$getSource()[[1]], asText=T)
  load=length(xpathSApply(html, xpath, xmlValue))

  sleep=1
  while(load==0){
    print(paste("waiting ", sleep, sep=""))
    Sys.sleep(1)

    html <- htmlParse(remDr$getSource()[[1]], asText=T)
    load=length(xpathSApply(html, xpath, xmlValue))
    sleep=sleep+1

    if(sleep>60){
      print("site does not respond")
      load=1
    }
  }
  print("loading complete")
}