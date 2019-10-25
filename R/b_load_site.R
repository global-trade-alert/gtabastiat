# Roxygen documentation

#' Bastiat, please wait until the following XPATH appears on the site.
#' @param xpath Provide the XPATH for which the function should wait.
#' @param wait Specify the number of seconds you want to wait. Default is 60.
#' @param abort Specifiy whether to cause an error in case path is not find after waiting time. Default is FALSE.
#'
#' @return Pauses the algorithm until XPATH appears.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------
b_load_site=function(xpath=NULL,
                     wait=60,
                     abort=F,
                     wait.interval=.2){

  html.load= htmlParse(remDr$getSource()[[1]], asText=T)
  print("Loading site ...")

  refreshed=FALSE
  t=Sys.time()

  while(refreshed==F & as.numeric(Sys.time()-t)<=wait){
    print("... waiting ...")
    Sys.sleep(wait.interval)
    html.load = htmlParse(remDr$getSource()[[1]], asText=T)

    refreshed=length(xpathSApply(html.load, xpath, xmlValue))>0

  }

  if(refreshed){

    print("Site's fresh!")
    assign.global("html", html.load)

  }else {

    if(abort){
      stop("Site does not respond")
    }else{
      print("Site does not respond")
    }

  }




  # legacy version
  # html <- htmlParse(remDr$getSource()[[1]], asText=T)
  # load=length(xpathSApply(html, xpath, xmlValue))
  #
  # sleep=1
  # while(load==0){
  #   print(paste("waiting ", sleep, sep=""))
  #   Sys.sleep(1)
  #
  #   html <- htmlParse(remDr$getSource()[[1]], asText=T)
  #   load=length(xpathSApply(html, xpath, xmlValue))
  #   sleep=sleep+1
  #
  #   if(sleep>wait){
  #
  #     if(abort){
  #       stop("site does not respond")
  #     }else{
  #       print("site does not respond")
  #     }
  #
  #     load=1
  #   }
  # }
  # print("loading complete")
}

