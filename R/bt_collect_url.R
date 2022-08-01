# Roxygen documentation

#' Bastiat, collects a URL and stores it locally using file name. Websites are printed into PDF, URLS to files are downloaded.
#'
#' @return The complete file name of the collected url (incl. file format).
#' @references www.globaltradealert.org
#' @author Johannes Fritz, Robin Scherrer and Callum Campbell for GTA


# Function infos and parameters  --------------------------------------------


bt_collect_url = function(url=NULL,
                          file.name=NULL,
                          store.path=NULL,
                          add.timestamp=F,
                          update.file.name=T,
                          js.path="setup/rasterize.js",
                          phantom.path="~/bin/phantomjs"){


  library(httr)

  # send request to check if page is redirecting or broken
  r = tryCatch({
    httr::HEAD(url=url, timeout(5))
  }, error = function(e) {
    FALSE
  })

  if(is.logical(r)) {
    return(list("new.file.name"=NA, "file.suffix"=NA, "url"=url))
  } else if (((r$url != url) &
              (paste0(r$url, "/") != url)) &
             (gsub("www\\.", "", r$url) != url) |
             r$status_code != "200") {
    return(list("new.file.name"=NA, "file.suffix"=NA, "url"=url))
  }

  # define where the file should be stored
  if(is.null(store.path)){
    file.path=file.name
  } else {
    file.path=paste0(gsub("/$", "", store.path), "/", file.name)
  }

  # create the time stamp to include it in the file name
  if(add.timestamp){
    t.stamp=paste0(" - ", gsub("\\D", "-", Sys.time()))
  } else {
    t.stamp=""
  }

  # check whether URL leads to a file
  file.types=c("doc", "pdf","xls","txt","csv","rdata")
  is.file=grepl(paste(file.types, collapse="|"), str_extract(url, "\\.[A-Za-z]{1,5}$"), ignore.case = T)

  # download the file if there is one or save a screenshot of the webpage
  if(is.file) {
    file.suffix=str_extract(url,"\\.[A-Za-z]{1,5}$")
    GET(url, write_disk(paste0(file.path, t.stamp, file.suffix), overwrite=TRUE))
  } else {
    file.suffix=".pdf"
    cmd=paste(phantom.path,
              js.path,
              paste0("'",url,"'"),
              paste0("'",file.path, t.stamp, file.suffix,"'"),
              "'A1'")
    system(cmd)
  }

  # return the file name
  if(update.file.name){
    new.file.name=paste0(file.path, t.stamp, file.suffix)
  } else {
    new.file.name=file.name
  }
  return(list("new.file.name"=new.file.name, "file.suffix"=file.suffix, "url"=url))
}

