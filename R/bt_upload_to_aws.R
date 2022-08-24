# Roxygen documentation

#' Bastiat, upload a file into our AWS S3 account and return its  public URL.
#'
#' @return An AWS url.
#' @references www.globaltradealert.org
#' @author Johannes Fritz for GTA


# Function infos and parameters  --------------------------------------------


bt_upload_to_aws = function(upload.file.name=NULL,
                            upload.file.path=NULL,
                            upload.destination="gtaupload/Uploads/files/",
                            credential.path="setup/keys/as3.R") {

  library(httr)
  library(glue)

  ## For how to interact with AWS from the command line, see https://docs.aws.amazon.com/cli/latest/userguide/install-cliv2.html
  source(credential.path)
  upload.location=gsub("/+","/",paste0(upload.file.path,"/",upload.file.name))
  upload.location=gsub("^/+","",upload.location)
  system(paste0(aws.cred," aws s3 cp '",upload.location,"' s3://",upload.destination," --acl public-read --cli-connect-timeout 6000"),timeout = 60)
  aws.url=paste0("http://s3-eu-west-1.amazonaws.com/",gsub("/+","/",paste0(upload.destination,"/",upload.file.name)))

  #check the upload worked correctly
  r = tryCatch({
    httr::HEAD(url=aws.url, timeout(5))
  }, error = function(e) {
    FALSE
  })


  if(is.logical(r) | r$status_code!=200){
    stop(glue("Error uploading to AWS! The URL was supposed to be: {aws.url}"))
  }



  if(r$status_code == 200){
    print("AWS upload return code 200!")
    return(aws.url)
  }
}

