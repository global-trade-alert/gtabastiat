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
                            credential.path="setup/keys/as3.R"){

  source(credential.path)

  upload.location=gsub("/+","/",paste0(upload.file.path,"/",upload.file.name))

  system(paste0(aws.cred," aws s3 cp ",upload.location," s3://",upload.destination," --acl public-read"))


  aws.url=paste0("http://s3-eu-west-1.amazonaws.com/",upload.destination,upload.file.name)


  return(aws.url)
}



