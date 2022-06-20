#' Initialise the environment before scrape routine
#'
#' Loads all packages and opens the DB connection in preparation for running the scrape routine.
#'
#' Also cleans the environment - so make sure you save any objects you need before running this!
#'
#' Any packages the routine requires should go in here.
#'
#' This will only work in Bastiat's home directory.
#'
#' Originally was located at the beginning of bt_scrape_handler.R
#'
#' @return
#' @export
#'
#' @examples
bt_scraper_init = function(){

  print("Loading packages...")

  library(dplyr)
  library("rvest")
  library("httr")
  library(RCurl)
  library(XML)
  library("data.table")
  # library(xlsx)
  # library("mailR")
  library("stringr")
  library("tabulizer")
  library("htmltab")
  # library("splitstackshape")
  library("webdriver")
  library("lubridate")
  library("textcat")
  # library(flipTime)
  library(tidytext)
  # library(caret)
  library("googleLanguageR")
  library(gtasql)
  library(pool)
  library(RMariaDB)

  library(gtabastiat)
  library(gtalibrary)
  library(xml2)


  #clean workspace
  rm(list = ls())

  print("Opening connection...")


# Old Dropbox stack -------------------------------------------------------


  if(grepl(x = getwd(), pattern = ".+Dropbox/")){
    bastiat.wd = paste0(str_extract(getwd(), ".+Dropbox/"), "[Bb]astiat")


  #setwd("/home/rstudio/Dropbox/Bastiat")
  setwd(bastiat.wd)
  source("setup/keys/ric.R")
  pool <<- pool::dbPool(
    drv = RMariaDB::MariaDB(),
    host = db.host,
    username = db.user,
    password = db.password,
    dbname=db.name
  )
  #these are not retained when this is run as a function anyway
  #rm(db.host, db.user, db.password, db.name)
  session.prefix<<-"bt_"

  print("Loading Bastiat base...")

  source("code/daily/infrastructure/scrape directory.R")
  source("code/daily/infrastructure/Bastiat base.R")
  bastiat=c("bastiat", ls())


  } else {

# New GDrive/AWS stack ----------------------------------------------------


    bastiat.wd = str_extract(getwd(), ".+Bastiat")

    setwd(bastiat.wd)
    pool <<- pool::dbPool(
      drv = RMariaDB::MariaDB(),
      host = gta_pwd('ricardomain')$host,
      username = gta_pwd('ricardomain')$user,
      password = gta_pwd('ricardomain')$password,
      dbname=gta_pwd('ricardomain')$name
    )
    #these are not retained when this is run as a function anyway
    #rm(db.host, db.user, db.password, db.name)
    session.prefix<<-"bt_"

    bastiat=c("bastiat", ls())

  }

}
