#' Update missing state act sources using rasterise.js.
#'
#' @param timeframe how long ago to check
#' @param update.source.log whether to update the master table
#'
#' @return
#' @export
#'
#' @examples
bt_store_sa_source = function(timeframe = "30",
                              update.source.log = T){

  #these used to be params. should not be changed.
  initialise.source.tables = F
  path.root = "0 source completion"


  #check we are in BT WD
  bastiat.wd = str_extract(getwd(), ".+Bastiat")
  setwd(bastiat.wd)

  library(gtalibrary)
  library(gtasql)
  library(pool)
  library(RMariaDB)
  library(data.table)
  library(stringr)
  library(pdftools)
  library(glue)



  gta_sql_pool_open(db.title="gtamain",
                    db.host = gta_pwd("gtamain")$host,
                    db.name = gta_pwd("gtamain")$name,
                    db.user = gta_pwd("gtamain")$user,
                    db.password = gta_pwd("gtamain")$password,
                    table.prefix = "gta_")

  if(update.source.log){
    print("Updating source log table with new entries")
    ## extracting URLs, if present, and adding them to gta_source_log and gta_state_act_source, where necessary.
    bt_sa_record_new_source(timeframe=timeframe)
  }

  ### Collecting new URLs
  # the old method should not be invoked, hence the '| T'.
  if(!initialise.source.tables | T){
    missing.sources=gta_sql_get_value(glue("SELECT *
                                        FROM gta_url_log gul, gta_measure_url gmu
                                        WHERE gul.id = gmu.url_id
                                        AND (gul.check_status_id <> 0 #0=successfully collected
                                              or gul.check_status_id IS NULL)
                                        AND (gul.last_check IS NULL
                                            OR gul.last_check > (SELECT NOW() - INTERVAL {timeframe} DAY)
                                            );"))
  }else{

    #Robin's approach
    # works in theory. problem is that the gta_files does not have any correspondence to individual URLs, just SA IDs
    # so we can't determine if an individual URL has been gathered or not.



    # define and create folders for output
    path.current.scrape <- paste0(path.root, "/", gsub("\\D", "-", Sys.time()))
    dir.create(path.current.scrape)

    print(paste("Now set directory to:", path.current.scrape))

    path.data <- paste0(path.current.scrape, "/data")
    dir.create(path.data)

    path.files <- paste0(path.current.scrape, "/files")
    dir.create(path.files)

    # get all published measures without an attached file and save them to disk
    missing.sources <- gta_sql_get_value("SELECT *
                                      FROM gta_measure AS gm
                                      LEFT JOIN
                                          (
                                             SELECT field_id
                                             FROM gta_files AS gf
                                             WHERE gf.field_type = 'measure'
                                          ) AS gf
                                      ON gm.id = gf.field_id
                                      WHERE gf.field_id IS NULL
                                      AND gm.status_id = 4;")

    save(missing.sources, file = paste0(path.data, "/missing_sources.RData"))
  }




  #must select executable path. depending on the OS
  #could maybe use a switch?

  #WINDOWS
  if(grepl("Windows", Sys.info()['sysname'])){

    #this assumes your wd and pjs exe are on the same disk and installed in the default directory

    pfx = str_extract(getwd(), "^.+Users/[^/]+")

    phantom.path.os = paste0(pfx, "/AppData/Roaming/PhantomJS/phantomjs")
    #e.g. C:\Users\d-trump\AppData\Roaming\PhantomJS

  } else if(grepl("Darwin", Sys.info()['sysname'])){

    phantom.path.os = "~/Library/Application\\ Support/PhantomJS/phantomjs"

  } else if(grepl("Linux", Sys.info()['sysname'])){

    phantom.path.os = "~/bin/PhantomJS/phantomjs"

  }


  if(nrow(missing.sources)>0){

    for(i in length(missing.sources$url)){

      src.url = missing.sources$url[i]

      ## collect the source,
      ##TBA from here
      ## upload it to AWS (user soucre ID as file name)

      ## ADD collection URL to gta_files (name by source ID and URL-tld)

      ## record file_id in gta_source_log

      library(digest)


      url.timestamp = gsub("\\D", "-", Sys.time())
      url.hash = digest(src.url)
      url.quick.id = gsub("\\.","",str_extract(str_extract(src.url,"((https?://)|(www\\.))[A-Za-z\\.\\-_0-9]+"), "[A-Za-z\\.\\-_0-9]+$"))

      base.file.name = glue("{path.root}/{url.timestamp}_{url.quick.id}-{url.hash}")

      # do the scraping
      print(glue("Attempting scrape of {src.url}... "))
      scrape.result=bt_collect_url(file.name=base.file.name,
                                    url=as.character(src.url),
                                    store.path="temp",
                                    phantom.path = phantom.path.os)
      #returns a list of 4 objs:
      # new.file.name
      # file.suffix
      # url
      # status

      if(scrape.result$status=0){
        cat("Success! Uploading to S3...")
        aws.file.name = paste0(scrape.result$new.file.name, scrape.result$file.suffix)

        aws.url=bt_upload_to_aws(upload.file.name = aws.file.name,
                                 upload.file.path = store.path)


        n.measures = unique(subset(missing.sources, url == src.url)$measure.id) %>% length()
        print(glue("Adding url ID {missing.sources$id[i]} to {n.measures}..."))
        for(sa.id in unique(subset(missing.sources, url == src.url)$measure.id)){

          # update gta_file
          gta.files.sql = paste0("INSERT INTO gta_files (field_id, field_type, file_url, file_name, is_deleted)
                                    VALUES (",sa.id,",'measure','",aws.url,"','",gsub("temp/","", base.file.name),"',0);")
          gta_sql_multiple_queries(gta.files.sql, 1)

          # update gta_url_log
          aws.file.id=gta_sql_get_value(paste0("SELECT MAX(id)
                                              FROM gta_files WHERE file_url = '",aws.url,"';"))

          url.log.sql = glue("UPDATE gta_url_log
                           SET last_check = CURRENT_TIMESTAMP, check_status_id = {scrape.result$status}, s3_url = '{aws.url}', gta_file_id = {aws.file.id}
                           WHERE url = '{src.url}';")

          gta_sql_multiple_queries(url.log.sql, 1)

          # gta_sql_update_table(paste0("INSERT INTO gta_sa_source_url
          #                             VALUES (",sa.id,",'",src.url,"',1,1,CURRENT_TIMESTAMP,1,",aws.file.id,");"))

        }
      }else{

        url.log.sql = glue("UPDATE gta_url_log
                           SET last_check = CURRENT_TIMESTAMP, check_status_id = {scrape.result$status}, s3_url = NULL, gta_file_id = NULL
                           WHERE url = '{src.url}';")
        gta_sql_multiple_queries(url.log.sql, 1)
      }
    }
    # this should now be redundant with the gmu and gul relational tables
    # for(chk.url in check.for.update){
    #
    #   ## check whether I have a record linking the state act id to this URL, if not, add
    #   local.acts=unique(subset(gta.urls, url==chk.url)$state.act.id)
    #   listed.acts=gta_sql_get_value(paste0("SELECT state_act_id
    #                                           FROM gta_sa_source_url WHERE source_url = '",chk.url,"';"))
    #
    #   if(any(! local.acts %in% listed.acts)){
    #
    #     aws.file.id=gta_sql_get_value(paste0("SELECT MAX(source_file_id)
    #                                           FROM gta_sa_source_url WHERE source_url = '",chk.url,"';"))
    #
    #     for(act.id in setdiff(local.acts, listed.acts)){
    #
    #       gta_sql_update_table(paste0("INSERT INTO gta_sa_source_url
    #                                 VALUES (",act.id,",'",chk.url,"',1,1,CURRENT_TIMESTAMP,1,",aws.file.id,");"))
    #
    #     }
    #   }
    #
    # }


  }


#
#   # check listing status per state act of remote
#   for(sa in unique(intersect(gta.urls$state.act.id,documented.urls$state.act.id))){
#
#     #are all urls in documented.urls stil in gta.urls? If not, set listing to F
#
#   }
#
#
#
#   ## checking for attachments
#   has.attachment=gta_sql_get_value("SELECT field_id FROM gta_files WHERE field_type = 'measure' AND is_deleted=0;")
#
#
#   sa.source.repo=unique(data.frame(state.act.id=gta.urls$state.act.id,
#                                    sa.source.attached=gta.urls$state.act.id %in% has.attachment,
#                                    sa.source.contains.url=is.na(gta.urls$url)==F,
#                                    sa.source.collected=F,
#                                    stringsAsFactors = F))
#
#   gta_sql_append_table(append.table = "sa.source.repo",
#                        append.by.df = "sa.source.repo")
#
#
#   gta_sql_create_table(write.df="sa.source.repo",
#                        create.foreign.key = "state.act.id",
#                        foreign.key.parent.table = "measure",
#                        foreign.key.parent.name = "id",
#                        foreign.key.parent.table.prefix = "gta_",
#                        append.existing = F)




  #set trigger on gtamain that marks table as re-check.

  #set CRON


}
