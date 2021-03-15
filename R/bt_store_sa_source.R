# Roxygen documentation

#' Bastiat, please extract all URLs you find in the following string.


# Function infos and parameters  --------------------------------------------

bt_store_sa_source = function(){

  library(gtalibrary)
  library(gtasql)
  library(pool)
  library(RMariaDB)
  library(data.table)

  gta_sql_pool_open(db.title="gtamain",
                    db.host = gta_pwd("gtamain")$host,
                    db.name = gta_pwd("gtamain")$name,
                    db.user = gta_pwd("gtamain")$user,
                    db.password = gta_pwd("gtamain")$password,
                    table.prefix = "gta_")


  ## extracting URLs, if present, and adding them to gta_source_log and gta_state_act_source, where necessary.
  bt_sa_record_new_source()


  ### Collecting new URLs

  new.sources=gta_sql_get_value("SELECT source_id, url
                                 FROM gta_source_log
                                 WHERE source_collected=0;")

  if(nrow(new.sources)>0){

    for(src.url in need.to.collect){

      ## collect the source,
      ##TBA from here
      ## upload it to AWS (user soucre ID as file name)

      ## ADD collection URL to gta_files (name by source ID and URL-tld)

      ## record file_id in gta_source_log



      base.file.name=gsub("\\.","",str_extract(str_extract(src.url,"((https?://)|(www\\.))[A-Za-z\\.\\-_0-9]+"), "[A-Za-z\\.\\-_0-9]+$"))

      this.file.name=bt_collect_url(file.name=base.file.name,
                                    url=as.character(src.url),
                                    store.path="temp")


      aws.url=bt_upload_to_aws(upload.file=this.file.name)


      for(sa.id in unique(subset(new.sources, url == src.url)$state.act.id)){

        # update gta_file
        gta_sql_update_table(paste0("INSERT INTO gta_files (field_id, field_type, file_url, file_name, is_deleted)
                                    VALUES (",sa.id,",'measure','",aws.url,"','",gsub("temp/","", this.file.name),"',0);"))

        # update gta_sa_soure_url
        aws.file.id=gta_sql_get_value(paste0("SELECT MAX(id)
                                              FROM gta_files WHERE file_url = '",src.url,"';"))

        gta_sql_update_table(paste0("INSERT INTO gta_sa_source_url
                                    VALUES (",sa.id,",'",src.url,"',1,1,CURRENT_TIMESTAMP,1,",aws.file.id,");"))


      }
    }

    for(chk.url in check.for.update){

      ## check whether I have a record linking the state act id to this URL, if not, add
      local.acts=unique(subset(gta.urls, url==chk.url)$state.act.id)
      listed.acts=gta_sql_get_value(paste0("SELECT state_act_id
                                              FROM gta_sa_source_url WHERE source_url = '",chk.url,"';"))

      if(any(! local.acts %in% listed.acts)){

        aws.file.id=gta_sql_get_value(paste0("SELECT MAX(source_file_id)
                                              FROM gta_sa_source_url WHERE source_url = '",chk.url,"';"))

        for(act.id in setdiff(local.acts, listed.acts)){

          gta_sql_update_table(paste0("INSERT INTO gta_sa_source_url
                                    VALUES (",act.id,",'",chk.url,"',1,1,CURRENT_TIMESTAMP,1,",aws.file.id,");"))

        }
      }

    }


  }



  # check listing status per state act of remote
  for(sa in unique(intersect(gta.urls$state.act.id,documented.urls$state.act.id))){

    #are all urls in documented.urls stil in gta.urls? If not, set listing to F

  }



  ## checking for attachments
  has.attachment=gta_sql_get_value("SELECT field_id FROM gta_files WHERE field_type = 'measure' AND is_deleted=0;")


  sa.source.repo=unique(data.frame(state.act.id=gta.urls$state.act.id,
                                   sa.source.attached=gta.urls$state.act.id %in% has.attachment,
                                   sa.source.contains.url=is.na(gta.urls$url)==F,
                                   sa.source.collected=F,
                                   stringsAsFactors = F))

  gta_sql_append_table(append.table = "sa.source.repo",
                       append.by.df = "sa.source.repo")


  gta_sql_create_table(write.df="sa.source.repo",
                       create.foreign.key = "state.act.id",
                       foreign.key.parent.table = "measure",
                       foreign.key.parent.name = "id",
                       foreign.key.parent.table.prefix = "gta_",
                       append.existing = F)




  #set trigger on gtamain that marks table as re-check.

  #set CRON


}
