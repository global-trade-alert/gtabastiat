bt_sa_record_new_source=function(establish.connection=F){


  if(establish.connection){
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
  }

  updated.sa.sources=gta_sql_get_value("SELECT DISTINCT(state_act_id)
                                        FROM gta_source_temp
                                        WHERE source_processed=0;")

  if(is.na(updated.sa.sources)==F){


    ## extract the URLs
    sources.to.parse=gta_sql_get_value(paste0("SELECT id, source FROM gta_measure WHERE id IN (",paste(updated.sa.sources, collapse=","),");"))

    new.urls=data.frame()
    for(i in 1:nrow(sources.to.parse)){
      new.urls=rbind(new.urls,
                     data.frame(state.act.id=sources.to.parse$id[i],
                                url=bt_extract_url(sources.to.parse$source[i]),
                                stringsAsFactors = F))
      print(i)

    }
    new.urls=unique(subset(new.urls, is.na(url)==F))

    ## remove all entries from gta_state_act_source
    gta_sql_update_table(paste0("DELETE FROM gta_state_act_source WHERE state_act_id IN (",paste(unique(new.urls$state.act.id), collapse=","),");"))


    ## add processed URLS to gta_source_log and gta_state_act_source
    rnds.max=length(unique(new.urls$url))
    rnds=1
    for(this.url in unique(new.urls$url)){


      ## Adding URL if not already in gta_source_log
      if(is.na(gta_sql_get_value(paste0("SELECT source_id FROM gta_source_log WHERE source_url = '",this.url,"';")))){

        gta_sql_update_table(paste0("INSERT gta_source_log (source_url)
                                    VALUES ('",this.url,"');"))

      }



      ## checking if URL already in source_log
      url.src.id=gta_sql_get_value(paste0("SELECT source_id FROM gta_source_log WHERE source_url = '",this.url,"';"))

      if(is.na(url.src.id)){stop("Error when adding the URL to gta_source_log.")}

      url.sa.id=unique(subset(new.urls, url==this.url)$state.act.id)

      for(sa.id in url.sa.id){

        gta_sql_update_table(paste0("INSERT INTO gta_state_act_source
                                     VALUES (",url.src.id,",",sa.id,")"))
      }

      print(rnds/rnds.max)
      rnds=rnds+1
    }

    print("Recoreded all new URLs")


    ## mark as processed
    gta_sql_get_value(pate0("UPDATE gta_source_temp
                             SET source_processed=1
                             WHERE state_act_id IN(",paste(updated.sa.sources, collapse=","),");"))


  }
}
