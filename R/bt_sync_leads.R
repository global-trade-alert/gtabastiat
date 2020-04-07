# Roxygen documentation

#' Bastiat, please extract all URLs you find in the following string.


# Function infos and parameters  --------------------------------------------

bt_sync_leads = function(){

  library(gtalibrary)
  library(gtasql)
  library(pool)
  library(RMariaDB)
  library(data.table)


  database <<- "gtamain"

  gta_sql_pool_open(pool.name = "main", db.title=database,
                    db.host = gta_pwd(database)$host,
                    db.name = gta_pwd(database)$name,
                    db.user = gta_pwd(database)$user,
                    db.password = gta_pwd(database)$password,
                    table.prefix = "bt_")

  database <<- "ricardomain"

  gta_sql_pool_open(pool.name = "ricdb", db.title=database,
                    db.host = gta_pwd(database)$host,
                    db.name = gta_pwd(database)$name,
                    db.user = gta_pwd(database)$user,
                    db.password = gta_pwd(database)$password,
                    table.prefix = "bt_")

  ## (1)
  ## fetch lead.ids that are not in hint.log from from gta main


  ## (2)
  ## fetch new evaluations and record them on bt_hint_evaluation
  useful=gta_sql_get_value("SELECT id FROM gta_leads WHERE removal_reason IS NOT NULL AND removal_reason != 'IRREVELANT';", "main")
  useless=gta_sql_get_value("SELECT id FROM gta_leads WHERE removal_reason = 'IRREVELANT';", "main")


  gta_sql_update_table(paste0("UPDATE bt_hint_evaluation
                              SET evaluation_id=2
                              WHERE hint_id IN (SELECT hint_id
                                                FROM bt_hint_lead
                                                WHERE lead_id IN(",paste(useful, collapse=","),"));"), "ricdb")

  gta_sql_update_table(paste0("UPDATE bt_hint_evaluation
                              SET evaluation_id=3
                              WHERE hint_id IN (SELECT hint_id
                                                FROM bt_hint_lead
                                                WHERE lead_id IN(",paste(useless, collapse=","),"));"), "ricdb")

  gta_sql_pool_close("main")
  gta_sql_pool_close("ricdb")


}
