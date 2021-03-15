#commented out to allow roxygenisation - uncomment to continue working on this
# bt_sync_gta_star = function(){
#
#   library(gtalibrary)
#   library(gtasql)
#   library(pool)
#   library(RMariaDB)
#   library(data.table)
#
#
#   database <<- "gtamain"
#
#   gta_sql_pool_open(pool.name = "main", db.title=database,
#                     db.host = gta_pwd(database)$host,
#                     db.name = gta_pwd(database)$name,
#                     db.user = gta_pwd(database)$user,
#                     db.password = gta_pwd(database)$password,
#                     table.prefix = "bt_")
#
#   database <<- "ricardomain"
#
#   gta_sql_pool_open(pool.name = "ricdb", db.title=database,
#                     db.host = gta_pwd(database)$host,
#                     db.name = gta_pwd(database)$name,
#                     db.user = gta_pwd(database)$user,
#                     db.password = gta_pwd(database)$password,
#                     table.prefix = "bt_")
#
#
#   ### populate the tables
#   # hint_id = intervention.id
#   # hint_state_id=7
#   # acting_agency="GTA WEBSITE"
#   # hint_date = date.announced
#   jurisdiction_name [from gta_implementing_jurisdiction]
#   # hint_title
#   # hint_description
#   assessment_name [sep =" ; "]
#   jurisdiction_id AS prio_cty [BT ids]
#   collection_id
#   collection_name
#   intevention_type [WB TYPES, sep =" ; "]
#   product_group_name [WB TYPES, sep =" ; "]
#   official [sep =" ; "]
#   news [sep =" ; "]
#   consultancy [sep =" ; "]
#   others [sep =" ; "]
#
#
#
#
#   gta.data=gta_sql_get_value("SELECT gm.id, gm.status_id, gi.id, gi.evaluation_id, gj.name, gm.title, gmt.name, gm.announcement_date, gi.inception_date, gi.removal_date,  gm.source, gm.is_source_official, gi.description
#                       FROM gta_measure gm
#                       JOIN gta_measure_framework gmf
#                       ON gmf.measure_id = gm.id
#                       JOIN gta_intervention gi
#                       ON gi.measure_id = gm.id
#                       JOIN gta_measure_type gmt
#                       ON gi.measure_type_id = gmt.id
#                       JOIN gta_implementing_jurisdiction gij
#                       ON gi.id = gij.intervention_id
#                       JOIN gta_jurisdiction gj
#                       ON gij.jurisdiction_id = gj.id
#                       WHERE gmf.framework_id IN (132,136)
#                       AND gm.status_id!=5;", "main")
#
#
#   names(gta.data)=c("state.act.id","status.id","intervention.id","assessment.id", "country", "title", "intervention.type","announcement.date","inception.date", "removal.date","source", "source.is.official","description")
#   Encoding(gta.data$source)="UTF-8"
#   Encoding(gta.data$description)="UTF-8"
#
#   gta.data$description=gsub("<.*?>","",iconv(gta.data$description, "", "ASCII", "byte"))
#   gta.data$description=textutils::HTMLdecode(gta.data$description)
#   gta.data$description=gsub("<.*?>", "", gta.data$description)
#
#
#   gta.data$source=gsub("<.*?>","",iconv(gta.data$source, "", "ASCII", "byte"))
#   gta.data$source=textutils::HTMLdecode(gta.data$source)
#   gta.data$source=gsub("<.*?>", "", gta.data$source)
#
#   ## intervention_log
#   int.log=unique(gta.data[,c("intervention.id", "state.act.id", "announcement.date")])
#   int.log$intervention.state.id=7
#   int.log$acting.agency="GTA WEBSITE"
#
#   names(int.log)=gsub("\\.","_" , names(int.log))
#   dbWriteTable(conn = ricdb, name = "bt_intervention_log", value = int.log, row.names=F, append=F, overwrite=T)
#
#   ## intervention_text
#   int.text=unique(gta.data[,c("intervention.id", "title", "description")])
#   int.text$description[nchar(int.text$description)>497]=paste(substr(int.text$description[nchar(int.text$description)>497], 1,497), "...")
#   names(int.text)=c("intervention.id", "intervention.title", "intervention.description")
#
#   names(int.text)=gsub("\\.","_" , names(int.text))
#   dbWriteTable(conn = ricdb, name = "bt_intervention_text", value = int.text, row.names=F, append=F, overwrite=T)
#
#   ## b221_intervention_assessment
#   gta.data$assessment.id[gta.data$assessment.id==1]=2
#   int.assess=unique(gta.data[,c("intervention.id","assessment.id")])
#
#   names(int.assess)=gsub("\\.","_" , names(int.assess))
#   dbWriteTable(conn = ricdb, name = "b221_intervention_assessment", value = int.assess, row.names=F, append=F, overwrite=T)
#
#
#   ## b221_intervention_date
#   int.date=gta.data[,c("intervention.id","announcement.date")]
#   int.date$date.type.id=1
#   setnames(int.date, "announcement.date","date")
#   int.date=subset(int.date, ! is.na(date))
#   int.date=unique(int.date)
#   names(int.date)=gsub("\\.","_" , names(int.date))
#   dbWriteTable(conn = ricdb, name = "bt_intervention_date", value = int.date, row.names=F, append=F, overwrite=T)
#
#   int.date=gta.data[,c("intervention.id","inception.date")]
#   int.date$date.type.id=2
#   setnames(int.date, "inception.date","date")
#   int.date=subset(int.date, ! is.na(date))
#   int.date=unique(int.date)
#   names(int.date)=gsub("\\.","_" , names(int.date))
#   dbWriteTable(conn = ricdb, name = "bt_intervention_date", value = int.date, row.names=F, append=T, overwrite=F)
#
#
#
#   int.date=gta.data[,c("intervention.id","removal.date")]
#   int.date$date.type.id=3
#   setnames(int.date, "removal.date","date")
#   int.date=subset(int.date, ! is.na(date))
#   int.date=unique(int.date)
#   names(int.date)=gsub("\\.","_" , names(int.date))
#   dbWriteTable(conn = ricdb, name = "bt_intervention_date", value = int.date, row.names=F, append=T, overwrite=F)
#
#   bt_intervention_jurisdiction
#   bt_intervention_date
#   b221_intervention_product_group
#   b221_intervention_intervention
#   b221_intervention_collection
#
#
#
#
#
#
#
#   ## correcting for EU/EEU
#   gta.supra=gta_sql_get_value("SELECT measure_id, id
#                             FROM gta_intervention
#                             WHERE implementation_level_id=1;","main")
#   names(gta.supra)=c("state.act.id","intervention.id")
#
#   gta.data$country[gta.data$intervention.id %in% gta.supra$intervention.id & gta.data$intervention.id %in% subset(gta.data, country=="Luxembourg")$intervention.id]="European Union"
#   gta.data$country[gta.data$intervention.id %in% gta.supra$intervention.id & gta.data$intervention.id %in% subset(gta.data, country=="Russia")$intervention.id]="Eurasian Economic Union"
#   gta.data$country[gta.data$intervention.id %in% gta.supra$intervention.id & gta.data$intervention.id %in% subset(gta.data, country=="Lesotho")$intervention.id]="Southern African Customs Union"
#   gta.data=unique(gta.data)
#
#   unique(gta.data$country[gta.data$intervention.id %in% gta.supra$intervention.id])
#
#
#   ## remove td.cases
#   td.sa=c(31820, 748, 759)
#
#   gta.data=subset(gta.data, ! state.act.id %in% td.sa)
#
#   ## remove specific interventions
#   int.rm=c(79104, 79259, 58641, 79143, 79486, 79478, 79479,79477)
#
#   gta.data=subset(gta.data, ! intervention.id %in% int.rm)
#
#   ### Product groups
#   ## HS code definitions
#   hs.all=gtalibrary::hs.codes$hs.code
#   hs.food=hs.codes$hs.code[hs.codes$is.covid.food==T]
#   hs.equip=hs.codes$hs.code[hs.codes$is.covid.medical.equipment==T]
#   hs.consum=c(hs.codes$hs.code[hs.codes$is.covid.medical.supplies==T],hs.codes$hs.code[hs.codes$is.covid.antiepidemic.goods==T])
#   hs.drug=hs.codes$hs.code[hs.codes$is.covid.medicines==T]
#   hs.other=hs.all[! hs.all %in% c(hs.food,hs.equip,hs.consum,hs.drug)]
#
#   gta.sa.hs=gta_sql_get_value("SELECT gm.id, gi.id, atl.tariff_line_code
#                       FROM gta_measure gm
#                       JOIN gta_measure_framework gmf
#                       ON gmf.measure_id = gm.id
#                       JOIN gta_intervention gi
#                       ON gi.measure_id = gm.id
#                       JOIN gta_affected_tariff_line atl
#                       ON gi.id = atl.intervention_id
#                       WHERE gmf.framework_id IN (132,136)
#                       AND gm.status_id!=5;","main")
#   names(gta.sa.hs)=c("state.act.id", "intervention.id","hs6")
#   gta.sa.hs$state.act.id=as.numeric(gta.sa.hs$state.act.id)
#   gta.sa.hs$hs6=as.numeric(gta.sa.hs$hs6)
#
#   sa.food=unique(gta.sa.hs$state.act.id[as.numeric(as.character(gta.sa.hs$hs6)) %in% hs.food])
#   sa.equip=unique(gta.sa.hs$state.act.id[as.numeric(as.character(gta.sa.hs$hs6)) %in% hs.equip])
#   sa.consum=unique(gta.sa.hs$state.act.id[as.numeric(as.character(gta.sa.hs$hs6)) %in% hs.consum])
#   sa.drug=unique(gta.sa.hs$state.act.id[as.numeric(as.character(gta.sa.hs$hs6)) %in% hs.drug])
#   sa.other=unique(gta.sa.hs$state.act.id[as.numeric(as.character(gta.sa.hs$hs6)) %in% hs.other])
#
#   ## interventions without HS code
#   missing.hs=unique(gta.data$intervention.id)[! unique(gta.data$intervention.id) %in% gta.sa.hs$intervention.id]
#   is.med.con=c(78883,78982,79095,79094, 79191, 79308, 79306, 79329, 79309, 79298,79389,79390)
#   is.med.eqm=c(78883,78982,79042,79043,79095,79094, 79191, 79308, 79298, 79306, 79299, 79329, 79309, 79084,79389,79390)
#   is.med.drug=c(78871,78883,78982, 79024,79191, 79308, 79329, 79309, 79367,79476, 79436)
#   is.food=c(78990, 79255,79373, 79298)
#   is.other=c(79019, 73754, 79058, 79057, 79059, 79058, 79056, 61769,62514, 12542, 58887, 59221, 79324,79415)
#
#
#   is.irrelevant.for.this=c()
#   missing.hs=missing.hs[! missing.hs %in% c(is.med.con, is.med.eqm ,is.med.drug, is.food, is.other,is.irrelevant.for.this)]
#
#
#   gta.product=data.frame()
#   gta.product=rbind(gta.product,
#                     data.frame(intervention.id=unique(c(is.med.con,gta.data$intervention.id[gta.data$state.act.id %in% sa.consum])),
#                                product.group.name="medical consumables",
#                                stringsAsFactors = F))
#   gta.product=rbind(gta.product,
#                     data.frame(intervention.id=unique(c(is.med.eqm,gta.data$intervention.id[gta.data$state.act.id %in% sa.equip])),
#                                product.group.name="medical equipment",
#                                stringsAsFactors = F))
#   gta.product=rbind(gta.product,
#                     data.frame(intervention.id=unique(c(is.med.drug,gta.data$intervention.id[gta.data$state.act.id %in% sa.drug])),
#                                product.group.name="medicines or drugs",
#                                stringsAsFactors = F))
#   gta.product=rbind(gta.product,
#                     data.frame(intervention.id=unique(c(is.food,gta.data$intervention.id[gta.data$state.act.id %in% sa.food])),
#                                product.group.name="food",
#                                stringsAsFactors = F))
#   gta.product=rbind(gta.product,
#                     data.frame(intervention.id=unique(c(is.other,gta.data$intervention.id[gta.data$state.act.id %in% sa.other])),
#                                product.group.name="other",
#                                stringsAsFactors = F))
#
#
#   #### Intervention types
#   int.exp.barrier=c("Export licensing requirement","Export ban","Export quota","Export tax","Export-related non-tariff measure, nes" )
#   int.imp.barrier=c("Import ban", "Import tariff", "Internal taxation of imports","Import licensing requirement","Import-related non-tariff measure, nes","Anti-dumping","Import tariff quota", "Safeguard")
#   int.dom.subsidy=c( "Tax or social insurance relief","State loan" ,"Financial grant","Loan guarantee" ,"State aid, nes","Interest payment subsidy","Capital injection and equity stakes (including bailouts)","In-kind grant","Production subsidy" , "Price stabilisation"  )
#   int.exp.subsidy=c("Trade finance","Tax-based export incentive","Financial assistance in foreign market","Other export incentive" ,"Export subsidy"  )
#   int.other=c("Controls on credit operations", "Repatriation & surrender requirements" , "Public procurement, nes","Controls on commercial transactions and investment instruments","FDI: Entry and ownership rule", "FDI: Financial incentive","Public procurement preference margin","Labour market access","Public procurement localisation","Post-migration treatment","Local sourcing")
#   int.unclear=c("Instrument unclear")
#
#   if(any(! unique(gta.data$intervention.type) %in% c(int.imp.barrier,int.exp.barrier,int.dom.subsidy,int.exp.subsidy, int.other, int.unclear))){
#     stop(paste0("Need to classify a GTA intervention type into the proper instrument category: ",
#                 paste(unique(gta.data$intervention.type)[! unique(gta.data$intervention.type) %in% c(int.imp.barrier,int.exp.barrier,int.dom.subsidy,int.exp.subsidy, int.other)],  collapse="; ")))
#   }
#
#   gta.intervention=data.frame()
#
#   gta.intervention=rbind(gta.intervention,
#                          data.frame(intervention.id=unique(gta.data$intervention.id[gta.data$intervention.type %in% int.exp.barrier]),
#                                     intervention.type.name="export barrier",
#                                     stringsAsFactors = F))
#   gta.intervention=rbind(gta.intervention,
#                          data.frame(intervention.id=unique(gta.data$intervention.id[gta.data$intervention.type %in% int.imp.barrier]),
#                                     intervention.type.name="import barrier",
#                                     stringsAsFactors = F))
#   gta.intervention=rbind(gta.intervention,
#                          data.frame(intervention.id=unique(gta.data$intervention.id[gta.data$intervention.type %in% int.dom.subsidy]),
#                                     intervention.type.name="domestic subsidy (incl. tax cuts, rescues etc.)",
#                                     stringsAsFactors = F))
#   gta.intervention=rbind(gta.intervention,
#                          data.frame(intervention.id=unique(gta.data$intervention.id[gta.data$intervention.type %in% int.exp.subsidy]),
#                                     intervention.type.name="export subsidy",
#                                     stringsAsFactors = F))
#   gta.intervention=rbind(gta.intervention,
#                          data.frame(intervention.id=unique(gta.data$intervention.id[gta.data$intervention.type %in% int.other]),
#                                     intervention.type.name="other",
#                                     stringsAsFactors = F))
#   gta.intervention=rbind(gta.intervention,
#                          data.frame(intervention.id=unique(gta.data$intervention.id[gta.data$intervention.type %in% int.unclear]),
#                                     intervention.type.name="unclear",
#                                     stringsAsFactors = F))
#
#
#
#
#
#
#
#
#
#
#   gta_sql_pool_close("main")
#   gta_sql_pool_close("ricdb")
#
#
# }
