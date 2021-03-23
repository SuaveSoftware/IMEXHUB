config <- config::get(file = "config.yaml")
mongo_connect <- function(collection, database,
             host = config$host,
             port = config$port,
             username = config$username,
             password = config$password) {
    
    if (port != "" && username != "") {
        mongo(
            collection = collection,
            url = str_glue("mongodb://{username}:{password}@{host}:{port}/{database}")
        )
    } else if (port != "") {  #local
        mongo(
            collection = collection,
            url = str_glue("mongodb://{host}:{port}/{database}")
        )
    } else {
        mongo(
            collection = collection,
            url = str_glue("mongodb+srv://{username}:{password}@{host}/{database}")
        )
    }
}
sqlite_connect <- function(db_name) {
    dbConnect(RSQLite::SQLite(), db_name)
}

mongo_read_ui_data <- function() {
    mongo_connection <- mongo_connect(collection = "myfirstcollection", database = "myfirstdb")
    
    it <- mongo_connection$iterate(query = str_c('{}'))
    mydata <<- list()  #list(import_type=tibble)
    while (!is.null(x <- it$one())) {
        list_temp <- list(
            import_id = x$import_id,
            import_type = x$import_type,
            import_group = x$import_group,
            import_basename = x$import_basename,
            import_size = x$import_size,
            import_mtime = x$import_mtime,
            selected_ui = x$selected_ui,
            import_datetime = x$import_datetime,
            exported = x$exported
            )
        mydata <<- mydata %>% append(list(list_temp))
    }
    
    ui_data <<- list()
    lapply(seq_along(mydata),function(n){
        ui_data[[n]] <<- list(import_id=mydata[[n]]$import_id,
                              import_type=mydata[[n]]$import_type,
                              import_group=mydata[[n]]$import_group,
                              import_basename=mydata[[n]]$import_basename,
                              import_size=mydata[[n]]$import_size,
                              import_mtime=mydata[[n]]$import_mtime,
                              selected_ui=mydata[[n]]$selected_ui,
                              import_datetime=mydata[[n]]$import_datetime,
                              exported=mydata[[n]]$exported
                              )  
    })

    mongo_connection$disconnect()
    ui_data
}

#data
mongo_update_and_write <- function(mydata,import_group="NULL",import_basename="NULL",import_size="NULL",import_mtime="NULL",import_datetime="",exported=0) {
    mongo_connection <- mongo_connect(collection = "myfirstcollection", database = "myfirstdb")

    mydata$data <- mydata$data %>% cbind(list(selected_ui=1))  #ui loads with checkboxes selected, always exists
    myimport_type=strsplit(import_group,"\\|")[[1]][2]
    #get max import_type import_id
    max_import_type_tbl <-
        mongo_connection$find(query = str_c('{ "import_type": "',myimport_type,'" }'),
                              fields = '{"import_id":true}',
                              limit = 1,
                              sort = '{"import_id": -1}')
    max_import_type_id <- 0
    if (max_import_type_tbl %>% nrow() >0) max_import_type_id <- max_import_type_tbl %>% pull(import_id) %>% str_extract(pattern = "\\d+") %>% as.numeric()
    max_import_type_id <- (max_import_type_id+1) %>% as.character() %>% str_pad(width = 2,side = c("left"),pad = "0")  #00 fill for 10+ ids
    my_import_id <- paste0(myimport_type,max_import_type_id)

    tibble(
        import_id=my_import_id,
        export_type=strsplit(import_group,"\\|")[[1]][1],
        import_type=myimport_type,
        import_group=import_group,
        import_basename=import_basename,
        import_size=import_size,
        import_mtime=import_mtime,
        import_datetime=import_datetime,
        selected_ui=1,
        exported=exported
    ) %>%
        mongo_connection$insert()

    mongo_connection$disconnect()

    list(import_id=my_import_id,export_type=strsplit(import_group,"\\|")[[1]][1],import_type=myimport_type)
}
sqlite_update_and_write <- function(my_import_id,mydata) {
    my_db_name <- str_c("db_files/",my_import_id,".db")
    sqlite_connection <- sqlite_connect(my_db_name)

    #add all data from sqlite db to new file
    RSQLite::dbWriteTable(sqlite_connection, my_import_id, mydata$data,overwrite=FALSE)

    dbDisconnect(sqlite_connection)
}

mongo_delete <- function(import_id) {
    mongo_connection <- mongo_connect(collection = "myfirstcollection", database = "myfirstdb")

    #delete using mongo_connection
    mongo_connection$remove(query = str_c('{ "import_id":"',import_id,'" }'),just_one = TRUE)
    mongo_connection$disconnect()
}
sqlite_delete <- function(my_import_id) {
    my_db_name <- str_c("db_files/",my_import_id,".db")

    file.remove(my_db_name)
}

# #EXPORT: check types seleted -> match export rules -> read from db -> process -> save
# mongo_get_selected_ui <- function(param=1) {
#     mongo_connection <- mongo_connect(collection = "myfirstcollection", database = "myfirstdb")
# 
#     it <- mongo_connection$iterate(query = str_c('{"selected_ui":',param,'}'))
#     selected_imports <- list()
#     while (!is.null(x <- it$one())) {
#         paste(x$import_id)
# 
#         selected_imports <- selected_imports %>% append(
#             list(list(
#                 import_id = x$import_id,
#                 export_type = x$export_type,
#                 import_type = x$import_type
#             ))
#         )
#     }
# 
#     mongo_connection$disconnect()
#     selected_imports  #list(list(import_id=import_id,export_type=export_type,import_type=import_type),...)
# }
sqlite_read_data <- function(my_import_id) {
    my_db_name <- str_c("db_files/",my_import_id,".db")
    # print("got here")
    # # sqlite_connect(my_db_name)
    sqlite_connection <- dbConnect(RSQLite::SQLite(), my_db_name)
    
    #get data_sql
    data_tbl <- DBI::dbReadTable(sqlite_connection, my_import_id)

    dbDisconnect(sqlite_connection)
    data_tbl
}
# mongo_update_exported <- function(selected_imports) {
#     mongo_connection <- mongo_connect(collection = "myfirstcollection", database = "myfirstdb")
#     
#     selected_imports %>%
#         map(function(x) {
#             mongo_connection$update(
#                 query = str_c('{"import_id":"',x$import_id,'"}'),
#                 update = str_c('{"$set" : {"exported":1} }'),
#                 upsert = TRUE)
#         })
#     
#     mongo_connection$disconnect()
# }
# 
# 
# 
# 
