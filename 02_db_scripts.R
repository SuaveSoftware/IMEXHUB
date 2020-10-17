mongo_connect_local <- function(collection,database,
                                host = config$host,
                                port = config$port) {
    mongo(
        collection = collection,
        url = str_glue("mongodb://{host}:{port}/{database}")
    )
}
sqlite_connect <- function(db_name) {
    dbConnect(RSQLite::SQLite(), db_name)
}

mongo_read_ui_data <- function() {
    mongo_connection <- mongo_connect_local(collection = "myfirstcollection", database = "myfirstdb")
    
    it <- mongo_connection$iterate(query = str_c('{}'))
    mydata <<- list()  #list(import_type=tibble)
    while (!is.null(x <- it$one())) {
        #get all data from sqlite db
        # data_sql <- sqlite_read_data(x$import_id)
        
        #get ui data from mongo db
        data_mon <- x$data[1:10] %>% map(function(y) {y %>% data.frame()}) %>% rbind.fill()  #%>% filter(selected_ui==1)
        
        #selected columns for ui
        selected_columns_labels <- x$selected_columns_labels %>% unlist()
        selected_columns <- x$selected_columns %>% unlist()
        selected_columns <- enquo(selected_columns)
        
        #valuebox subtitles
        valuebox_subtitles <- list(
            count_label = x$valuebox_subtitles$count_label[[1]],
            sum_label = x$valuebox_subtitles$sum_label[[1]],
            min_label = x$valuebox_subtitles$min_label[[1]],
            max_label = x$valuebox_subtitles$max_label[[1]]
        )

        #valuebox icons
        valuebox_icons <- list(
            count_column = x$valuebox_icons$count_column[[1]],
            sum_column = x$valuebox_icons$sum_column[[1]],
            min_column = x$valuebox_icons$min_column[[1]],
            max_column = x$valuebox_icons$max_column[[1]]
        )

        list_temp <- list(
            import_id = x$import_id,
            import_type = x$import_type,
            import_group = x$import_group,
            import_basename = x$import_basename,
            import_size = x$import_size,
            import_mtime = x$import_mtime,
            valuebox_subtitles = valuebox_subtitles,
            valuebox_icons = valuebox_icons,
            count_value = x$count_value,
            sum_value = x$sum_value,
            min_value = x$min_value,
            max_value = x$max_value,
            selected_ui = x$selected_ui,
            import_datetime = x$import_datetime,
            exported = x$exported,
            data = data_mon %>% as_tibble() %>% select(!!selected_columns) %>% setNames(selected_columns_labels))
        mydata <<- mydata %>% append(list(list_temp))
    }
    
    ui_data <<- list()
    lapply(seq_along(mydata),function(n){
        ui_data[[n]] <<- list(mytitle=mydata[[n]]$import_id,
                              import_type=mydata[[n]]$import_type,
                              import_group=mydata[[n]]$import_group,
                              import_basename=mydata[[n]]$import_basename,
                              import_size=mydata[[n]]$import_size,
                              import_mtime=mydata[[n]]$import_mtime,
                              valuebox_subtitles=mydata[[n]]$valuebox_subtitles,  #list()
                              valuebox_icons=mydata[[n]]$valuebox_icons,  #list()
                              count_value=mydata[[n]]$count_value,
                              sum_value=mydata[[n]]$sum_value,
                              min_value=mydata[[n]]$min_value,
                              max_value=mydata[[n]]$max_value,
                              selected_ui=mydata[[n]]$selected_ui,
                              import_datetime=mydata[[n]]$import_datetime,
                              exported=mydata[[n]]$exported,
                              data=mydata[[n]]$data)  #tbl()
    })

    mongo_connection$disconnect()
    ui_data
}

#data    
mongo_update_and_write <- function(mydata,import_group="NULL",import_basename="NULL",import_size="NULL",import_mtime="NULL",import_datetime="",exported=0) {
    mongo_connection <- mongo_connect_local(collection = "myfirstcollection", database = "myfirstdb")

    mydata$data <- mydata$data %>% cbind(list(selected_ui=1))  #ui loads with checkboxes selected, always exists
    myimport_type=strsplit(import_group,"\\|")[[1]][2]
    #my_import_id <- paste0("import_",mongo_connection$count()+1)
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
        exported=exported,
        
        data=list(mydata$data %>% slice(1:2))  #keep null columns in object?
    ) %>% 
        mongo_connection$insert()
    
    mongo_connection$disconnect()
    
    list(import_id=my_import_id,export_type=strsplit(import_group,"\\|")[[1]][1],import_type=myimport_type)
}

#user_settings and related values
mongo_update_and_write_user_settings <- function(data_list,import_id_list,all_import_type_settings) {
    mongo_connection <- mongo_connect_local(collection = "myfirstcollection", database = "myfirstdb")
    data_tbl <- data_list$data
    
    import_id=import_id_list$import_id
    export_type=import_id_list$export_type
    import_type=import_id_list$import_type

    #get valuebox_values
    valuebox_columns <- list(
        count_column = all_import_type_settings[[export_type]][[import_type]]$valuebox_columns$count_column[[1]],
        sum_column = all_import_type_settings[[export_type]][[import_type]]$valuebox_columns$sum_column[[1]],
        min_column = all_import_type_settings[[export_type]][[import_type]]$valuebox_columns$min_column[[1]],
        max_column = all_import_type_settings[[export_type]][[import_type]]$valuebox_columns$max_column[[1]]
    )

    #distinct column for count
    temp_symbol <- valuebox_columns$count_column
    distinct_col_value <- enquo(temp_symbol)

    #sum column
    temp_symbol <- valuebox_columns$sum_column
    sum_col_value <- enquo(temp_symbol)

    #min/max column
    temp_symbol <- valuebox_columns$min_column
    min_col_value <- enquo(temp_symbol)
    temp_symbol <- valuebox_columns$max_column
    max_col_value <- enquo(temp_symbol)

    # merge all_import_type_settings, data_tbl, valuebox_values
    my_values <- list(
        count_value = data_tbl %>% select(!!distinct_col_value) %>% count() %>% nrow(),
        sum_value = data_tbl %>% nrow(),  #fallback? eval()?  #pull(!!sum_col_value) %>% sum()
        min_value = data_tbl %>% select(!!min_col_value) %>% dplyr::arrange(!!min_col_value) %>% slice(1) %>% pluck(1),
        max_value = data_tbl %>% select(!!max_col_value) %>% dplyr::arrange(!!max_col_value) %>% slice(data_tbl%>% nrow()) %>% pluck(1)
    )
    
    update_string <- tibble(
        selected_columns_labels=list(all_import_type_settings[[export_type]][[import_type]]$selected_columns_labels),
        selected_columns=list(all_import_type_settings[[export_type]][[import_type]]$selected_columns),
        valuebox_subtitles=list(all_import_type_settings[[export_type]][[import_type]]$valuebox_subtitles),
        valuebox_icons=list(all_import_type_settings[[export_type]][[import_type]]$valuebox_icons),
        valuebox_columns=list(all_import_type_settings[[export_type]][[import_type]]$valuebox_columns),

        count_value = my_values$count_value,
        sum_value = my_values$sum_value,
        min_value = my_values$min_value %>% as.character() %>% str_extract(.,"(.{1,10})"),
        max_value = my_values$max_value %>% as.character() %>% str_extract(.,"(.{1,10})"),
    )  %>% 
        toJSON() %>%
        str_remove_all(pattern = "^\\[|\\]$")

    mongo_connection$update(
        query = str_c('{"import_id":"',import_id,'"}'),
        update = str_c('{"$set" : ',update_string,' }'),
        upsert = FALSE)
    
    mongo_connection$disconnect()
}

sqlite_update_and_write <- function(my_import_id,mydata) {
    my_db_name <- str_c("db_files/",my_import_id,".db")
    sqlite_connection <- sqlite_connect(my_db_name)
    
    #add all data from sqlite db to new file
    dbWriteTable(sqlite_connection, my_import_id, mydata$data,overwrite=FALSE)
    
    dbDisconnect(sqlite_connection)
}

# mongo_aggregate <- function() {
#     mongo_connection <- mongo_connect_local(collection = "myfirstcollection", database = "myfirstdb")
#     overview_stats_from_db <<- list()
#     
#     overview_stats_from_db$import_type_count_df <<-
#         mongo_connection$aggregate(pipeline = '[{ "$group" : { "_id" : "$import_type" , "number_records" : {"$sum":1} } }]')
#     
#     overview_stats_from_db$export_type_count_df <<-
#         mongo_connection$aggregate(pipeline = '[{ "$group" : { "_id" : "$export_type" , "number_records" : {"$sum":1} } }]')
#     
#     overview_stats_from_db$document_count <<- 
#         mongo_connection$count(query = '{}')
# 
#     mongo_connection$disconnect()
#     
#     overview_stats_from_db
# }

mongo_delete <- function(import_id) {
    mongo_connection <- mongo_connect_local(collection = "myfirstcollection", database = "myfirstdb")
    mongo_archive_connection <- mongo_connect_local(collection = "archive_col", database = "myfirstdb")
    
    #get data using mongo_connection
    old_data_tbl <- mongo_connection$find(query = str_c('{ "import_id": "',import_id,'" }'))    
    
    #insert data using mongo_archive_connection
    old_data_tbl %>% 
        select(-data) %>%  #dev only!!!
        mongo_archive_connection$insert()
    mongo_archive_connection$disconnect()
    
    #delete using mongo_connection
    mongo_connection$remove(query = str_c('{ "import_id":"',import_id,'" }'),just_one = TRUE)
    mongo_connection$disconnect()
}

sqlite_delete <- function(my_import_id) {
    my_db_name <- str_c("db_files/",my_import_id,".db")
    
    file.remove(my_db_name)
}

#change selected_ui at document level
mongo_update_selected_ui <- function(selected_import_ids) {
    mongo_connection <- mongo_connect_local(collection = "myfirstcollection", database = "myfirstdb")
    
    it <- mongo_connection$iterate(query = str_c('{}'))
    while (!is.null(x <- it$one())) {
        # p_selected_ui <- x$selected_ui
        
        selected_ui_value <- 0
        if (x$import_id %in% selected_import_ids) {
            selected_ui_value <- 0
        } else {
            selected_ui_value <- 1
        }
        
        mongo_connection$update(
            query = str_c('{"import_id":"',x$import_id,'"}'),
            update = str_c('{"$set" : {"selected_ui":',selected_ui_value,'} }'),
            upsert = TRUE)
    }
    
    mongo_connection$disconnect()
}

#EXPORT: check types seleted -> match export rules -> read from db -> process -> save
mongo_get_selected_ui <- function(param=1) {
    mongo_connection <- mongo_connect_local(collection = "myfirstcollection", database = "myfirstdb")
    
    it <- mongo_connection$iterate(query = str_c('{"selected_ui":',param,'}'))
    selected_imports <- list()
    while (!is.null(x <- it$one())) {
        paste(x$import_id)
        
        selected_imports <- selected_imports %>% append(
            list(list(
                import_id = x$import_id,
                export_type = x$export_type,
                import_type = x$import_type
            ))
        )
    }
    
    mongo_connection$disconnect()
    selected_imports  #list(list(import_id=import_id,export_type=export_type,import_type=import_type),...)
}
sqlite_read_data <- function(my_import_id) {
    my_db_name <- str_c("db_files/",my_import_id,".db")
    sqlite_connection <- sqlite_connect(my_db_name)
    
    #get data_sql
    data_tbl <- dbReadTable(sqlite_connection, my_import_id)
    
    dbDisconnect(sqlite_connection)
    data_tbl
}
mongo_update_exported <- function(selected_imports) {
    mongo_connection <- mongo_connect_local(collection = "myfirstcollection", database = "myfirstdb")
    
    selected_imports %>%
        map(function(x) {
            mongo_connection$update(
                query = str_c('{"import_id":"',x$import_id,'"}'),
                update = str_c('{"$set" : {"exported":1} }'),
                upsert = TRUE)
        })
    
    mongo_connection$disconnect()
}




