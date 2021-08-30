json_read <- function() {
    if (file.exists("imexhub_database.rds")) {
        json <- read_rds(file = "imexhub_database.rds")
        json %>% fromJSON(simplifyDataFrame = TRUE) %>% bind_rows()
    } else {
        temp <- c() %>% toJSON()
        write_rds(x = temp,file = "imexhub_database.rds")
        tibble()
    }
}
json_write <- function(obj) {
    write_rds(x = obj %>% toJSON(),file = "imexhub_database.rds")
}
json_read_ui_data <- function() {
    imexhub_data <- json_read()

    ui_data <<- list()
    if (imexhub_data %>% nrow() >0) {
        ui_data <<- imexhub_data %>% 
            select(import_id,import_type,import_group,import_basename,import_size,import_mtime,selected_ui,import_datetime,exported) %>%
            group_split(.keep = TRUE,import_id) %>% map(.,function(x){x %>% as.list()})
    }

    ui_data
}
json_update_and_write <- function(mydata,import_group="NULL",import_basename="NULL",import_size="NULL",import_mtime="NULL",import_datetime="",exported=0) {
    mydata$data <- mydata$data %>% cbind(list(selected_ui=1))  #ui loads with checkboxes selected, always exists
    myimport_type=strsplit(import_group,"\\|")[[1]][2]

    imexhub_data <- json_read()

    #get max_import_type_id
    if (imexhub_data %>% nrow() >0) {
        max_import_type_tbl <- imexhub_data %>% filter("import_type" == myimport_type)
        if (max_import_type_tbl %>% nrow() >0) max_import_type_id <- max_import_type_tbl %>% pull(import_id) %>% max() %>% str_extract(pattern = "\\d+") %>% as.numeric()
    }
    max_import_type_id <- 0
    max_import_type_id <- (max_import_type_id+1) %>% as.character() %>% str_pad(width = 2,side = c("left"),pad = "0")  #00 fill for 10+ ids
    my_import_id <- paste0(myimport_type,max_import_type_id)
    
    #combine and write to json
    imexhub_data <- imexhub_data %>% rbind(
        data.frame(
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
        ))
        
    json_write(imexhub_data)
    
    list(import_id=my_import_id,export_type=strsplit(import_group,"\\|")[[1]][1],import_type=myimport_type)
}
json_delete <- function(import_id) {
    imexhub_data <- json_read()

    import_id_text <- import_id
    imexhub_data <- imexhub_data %>% filter(import_id != import_id_text)
    
    json_write(imexhub_data)
}


#sqlite
sqlite_connect <- function(db_name) {
    dbConnect(RSQLite::SQLite(), db_name)
}
sqlite_update_and_write <- function(my_import_id,mydata) {
    my_db_name <- str_c("db_files/",my_import_id,".db")
    sqlite_connection <- sqlite_connect(my_db_name)

    RSQLite::dbWriteTable(sqlite_connection, my_import_id, mydata$data,overwrite=FALSE)

    dbDisconnect(sqlite_connection)
}
sqlite_delete <- function(my_import_id) {
    my_db_name <- str_c("db_files/",my_import_id,".db")

    file.remove(my_db_name)
}
sqlite_read_data <- function(my_import_id) {
    my_db_name <- str_c("db_files/",my_import_id,".db")
    sqlite_connection <- sqlite_connect(my_db_name)

    data_tbl <- DBI::dbReadTable(sqlite_connection, my_import_id)

    dbDisconnect(sqlite_connection)
    data_tbl
}
