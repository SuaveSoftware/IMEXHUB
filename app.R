# LIBRARIES ----
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(shinyFiles)
library(shinyjs)

library(config)

library(tidyverse)
library(lubridate)
library(readxl)
library(openxlsx)  #for write
library(DT)
library(rdrop2)  #https://github.com/karthik/rdrop2

library(mongolite) # Resource: https://jeroen.github.io/mongolite/
library(jsonlite)
library(RSQLite)


#SETUP
options(shiny.maxRequestSize = 80*1024^2)
theme <- "paper"
button_theme_search <- "primary"
Sys.setenv(R_CONFIG_ACTIVE = "local")
config <<- config::get(file = "config.yaml")
source(file = "02_db_scripts.R")
source(file = "03_file_scripts_general.R")
source(file = "08_gladage_export.R")
source(file = "09_paypal_export.R")


# FUNCTIONS ----
sendnotification <- function(msg,duration,id,session) {
    print(msg)
    notification_id <<- showNotification(msg,
                                         duration = duration,
                                         closeButton = FALSE,
                                         id = id,
                                         session = session)
    addClass(id = str_c("shiny-notification-",id), class = "customclass2")
}
process_files <- function(file_selected,session) {
    #pre open file checks, rule 1: is file already in imexhub_collection?
    new_file_check_output <- file_selected %>% rowwise() %>% mutate( "new_file_check_output" = new_file_check(name,size) )
    skipped_files <- new_file_check_output[,1][new_file_check_output[,5]==FALSE] %>% matrix(ncol = 1)
    new_files_mat <- new_file_check_output[,1][new_file_check_output[,5]==TRUE] %>% matrix(ncol = 1)  #returns new files
    notification_id <<- sendnotification(msg = str_c("new_file_check_output:",new_files_mat, " skipped_files:",skipped_files,collapse = TRUE),
                                         duration = 2,id = "my_notification1",session = session)
    addClass(id = "shiny-notification-panel", class = "customclass1")  #need to run once
    req(new_files_mat %>% nrow() >0)  #only need to proceed with new files
    file_selected <- new_file_check_output[new_file_check_output$new_file_check_output==TRUE,]
    
    #fill for archive check, has file been exported before?!!!
    new_files_exported_mat <- data.frame(row.names = NULL,file_selected,new_files_exported_mat=1)
    
    #first line export_type identification
    new_files_exported_importgroup_mat <- new_files_exported_mat %>% rowwise() %>% mutate( "new_files_exported_importgroup_mat" = get_group(datapath,name) )
    notification_id <<- sendnotification(msg = str_c("import_group_output:",new_files_exported_importgroup_mat[,"new_files_exported_importgroup_mat"] %>% paste(.,collapse = ',')),
                                         duration = 2,id = "my_notification3",session = session)
    req(sum(new_files_exported_importgroup_mat[,"new_files_exported_importgroup_mat"]=="") ==0)  #no null import_groups
    new_files_exported_importgroup_mat <- new_files_exported_importgroup_mat[new_files_exported_importgroup_mat$new_files_exported_importgroup_mat != "",]  #remove missing import_groups
    
    #parameters for all imports
    import_datetime <- Sys.time()
    
    1:nrow(new_files_exported_importgroup_mat) %>% map(function(x) {
        addClass(id = "shiny-notification-panel", class = "customclass1")  #need to run once (added here solves lost center container issue)
        name <- new_files_exported_importgroup_mat[x,"name"] %>% unlist()
        size <- new_files_exported_importgroup_mat[x,"size"] %>% unlist()
        datapath <- new_files_exported_importgroup_mat[x,"datapath"] %>% unlist()
        exported <- new_files_exported_importgroup_mat[x,"new_files_exported_mat"] %>% unlist()
        import_group <- new_files_exported_importgroup_mat[x,"new_files_exported_importgroup_mat"] %>% unlist()
        
        #preprocessing
        data <- data_preprocessed(datapath,import_group)  #list(data=data)
        notification_id <<- sendnotification(msg = str_c("ran data_preprocessed: ",datapath," - ",import_group),
                                             duration = 2,id = str_c("my_notification",3+x),session = session)
        
        #add to db (mydata,import_group,import_basename,import_size,import_mtime) +import_time
        # import_mtime <- file.info(datapath)$mtime
        my_import_id_list <- mongo_update_and_write(mydata = data,
                                                    import_group = import_group,
                                                    import_basename = name,
                                                    import_size = size,
                                                    import_mtime = '',
                                                    import_datetime = import_datetime,
                                                    exported = exported)
        my_import_id <- my_import_id_list$import_id
        notification_id <<- sendnotification(msg = str_c("ran mongo_update_and_write: ",my_import_id," - ",import_group),
                                             duration = 2,id = str_c("my_notification",4+x),session = session)
        
        #add all data to new sqlite file
        sqlite_update_and_write(my_import_id,data)
        notification_id <<- sendnotification(msg = str_c("ran sqlite_update_and_write: ",my_import_id),
                                             duration = 5,id = str_c("my_notification",6+x),session = session)
    })
}

ui <- fluidPage(
    useShinyjs(),
    
    # CSS ----
    tags$head(
        # tags$link(rel = "stylesheet", type = "text/css", href = shinytheme("cyborg")),
        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
    ),
    inlineCSS(list(.customclass1 = "top: 0 !important; left: calc(50% - 250px) !important; width: 500px !important;")),
    inlineCSS(list(.customclass2 = "background-color: rgba(241, 160, 85, 0.53) !important;")),
    tags$style(".btn-file {width: 180px; text-align: left;cursor: pointer;}"),
    tags$style("#GetPayPal {cursor: default;pointer-events: none;}"),
    tags$style("#button4 {cursor: default;pointer-events: none;}"),
    tags$style(".shiny-input-container {margin-left: 4px;margin-bottom: 0px;}"),
    tags$style(".shiny-file-input-progress {display: none;}"),  #inline or none
    tags$style("input.form-control {width:0px; visibility: hidden}"),
    inlineCSS(list(.btn = "text-transform: none !important; -webkit-box-shadow: none !important; box-shadow: none !important; ")),

    #1.0 HEAD ----
    tagList(
        tags$head(HTML("<title>IMEXHUB</title>"))
    ),
    style = "padding:0px;",

    
    #2.0 NAVBAR ----
    navbar <- shiny::navbarPage(
        #2.1 Application Title
        title = div(
            tags$img(
                src = "https://www.business-science.io/img/business-science-logo.png",
                width = "30",
                height = "30",
                style = "webkit-filter: drop-shadow(3px 3px 3px #222)"
            ),
            "IMEXHUB"
        ),
        collapsible = FALSE,
        theme = shinytheme(theme)
    ),

        
    #2.1 Main panel output ----
    mainPanel(width = 12,style="",class="container",
              div(class="row",style="margin:0px;",
                         #build my_import_export_card here...
                         div(class = "panel col-xs-12 col-sm-6 col-md-6 col-lg-4",style="margin: 0px;padding: 10px;border: 1px solid #D3D3D3;border-radius:0px;box-shadow:0 14px 18px rgba(0,0,0,0.3);",
                             div(class = "panel-footer",style = "padding:10px 10px 10px 10px;text-align: center;background:none;",
                                 dropdownButton(
                                     fileInput(inputId = 'GetFileInput', label = NULL, multiple = TRUE,buttonLabel = "File",width = "100%"),  #accept = c('text/csv','text/comma-separated-values','.csv'
                                     actionLink(inputId = "GetDropbox",label="DropBox",class=str_glue("btn-block")),
                                     actionLink(inputId = "GetPayPal",label="PayPal",class=str_glue("btn-block")),
                                     actionLink(inputId = "button4",label="Bills API",class=str_glue("btn-block")),
                                     circle = FALSE, status = "primary",
                                     tooltip = NULL,
                                     inline = TRUE,
                                     label = "IMPORT"
                                 ),
                             ),
                             div(
                                 class = "panel-body",style = "padding:0px 10px 0px 10px;",
                                 uiOutput("file_datatable_ui"),
                             ),
                             div(
                                 class = "panel-footer",style = "padding:0px 10px 10px 10px;text-align: center;background:none;",
                                 actionButton(inputId = "init",label="EXPORT",class=str_glue("btn-{button_theme_search}"),style="margin:0px;width:92px;")
                             ),
                         ),
                         
                  downloadButton(outputId = "export_button", "EXPORT",class = str_glue("btn-{button_theme_search}"),style = "visibility: hidden;height:0px;"),
                  
              )
    )

    
)

# SERVER ----
server <- function(input, output, session) {
    
    #SETUP
    reactive_values <- reactiveValues(ui_data = NULL, folder_ignore_already_exported = FALSE)
    reactive_values$ui_data <- mongo_read_ui_data()
    notification_id <- NULL


    #LOCAL FILE INPUT
    observe({
        if (!is.null(input$GetFileInput)) {
            file_selected <- input$GetFileInput
            reactive_values$file_path <- file_selected$datapath  #$name,size,type,datapath
            req(reactive_values$file_path %>% length() >0)
            
            process_files(file_selected,session)

            #update ui
            mongo_read_ui_data()
            reactive_values$ui_data <- ui_data
        }
    })

    #DROPBOX FOLDER INPUT
    observeEvent(input$GetDropbox,{
        print("running input$GetDropbox")

        token <<- ""
        if (!file.exists("drop_token.rds")) {
            print("no token")

            token <<- drop_auth(key = config$drop_key,secret = config$drop_secret,new_user = TRUE,cache = FALSE)
            saveRDS(token, file = "drop_token.rds")
        } else {
            token <<- readRDS("drop_token.rds")
        }

        #check token
        if (token %>% length() == 1) {
            print("no token")

            file.remove("drop_token.rds")
            return()
        }

        #all ok, proceed
        dir_files <- drop_dir(recursive = TRUE,dtoken = token)  #".tag" "name" "path_lower" "path_display" "id" "client_modified" "server_modified" "rev" "size" "content_hash"
        req(dir_files %>% nrow() >0)

        #empty server /dropbox_downloads_temp
        f <- list.files("dropbox_downloads_temp", include.dirs = F, full.names = T, recursive = T)
        file.remove(f)

        #download files from dropbox to /dropbox_downloads_temp (do after file_archive_check!!!)
        dir_files %>%
            split(x = .,f = dir_files$path_display) %>%
            map(function(x) {drop_download(dtoken = token, path = x$path_display,overwrite = TRUE,local_path = "dropbox_downloads_temp")})

        #construct data.frame
        file_selected <- data.frame(row.names = NULL,
                                    name = dir_files$name,
                                    size = dir_files$size,
                                    type = '',
                                    datapath = str_c("dropbox_downloads_temp",dir_files$path_display)
        )

        process_files(file_selected,session)

        #update ui
        mongo_read_ui_data()
        reactive_values$ui_data <- ui_data

    })

    
    # create the ui DataTables, changes with ui_data
    observe({
        req(length(reactive_values$ui_data)>0)
        output$overview <- DT::renderDataTable({
            DT::datatable(
                data = reactive_values$ui_data %>% lapply(function(x) {
                    tibble(x$import_basename,x$import_type,x$import_datetime %>% format("%x %X"))  #x$import_id,
                }) %>% bind_rows() %>% setNames(c("File name","Import type","Import time")),
                options=list(
                    searching=FALSE,
                    lengthChange=FALSE,
                    paging=FALSE,
                    ordering=FALSE,
                    info=FALSE
                ),
                rownames=NULL,
                selection = 'none',
                class = "compact"
            )
        })
    })
    output$file_datatable_ui <- renderUI({
        req(length(reactive_values$ui_data)>0)
        tagList(
            DT::dataTableOutput("overview"),
            shiny::actionButton(
                inputId = "remove_imported_file",
                label = NULL,
                class = "btn-secondary btn-sm pull-right",
                icon = icon("trash",class = "fa-lg",lib = "font-awesome"),
                style = "border-radius:0px;"),
        )
    })


    # export_button tests and processing
    observeEvent(input$init, {
        test1 <- FALSE
        test2 <- FALSE
        test3 <- FALSE

        selected_imports <- reactive_values$ui_data
        test1 <- selected_imports %>% length()>0
        notification_id <<- sendnotification(msg = str_c("selected_imports:",selected_imports %>% as.character(),"(",test1,")",collapse = TRUE),
                                             duration = 4,id = "my_notification11",session = session)
        addClass(id = "shiny-notification-panel", class = "customclass1")

        #check_single_export_type
        if (test1) {
            selected_imports_check <- selected_imports %>% lapply(function(x) {x$export_type}) %>% unique() %>% length() ==1
            test2 <- selected_imports_check
            notification_id <<- sendnotification(msg = str_c("selected_imports_check:",selected_imports_check),
                                                 duration = 2,id = "my_notification12",session = session)
        }

        #match_export_rules()
        if (test2) {
            export_rules_check <- match_export_rules(selected_imports) ==1
            test3 <- export_rules_check
            notification_id <<- sendnotification(msg = str_c("export_rules_check: ",export_rules_check),
                                                 duration = 2,id = "my_notification13",session = session)
        }

        #check options for export_type
        if (test3) {
            my_export_options <- get_export_options(selected_imports)

            if ("FILE" %in% my_export_options) {
                shinyjs::runjs("document.getElementById('export_button').click();")  #expects file path
                return()
            }

        }#end of test3
    })
    output$export_button <- downloadHandler(
        filename = function() {
            paste("my_download", ".xlsx", sep = "")
        },
        content = function(file) {
            selected_imports <- reactive_values$ui_data
            my_output <- run_export_processing(selected_imports=selected_imports,export_mode="FILE")
            file.copy(my_output$filepath,file,overwrite = TRUE)
            #file.remove(my_output$filepath)  #dev

            notification_id <<- sendnotification(msg = str_c("saveWorkbook!!! ",file),
                                                 duration = 5,id = "my_notification15",session = session)
        }
    )
    
    
    # delete imported files
    observeEvent(input$remove_imported_file,{
        unselected_imports <- reactive_values$ui_data
        unselected_imports %>% map(function(x) {x$import_id} %>% mongo_delete(.))
        unselected_imports %>% map(function(x) {x$import_id} %>% sqlite_delete(.))
        
        notification_id <<- sendnotification(msg = str_c("remove_imported_file:",unselected_imports %>% paste(.,collapse = '')),
                                             duration = 2,id = "my_notification21",session = session)
        addClass(id = "shiny-notification-panel", class = "customclass1")
        mongo_read_ui_data()
        
        if (ui_data %>% length() ==0) {
            output[["overview"]] <- NULL  #NULL DT::datatable otherwise get error for data
        }
        reactive_values$ui_data <- ui_data
    })
    


}

shinyApp(ui, server)

