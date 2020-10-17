#pre-open file checks [generic] ----
#rule#1: new name,size,mtime
new_file_check <- function(name,size) {
    mongo_connection <- mongo_connect_local(collection = "myfirstcollection", database = "myfirstdb")
    
    query <- str_c('{ "import_basename" : "',name,'" , "import_size" : ',size,' }')
    test <- mongo_connection$distinct( key = "import_id" , query = query ) %>% length()==0  #pass
    
    mongo_connection$disconnect()
    test  #true/false
}
#rule2: get any info from archive
file_archive_check <- function(name,size,preserve) {
    mongo_archive_connection <- mongo_connect_local(collection = "archive_col", database = "myfirstdb")
    exported <- 0
    
    query <- str_c('{ "import_basename" : "',name,'" , "import_size" : ',size,' }')
    # mongo_archive_connection$distinct( key = "import_date",query = query ) %>% length()==0  #pass
    
    db_result <- mongo_archive_connection$find(query = query,
                          fields = '{"exported":true}',
                          limit = 1,
                          sort = '{"exported": -1}')
    if (db_result %>% nrow() >0) {
        #get exported value from archive
        exported <- db_result %>% pull(exported)
        
        #remove using mongo_archive_connection
        if (!(preserve==TRUE & exported==1)) mongo_archive_connection$remove(query = query,just_one = TRUE)
    }
    
    mongo_archive_connection$disconnect()
    exported  #0/1
}


#get import_group ----
all_tests <- list()
#prepare pre-open identification of export type from first line ----
#rule#1: file_name contains CSTLIST.csv
filename_test <- function(file_name,first_line,param) {
    sum(grepl(param,file_name,ignore.case = TRUE))==1
}
#rule#2: number of columns==6
columnnumber_test <- function(file_name,first_line,param) {
    ncol(first_line)==param
}
#rule#3:check class character_logical_character_character_character_character
class_test <- function(file_name,first_line,param) {
    param <- strsplit(param,split = "_")[[1]]
    if (length(param) != length(first_line)) return(FALSE)
    sum(sapply(first_line, class) == param)==ncol(first_line)
}
#rule#4:no column headers (at least one column header starts with 0-9 or is.na)
nocolumnheader_test <- function(file_name,first_line,param) {
    sum(str_detect(first_line[1,],"^[\\d]+"))>0 || sum(is.na(first_line[1,]))>0
}
#identify and store group with path,name
get_group <- function (path,name) {
    #open file and get first line
    if (str_detect(name,"\\.(CSV|csv)")) {
        first_line <<- read.csv(path, header=FALSE, stringsAsFactors=FALSE,fileEncoding="latin1",nrows=1) %>% as_tibble()
    } else if (str_detect(name,"\\.(XLS|xls)")) {
        first_line <<- read_excel(path,n_max=1)
    } else {
        NULL
    }
    
    #send first line into tests and return vector for each file, check sum and length for group match
    group_status <- 1:length(all_tests) %>% map(function (group_number) {  #loops through groups
        # all_tests[x] %>% call(first_line) %>% eval()
        sum(all_tests[[group_number]] %>% map(function(test_name_param){  #loops through tests
            test_name <- strsplit(test_name_param,split = "\\|")[[1]][1]
            param <- strsplit(test_name_param,split = "\\|")[[1]][2]
            test_name %>% call(name,first_line,param) %>% eval() 
        }) %>% unlist())==length(all_tests[[group_number]])
        
    }) %>% unlist()
    
    #get group where group_status==TRUE
    if ( sum(group_status)>0 ) {all_tests[group_status] %>% names()} else {""}
    
}


#preprocess and get data ----
all_preprocessing <- list()
#prepare import&export type preprocessing for import ----
#preprocess1: get data
get_csv_data <- function(path,data_tbl,param) {
    param <- str_replace(param,"^...","") %>% as.logical()  #get HDR
    data_tbl <<- read.csv(path, header=param, stringsAsFactors=FALSE,fileEncoding="latin1",nrows=Inf) %>% as_tibble()  #fileEncoding or mongo_connection$insert() complains | ,numerals = "no.loss"???
}
get_excel_data <- function(path,data_tbl,param) {
    param <- str_replace(param,"^...","") %>% as.logical()  #get HDR
    data_tbl <<- read_excel(path,col_names=param)  #add column for sheet name, to cater for multiple sheets!!! same columns in each sheet!!!
}
get_paypal_api_data <- function(path,data_tbl,param) {
    token <- readRDS(path)
    
    #transactions_api <- paste0("https://api.sandbox.paypal.com/v1/reporting/transactions?start_date=", start_date,"&end_date=", end_date)  #sandbox
    transactions_api <- paste0("https://api.paypal.com/v1/reporting/transactions?fields=all&start_date=", start_date,"&end_date=", end_date)
    start_date <- "2020-09-01T00:00:00-0700"
    end_date <- "2020-09-29T23:59:59-0700"
    datas <- GET(transactions_api, accept_json(), add_headers('Authorization'= paste0("Bearer ", token)))

    if(datas$status_code == 200){
        result <- fromJSON(content(datas, as = "text"))
        print(paste0('Account Number = ', result$account_number))
        print(paste0('Number of Transactions = ', result$total_items))
        
        #temp_df <- result$transaction_details %>% pull(cart_info)
        result$transaction_details$cart_info$item_details %>% toJSON() %>% prettify()
        level_1 <- result$transaction_details %>% nrow()
        level_2 <- result$transaction_details[1,] %>% names()
        
        temp_df1 <- 1:level_1 %>% map_dfr(function(x) { 
                #level_2 <- result$transaction_details[x,] %>% names()
                #print(str_c("checked ",x," - ",result$transaction_details$transaction_info[x,"paypal_account_id"]))
                level_2 %>% map_dfc(function(y) {
                    if (y == "cart_info") {
                        # "cart_info": {"item_details": [
                        if (result$transaction_details$cart_info$item_details[[x]] %>% as_tibble() %>% nrow() >0) result$transaction_details$cart_info$item_details[[x]] %>% as_tibble()  #multiple items???
                    } else {
                        if (result$transaction_details[x,y] %>% as_tibble() %>% nrow()>0) result$transaction_details[x,y] %>% as_tibble()
                    }
                })
            })
        temp_df2 <- temp_df1 %>% filter(!map_lgl(tax_amounts, is.null)) %>% unnest(tax_amounts) %>% right_join(select(temp_df1,-tax_amounts))  #tax_amounts is a list of 1x2 tibble or null
        temp_df3 <- do.call("cbind", temp_df2) %>% as_tibble()
        data_tbl <<- temp_df3
        
    } else {
        data_tbl <<- tibble()
    }
    
    #saveRDS(object = temp_df,file = "../MASTER_paypal.rds")

}
#preprocess2: rename column names
renamecolumns_preprocess <- function(path,data_tbl,param) {
    if (param=="FILL") {
        names(data_tbl) <<- unlist(map(seq_along(names(data_tbl)),function(x) paste('col_',x,sep='')))
    } else if (param=="REMOVESPACES") {
        names(data_tbl) <<- str_replace_all(names(data_tbl)," ","_")
    }
}
#preprocess3: split out cfm number
splitcfmname_preprocess <- function(path,data_tbl,param) {
    param <- enquo(param)
    data_tbl <<- data_tbl %>% 
        extract(!!param,c('cfm_name'),regex = "(.+?)(?:\\s|-|\\()(?:\\d{3,4}x{0,1})",remove = FALSE)
}
#preprocess4: split out multiple cfm numbers
splitcfmnumbers_preprocess <- function(path,data_tbl,param) {
    param <- enquo(param)
    data_tbl <<- data_tbl %>% 
        extract(!!param, c("cfm_number1","cfm_number2","cfm_number3"), regex = "(?:\\s|-|\\()(\\d{3,4}x{0,1})(?:-|,)?(\\d{3,4}x{0,1})?(?:-|,)?(\\d{3,4}x{0,1})?(?:\\b)", remove = FALSE)
}
#preprocess5: for large files select only required columns!!!
select_columns_preprocess <- function(path,data_tbl,param) {
    param <- strsplit(param,split = "-")[[1]]
    selected_columns <- enquo(param)
    data_tbl <<- data_tbl %>% select(!!selected_columns)
}
##preprocess6: for large files summarise around non-distinct rows!!!
#run preprocessing before adding to db and updating ui
data_preprocessed <- function (path,group) {
    export_type <- strsplit(group,"\\|")[[1]][1]
    import_type <- strsplit(group,"\\|")[[1]][2]
    
    data_tbl <<- tibble()
    
    all_preprocessing[[export_type]][[import_type]] %>% map(function(preprocess_param) {
        preprocess <- strsplit(preprocess_param,split = "\\|")[[1]][1]
        param <- strsplit(preprocess_param,split = "\\|")[[1]][2]
        preprocess %>% call(path,data_tbl,param) %>% eval() 
    })
    
    list(data=data_tbl)
    
    #check selected_columns exist on all records!!!  (valuebox_columns,selected_columns)
    
}


#export rules ----
all_export_rules <- list()
#rule 1
required_input_types <- function(selected_imports,param) {
    #split param
    param <- strsplit(param,split = "\\.")[[1]]
    
    selected_input_types <- selected_imports %>% lapply(function(x) {x$import_type}) %>% unique() %>% unlist()
    
    selected_input_types %>% map(function(x) {if(x %in% param) x}) %>% unlist() %>% length() == length(selected_input_types) && 
        (length(selected_input_types) == length(param)) #matched all specified input_types with no extras
    
    #true/false
}
#rule 2
optional_input_types <- function(selected_imports,param) {
    #split param
    param <- strsplit(param,split = "\\.")[[1]]
    
    selected_input_types <- selected_imports %>% lapply(function(x) {x$import_type}) %>% unique() %>% unlist()
    
    selected_input_types %>% map(function(x) {if(x %in% param) x}) %>% unlist() %>% length() == length(selected_input_types)  #all specified input_types exist
    
    #true/false
}
#rule 3
maximum_import_type_count <- function(selected_imports,param) {
    #frequency count of input_types, sort and check most<param
    selected_imports %>% sapply(function(x) {x$import_type}) %>% table() %>% pluck(1) <= param
    
    #true/false
}
#rule 4
unique_import_type_count <- function(selected_imports,param) {
    selected_input_types <- selected_imports %>% lapply(function(x) {x$import_type}) %>% unique() %>% unlist()
    
    selected_input_types %>% length() <= param
    
    #true/false
}
match_export_rules <- function(selected_imports) {  #list(list(import_id=import_id,export_type=export_type,import_type=import_type),...)
    #get export_type
    export_type <- selected_imports[[1]][["export_type"]]
    
    #progress through each all_export_rules[export_type], must match all ( sum()==length(all_export_rules[[export_type]]) )
    sum( all_export_rules[[export_type]] %>% lapply(function(export_rule) {
        fct <- strsplit(export_rule,split = "\\|")[[1]][1]
        param <- strsplit(export_rule,split = "\\|")[[1]][2]
        fct %>% call(selected_imports,param) %>% eval() 
    }) %>% unlist() )==length(all_export_rules[[export_type]])
    
    #true/false
}


#export requirements----
all_export_options <- list()
get_export_options <- function(selected_imports) {
    #get export_type
    export_type <- selected_imports[[1]][["export_type"]]
    
    all_export_options[[export_type]]  #c('FILE',QBO',...)
}


#export processing ----
all_export_processing <- list()
#add to all_export_processing
#add default export_type settings from user_base!!!
#process and export here
run_export_processing <- function(selected_imports,export_mode="FILE") {  #list(list(import_id=import_id,export_type=export_type,import_type=import_type),...)
    #get export_type
    export_type <- selected_imports[[1]][["export_type"]]
    print(str_c("running with",export_type))
    
    my_output <<- list()  #filepath=""
    #progress through each all_export_rules[export_type], must match all ( sum()==length(all_export_rules[[export_type]]) )
    all_export_processing[[export_type]][[export_mode]] %>% map(function(export_process) {
        fct <- strsplit(export_process,split = "\\|")[[1]][1]
        param <- strsplit(export_process,split = "\\|")[[1]][2]  #NA
        my_output <<- fct %>% call(selected_imports,param) %>% eval() 
    })
    
    my_output
}

