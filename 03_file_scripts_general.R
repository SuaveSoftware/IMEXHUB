#pre-open file checks [generic] ----
#rule#1: new name,size,mtime
new_file_check <- function(name,size) {
    mongo_connection <- mongo_connect(collection = "imexhub_collection", database = "imexhub_database")
    
    query <- str_c('{ "import_basename" : "',name,'" , "import_size" : ',size,' }')
    test <- mongo_connection$distinct( key = "import_id" , query = query ) %>% length()==0  #pass
    
    mongo_connection$disconnect()
    test  #true/false
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
        extract(!!param,c('cfm_name'),regex = "(.+?)(?:\\s|-|\\()(?:\\d{3,4}[xX]{0,1})",remove = FALSE)
}
#preprocess4: split out multiple cfm numbers
splitcfmnumbers_preprocess <- function(path,data_tbl,param) {
    param <- enquo(param)
    data_tbl <<- data_tbl %>% 
        extract(!!param, c("cfm_number1","cfm_number2","cfm_number3"), regex = "(?:\\s|-|\\()(\\d{3,4}[xX]{0,1})(?:-|,)?(\\d{3,4}[xX]{0,1})?(?:-|,)?(\\d{3,4}[xX]{0,1})?(?:\\.|\\b)", remove = FALSE)
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
    export_type <- strsplit(selected_imports[[1]][["import_group"]],"\\|")[[1]][1]

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
    export_type <- strsplit(selected_imports[[1]][["import_group"]],"\\|")[[1]][1]
    
    all_export_options[[export_type]]  #c('FILE',QBO',...)
}


#export processing ----
all_export_processing <- list()
run_export_processing <- function(selected_imports,export_mode="FILE") {  #list(list(import_id=import_id,export_type=export_type,import_type=import_type),...)
    #get export_type
    export_type <- strsplit(selected_imports[[1]][["import_group"]],"\\|")[[1]][1]
    print(str_c("running with ",export_type))
    
    my_output <<- list()  #filepath=""
    #progress through each all_export_rules[export_type], must match all ( sum()==length(all_export_rules[[export_type]]) )
    all_export_processing[[export_type]][[export_mode]] %>% map(function(export_process) {
        fct <- strsplit(export_process,split = "\\|")[[1]][1]
        param <- strsplit(export_process,split = "\\|")[[1]][2]  #NA
        my_output <<- fct %>% call(selected_imports,param) %>% eval() 
    })
    
    my_output
}

