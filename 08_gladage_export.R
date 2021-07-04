all_tests <- all_tests %>% append(list(  #if all tests pass export type=group
    'GLADAGE|CSTLIST'=c('filename_test|CSTLIST.csv','columnnumber_test|6','class_test|character_logical_character_character_character_character','nocolumnheader_test'),
    'GLADAGE|CSTIVDUEDATE'=c('filename_test|CSTIVDUEDATE'),
    'GLADAGE|INVSUM'=c('filename_test|INVSUM'),
    # 'GLADAGE|DISPATCH'=c('filename_test|DISPATCH SUMMARY','columnnumber_test|32'),
    'GLADAGE|DISPATCH'=c('filename_test|DISPATCH DETAIL','columnnumber_test|46'),
    'GLADAGE|MEMBER'=c('filename_test|NAMED MEMBER','columnnumber_test|177')
))

all_preprocessing <- all_preprocessing %>% append(list(
    GLADAGE = list(
        CSTLIST = c('get_csv_data|HDRFALSE','renamecolumns_preprocess|FILL','splitcfmname_preprocess|col_3','splitcfmnumbers_preprocess|col_3'),
        CSTIVDUEDATE = c('get_csv_data|HDRFALSE','renamecolumns_preprocess|FILL'),
        INVSUM = c('get_csv_data|HDRFALSE','renamecolumns_preprocess|FILL'),
        DISPATCH = c('get_excel_data|HDRTRUE','renamecolumns_preprocess|REMOVESPACES','splitcfmnumbers_preprocess|Member'),  #,'select_columns_preprocess|Member-Member_Electoral_Ward'
        MEMBER = c('get_excel_data|HDRTRUE','renamecolumns_preprocess|REMOVESPACES','splitcfmnumbers_preprocess|Name')
    )
))

all_export_rules <- all_export_rules %>% append(list(
    GLADAGE = c('required_input_types|CSTLIST.CSTIVDUEDATE.INVSUM.DISPATCH.MEMBER','maximum_import_type_count|1')
))

all_export_options <- all_export_options %>% append(list(
    GLADAGE = c('FILE')
))

all_export_processing <- all_export_processing %>% append(list(
    GLADAGE = list(
        FILE = c('gladage_export_processing')
    )
))

gladage_export_processing <- function(selected_imports,param) {
    
    CSTLIST <- tibble()
    selected_imports %>% map(function(x) {
        if (x$import_type=="CSTLIST") CSTLIST <<- CSTLIST %>% rbind(
            sqlite_read_data(x$import_id)
        )
    })

    CSTIVDUEDATE <- tibble()
    selected_imports %>% map(function(x) {
        if (x$import_type=="CSTIVDUEDATE") CSTIVDUEDATE <<- CSTIVDUEDATE %>% rbind(
            sqlite_read_data(x$import_id)
        )
    })

    INVSUM <- tibble()
    selected_imports %>% map(function(x) {
        if (x$import_type=="INVSUM") INVSUM <<- INVSUM %>% rbind(
            sqlite_read_data(x$import_id)
        )
    })

    DISPATCH <- tibble()
    selected_imports %>% map(function(x) {
        if (x$import_type=="DISPATCH") DISPATCH <<- DISPATCH %>% rbind(
            sqlite_read_data(x$import_id)
        )
    })

    MEMBER <- tibble()
    selected_imports %>% map(function(x) {
        if (x$import_type=="MEMBER") MEMBER <<- MEMBER %>% rbind(
            sqlite_read_data(x$import_id)
        )
    })

    #dev
    MASTER <- list(
        CSTLIST = tibble(CSTLIST),
        CSTIVDUEDATE = tibble(CSTIVDUEDATE),
        INVSUM = tibble(INVSUM),
        DISPATCH = tibble(DISPATCH),
        MEMBER = tibble(MEMBER)
    )
    #MASTER <- readRDS(file = "../MASTER.rds")
    write_rds(MASTER,"../MASTER.rds")

    
    #GET DATA
    cstlist_dat <- MASTER$CSTLIST
    invsum_dat <- MASTER$INVSUM
    cstivduedate_dat <- MASTER$CSTIVDUEDATE
    member_dat <- MASTER$MEMBER
    dispatch_dat <- MASTER$DISPATCH
    
    #additional preprocessing (already automated)
    # splitcfmnumbers_preprocess <- function(data_tbl,param) {
    #   param <- enquo(param)
    #   data_tbl %>% 
    #     extract(!!param, c("cfm_number1","cfm_number2","cfm_number3"), regex = "(?:\\s|-|\\()(\\d{3,4}x{0,1})(?:-|,)?(\\d{3,4}x{0,1})?(?:-|,)?(\\d{3,4}x{0,1})?(?:\\b)", remove = FALSE)
    # }
    # # DISPATCH = c('get_excel_data|HDRTRUE','renamecolumns_preprocess|REMOVESPACES','splitcfmnumbers_preprocess|Member'),  #,'select_columns_preprocess|Member-Member_Electoral_Ward'
    # # MEMBER = c('get_excel_data|HDRTRUE','renamecolumns_preprocess|REMOVESPACES','splitcfmnumbers_preprocess|Name')
    # dispatch_dat <- dispatch_dat %>% splitcfmnumbers_preprocess(.,"Member")
    # member_dat <- member_dat %>% splitcfmnumbers_preprocess(.,"Name")
    
    
    
    
    #MANIPULATE DATA
    #INVSUM.csv - add cfm_number by matching invsum_dat.col_5 to cstlist_dat.col_name OR use invsum_dat.col_4 to cstlist_dat.col_1
    #join on sage code
    cstlist_dat <- cstlist_dat %>% mutate( col_1 = col_1 %>% as.character() )
    invsum_cstlist <- right_join( invsum_dat , cstlist_dat[,c('col_1','cfm_name','cfm_number1','cfm_number2','cfm_number3')] , by = c("col_4" = "col_1") )
    
    #CSTIVDUEDATE.csv - add cfm_number by matching cstivduedate_dat.col_2 to cstlist_dat.col_name OR use cstivduedate_dat.col_1 to cstlist_dat.col_1, and unique_flag (first row from col_8 only)
    cstivduedate_cstlist <- right_join( cstivduedate_dat , cstlist_dat[,c('col_1','cfm_name','cfm_number1','cfm_number2','cfm_number3')] , by = c("col_1" = "col_1") )
    cstivduedate_cstlist <- distinct(cstivduedate_cstlist, col_8, .keep_all = TRUE)
    
    #added cfm_number1..3 in preprocessing, rename to match previous work below
    member_dat <- member_dat %>% dplyr::rename(. , cfm_number = cfm_number1)
    dispatch_dat <- dispatch_dat %>% dplyr::rename(. , cfm_number = cfm_number1)
    member_dat <- member_dat%>%select(-cfm_number2,-cfm_number3)
    dispatch_dat <- dispatch_dat%>%select(-cfm_number2,-cfm_number3)
    
    
    #REFORMAT DATES/NUMBERS
    str(invsum_cstlist)
    invsum_cstlist$col_3_adj <- strptime(invsum_cstlist[["col_3"]],format="%d/%m/%Y")
    invsum_cstlist$col_3_adj <- as.Date(invsum_cstlist$col_3_adj)
    str(invsum_cstlist)
    
    str(cstivduedate_cstlist)
    cstivduedate_cstlist$col_7_adj <- strptime(cstivduedate_cstlist[["col_7"]],format="%d/%m/%Y")
    cstivduedate_cstlist$col_7_adj <- as.Date(cstivduedate_cstlist$col_7_adj)
    str(cstivduedate_cstlist)
    
    dispatch_dat <- dispatch_dat %>% mutate( 'Dispatch_Date' = as.POSIXct(Dispatch_Date, origin="1970-01-01") %>% as.Date() )
    dispatch_dat <- dispatch_dat %>% mutate( 'Expiry' = as.POSIXct(Expiry, origin="1970-01-01") %>% as.Date() )
    
    member_dat <- member_dat %>% mutate( 'Inactive_Date' = as.POSIXct(Inactive_Date, origin="1970-01-01") %>% as.Date() )
    member_dat <- member_dat %>% mutate( 'Last_Spot_Check_Date' = as.POSIXct(Last_Spot_Check_Date, origin="1970-01-01") %>% as.Date() )
    
    #remove defra
    dispatch_dat <- dispatch_dat %>% filter(str_detect(Supplier,"DEFRA - Covid 19 Response",negate = TRUE) & str_detect(Supplier,"DEFRA - Covid 19 Response Phase 2",negate = TRUE))
    
    #dev
    # temp1 <- cstlist_dat %>% select(col_3,cfm_number1,cfm_number2,cfm_number3)
    # temp2 <- member_dat %>% select(Name,cfm_number)
    # temp2 <- temp2 %>% drop_na(cfm_number)
    # right_join(temp1 %>% drop_na(cfm_number1) , temp2 , by=c("cfm_number1"="cfm_number"))
    # right_join(temp1 %>% drop_na(cfm_number2) , temp2 , by=c("cfm_number2"="cfm_number"))
    # right_join(temp1 %>% drop_na(cfm_number3) , temp2 , by=c("cfm_number3"="cfm_number"))
    
    
    #CREATE OUTPUT
    my_df <- 
        right_join(cstlist_dat%>%drop_na(cfm_number1) , member_dat , by=c("cfm_number1"="cfm_number")) %>%  #,"cfm_number2"="cfm_number","cfm_number3"="cfm_number"
        select('cfm_number1','col_1','Name') %>%
        dplyr::rename(. , 
                      cfm_number = cfm_number1,
                      sage_number = col_1
        ) %>%
        drop_na(cfm_number)
    str(my_df)
    summary(my_df)
    
    #CLEANSE = check if any cstlist_dat have cfm_number2 or cfm_number3
    cstlist_dat[!is.na(cstlist_dat$cfm_number2),] %>% nrow()  #0
    cstlist_dat[!is.na(cstlist_dat$cfm_number3),] %>% nrow()  #0
    
    #CLEANSE - REMOVE missing cfm_number in member_dat
    member_dat[is.na(member_dat$cfm_number),] %>% nrow()
    MISSING_CFMNUMBER_IN_MEMBER_DAT <- member_dat[is.na(member_dat$cfm_number),c("Name")]
    member_dat <- member_dat %>% drop_na(cfm_number)
    
    #CLEANSE - REMOVE duplicate cfm_number in member_dat
    n_occur <- data.frame(table(member_dat$cfm_number))
    duplicates <- n_occur[n_occur$Freq > 1,]
    DUPLICATE_CFMNUMBER_IN_MEMBER_DAT <- right_join( member_dat , duplicates , by = c("cfm_number" = "Var1") ) %>% select(cfm_number,Name)
    member_dat <- distinct( member_dat , cfm_number , .keep_all = TRUE )
    
    #CLEANSE - get duplicate sage_number in my_df
    n_occur <- data.frame(table(my_df$sage_number))
    duplicates <- n_occur[n_occur$Freq > 1,]
    DUPLICATE_SAGE_NUMBER_IN_FINAL <- right_join( my_df , duplicates , by = c("sage_number" = "Var1") )
    
    #CLEANSE - REMOVE duplicate cfm_number in my_df
    n_occur <- data.frame(table(my_df$cfm_number))
    duplicates <- n_occur[n_occur$Freq > 1,]
    DUPLICATE_CFMNUMBER_IN_FINAL <- right_join( my_df , duplicates , by = c("cfm_number" = "Var1") )
    my_df <- distinct( my_df , cfm_number , .keep_all = TRUE )
    
    #CLEANSE - REMOVE duplicate cfm_number1/col_1 in cstlist_dat
    n_occur <- data.frame(table(cstlist_dat$cfm_number1))
    duplicates <- n_occur[n_occur$Freq > 1,]
    DUPLICATE_CFMNUMBER_IN_CSTLIST <- right_join( cstlist_dat[c('cfm_number1','cfm_name','col_1')] , duplicates , by = c("cfm_number1" = "Var1") )
    cstlist_dat <- distinct( cstlist_dat , cfm_number1 , .keep_all = TRUE )
    
    #CLEANSE - REMOVE na cfm_number in cstlist_dat
    cstlist_dat[is.na(cstlist_dat$cfm_number1),c('cfm_number1','cfm_name','col_1')]
    cstlist_dat <- cstlist_dat[!is.na(cstlist_dat$cfm_number1),]
    
    #CLEANSE - missing on sage in my_df
    my_df[is.na(my_df$sage_number),] %>% nrow()
    #my_df[is.na(my_df$cfm_number),] %>% nrow()
    
    
    
    #[temp]cstivduedate_cstlist <- right_join( cstivduedate_dat , cstlist_dat[,c('col_1','cfm_name','cfm_number1','cfm_number2','cfm_number3')] , by = c("col_1" = "col_1") )
    
    #GET NEW COLUMNS
    #Location Dispatch Detail.xlsx
    dispatch_dat_sample <- dispatch_dat %>% 
        select("Member","cfm_number","Department","Dispatch_Date","Weight_Kg","Expiry") #%>% sample_n(1000), removed 'Expiry'
    #mutate("Weight_Kg" = Weight_Tonne*1016.05)
    
    #>last dispatch
    by_cfm_number <- dispatch_dat_sample %>% 
        group_by(cfm_number) %>%
        dplyr::summarise(.groups="keep",
                         sum_Weight_Kg = sum(Weight_Kg),
                         max_Dispatch_Date = max(Dispatch_Date),
                         count_dispatch = n_distinct(Dispatch_Date)
        ) %>%
        ungroup() %>%
        arrange(desc(count_dispatch))  #dev
    #>last invoice
    invsum_cstlist_summary <- invsum_cstlist %>% 
        filter(col_2 == "Product Invoice") %>%
        group_by(col_4) %>%
        dplyr::summarise(.groups="keep",
                         max_invoice = as.POSIXct(max(`col_3_adj`)),
                         cfm_number1 = first(cfm_number1)
        ) %>%
        ungroup()
    invsum_cstlist_summary <- semi_join(invsum_cstlist_summary,my_df["sage_number"],by=c("col_4"="sage_number"))  #return all rows from x where there are matching values in y, keeping just columns from x.
    invsum_cstlist_summary[is.na(invsum_cstlist_summary$cfm_number1),] %>% nrow()
    #>kg rec'd since invoice
    kg_recd_since_invoice <- right_join( invsum_cstlist_summary[,c("col_4","max_invoice","cfm_number1")] , dispatch_dat_sample , by=c('cfm_number1'='cfm_number')  ) %>%
        filter(`Dispatch_Date`>max_invoice) %>%
        group_by(cfm_number1) %>%  #try col_4
        dplyr::summarise(.groups="keep",
                         kg_recd_since_invoice = sum(`Weight_Kg`),
                         count_dispatch = n_distinct(`Dispatch_Date`),
                         count_cfm_number1 = n()
        ) %>%
        ungroup() %>%
        arrange(desc(count_cfm_number1))  #dev
    #>kg rec'd in last cal'r mth
    som <- function(x,p) {
        if (p=="last") return ((as.Date(format(x, "%Y-%m-01"))-1) %>% format(., "%Y-%m-01") %>% as.Date())  #1st of previous month
        if (p=="this") return ((as.Date(format(x, "%Y-%m-01"))-1) %>% as.Date())  #end of previous month
        # if (p=="last") return (as.Date(format(x, "2021-04-01")))  #hardcoded date
        # if (p=="this") return (as.Date(format(x, "2021-04-30")))  #hardcoded date
    }
    kg_recd_in_last_calr_mth <- dispatch_dat_sample %>%
        mutate(
            #last_cal_month_flag = case_when( format(`Dispatch_Date`,"%Y-%m") == format(som(Sys.Date())-1,"%Y-%m") ~ TRUE ),
            last_cal_month_flag = case_when( difftime( `Dispatch_Date` , som(Sys.Date(),"this") , units="days") <=0 &
                                             difftime( som(Sys.Date(),"last") , `Dispatch_Date` , units="days") <=0 ~ TRUE ),
            
            month_col = format(`Dispatch_Date`,"%Y-%m")
        ) %>%
        filter(last_cal_month_flag == TRUE) %>%
        group_by(cfm_number) %>%
        dplyr::summarise(.groups="keep",
                         kg_recd_in_last_calr_mth = sum(`Weight_Kg`)
        ) %>%
        ungroup()
    #>invoices £ o/s <=30 days
    #>invoices £ o/s >30 days <=60 days
    #>invoices £ o/s >60 days
    inv_os_last_x_days <- cstivduedate_cstlist %>%
        mutate(
            upto_30_days_flag = case_when( difftime(Sys.Date() , `col_7_adj` , units="days") <= (30) ~ TRUE ),
            # upto_60_days_flag = case_when( difftime(Sys.Date() , `col_7_adj` , units="days") < (360) ~ TRUE ),
            # morethan_60_days_flag = case_when( difftime(Sys.Date() , `col_7_adj` , units="days") > (360) ~ TRUE ),
            morethan_30_days_flag = case_when( difftime(Sys.Date() , `col_7_adj` , units="days") > (30) ~ TRUE ),
            last_x_days = difftime(Sys.Date() , `col_7_adj` , units="days")
        ) %>% 
        # filter(col_1=='SHA001') %>%
        group_by(col_1) %>%
        dplyr::summarise(.groups="keep",
                         inv_os_upto_30_days = sum(col_11[!is.na(upto_30_days_flag)]),
                         # inv_os_between_30_60_days = sum(col_11[!is.na(upto_60_days_flag) & is.na(upto_30_days_flag)]),
                         # inv_os_morethan_60_days = sum(col_11[!is.na(morethan_60_days_flag)]),
                         inv_os_morethan_30_days = sum(col_11[!is.na(morethan_30_days_flag)]),
                         inv_count = n(),
                         min_x_days = min(last_x_days),
                         max_x_days = max(last_x_days),
                         total = sum(col_11)
        ) %>%
        ungroup() %>%
        arrange(desc(inv_count))  #dev
    ## alloc. >=1/4/19
    #Kg rec  >=1/4/19
    #Kg rec excl. fruit and veg (A) >=1/4/19
    #Short dated Kg rec since 1/4/19 (<=2 days)
    since_0104 <- dispatch_dat_sample %>%
        mutate(
            since_0104_flag = case_when( difftime(strptime("01.04.2021", format = "%d.%m.%Y") , `Dispatch_Date` , units="days")<=0 ~ TRUE ),
            short_dated_flag = case_when( difftime(`Expiry` , `Dispatch_Date` , units="days")<=2 ~ TRUE ),
            excl_fruitveg_flag = case_when( Department!="Fruit" & Department!="Vegetables" ~ TRUE, TRUE ~ NA )
        ) %>%
        group_by(cfm_number) %>%
        dplyr::summarise(.groups="keep",
                         kg_recd_since_0104 = sum(`Weight_Kg`[!is.na(since_0104_flag)]),
                         count_dispatch_date_since_0104 = n_distinct(`Dispatch_Date`[!is.na(since_0104_flag)]),
                         shortdated_kg_recd_since_0104 = sum(`Weight_Kg`[!is.na(short_dated_flag) & !is.na(since_0104_flag)]),
                         excl_fruitveg_kg_recd_since_0104 = sum(`Weight_Kg`[!is.na(excl_fruitveg_flag) & !is.na(since_0104_flag)]),
                         only_fruitveg_kg_recd_since_0104 = sum(`Weight_Kg`[is.na(excl_fruitveg_flag) & !is.na(since_0104_flag)]),
        ) %>%
        ungroup()
    #Avg. kg rec (A) per allocation >=1/4/19
    since_0104 <- since_0104 %>% 
        mutate(
            avg_kg_recd = kg_recd_since_0104 / count_dispatch_date_since_0104
        )
    ## alloc. per year
    #Kg per allocation
    #Fee per fee period
    #Fee_Period
    #Last_Spot_Check_Date
    #Status
    #Inactive_Date
    my_df_simple_cols <- 
        left_join(my_df , member_dat[,!names(member_dat) %in% c("Name")] , by=c("cfm_number"="cfm_number")) %>%
        select('cfm_number','sage_number','Name',
               'Service_Profile_Food_Spend','Max_Weight_Per_Allocation','Fee','Fee_Period','Last_Spot_Check_Date','Status','Inactive_Date','Allocation_Contact_Email'
        ) %>%
        dplyr::rename(
            '# alloc. per year'='Service_Profile_Food_Spend',
            'Kg per allocation'='Max_Weight_Per_Allocation',
            "Fee per fee period"='Fee'
        )
    #kg_per_year
    my_df_simple_cols$'# alloc. per year' <- as.numeric(my_df_simple_cols$'# alloc. per year')
    my_df_simple_cols[is.na(my_df_simple_cols$'# alloc. per year') , '# alloc. per year'] <- 0
    my_df_simple_cols[is.na(my_df_simple_cols$'Kg per allocation') , 'Kg per allocation'] <- 0
    my_df_simple_cols <- my_df_simple_cols %>% 
        mutate(
            kg_per_year = `# alloc. per year` * `Kg per allocation`
        )
    #Next credit review date - check exists on CSTLIST import!!!
    # my_df_other_simple_cols <-
    #   left_join(my_df["sage_number"] , cstlist_dat[,c("col_1","col_7")] , by=c("sage_number"="col_1")) %>%
    #   dplyr::rename(
    #     'Next credit review date'=col_7  #test!!!
    #   )
    # my_df_other_simple_cols <- my_df_other_simple_cols[!is.na(my_df_other_simple_cols$sage_number),]
    
    
    #join new columns to my_df (by=c("this col name is used"="")
    #str(my_df_final)
    str(my_df)
    my_df_final <- data.frame()
    
    str(by_cfm_number)
    my_df_final <- right_join( by_cfm_number[,c('cfm_number','max_Dispatch_Date')] , my_df[,c('cfm_number','sage_number','Name')] , by = c("cfm_number" = "cfm_number") )
    
    str(invsum_cstlist_summary)
    my_df_final <- right_join( invsum_cstlist_summary[,c('cfm_number1','max_invoice')] , my_df_final , by = c("cfm_number1" = "cfm_number") )
    
    str(kg_recd_since_invoice)
    my_df_final <- right_join( kg_recd_since_invoice[,c('cfm_number1','kg_recd_since_invoice')] , my_df_final , by = c("cfm_number1" = "cfm_number1") )
    
    str(kg_recd_in_last_calr_mth)
    my_df_final <- right_join( kg_recd_in_last_calr_mth[,c('cfm_number','kg_recd_in_last_calr_mth')] , my_df_final , by = c("cfm_number" = "cfm_number1") )
    
    str(inv_os_last_x_days)
    my_df_final <- right_join( inv_os_last_x_days[,c('col_1','inv_os_upto_30_days','inv_os_morethan_30_days')] , my_df_final , by = c("col_1" = "sage_number") )  #'inv_os_between_30_60_days',
    
    str(since_0104)
    my_df_final <- right_join( since_0104[,c('cfm_number','kg_recd_since_0104','count_dispatch_date_since_0104','excl_fruitveg_kg_recd_since_0104','only_fruitveg_kg_recd_since_0104','avg_kg_recd','shortdated_kg_recd_since_0104')] , my_df_final , by = c("cfm_number" = "cfm_number") )
    
    str(my_df_simple_cols)
    my_df_final <- right_join( my_df_simple_cols[,!names(my_df_simple_cols) %in% c('sage_number','Name')] , my_df_final , by = c("cfm_number" = "cfm_number") )
    
    # str(my_df_other_simple_cols)
    # my_df_final <- right_join( my_df_other_simple_cols[,c('sage_number','Next credit review date')] , my_df_final , by = c("sage_number" = "col_1") )
    
    my_df_final <- my_df_final %>% 
        dplyr::rename('sage_number'='col_1') %>%
        # select(
        #   "cfm_number","sage_number","Name",
        #   "max_Dispatch_Date","max_invoice","kg_recd_since_invoice","kg_recd_in_last_calr_mth","inv_os_upto_30_days","inv_os_between_30_60_days","inv_os_morethan_60_days",
        #   "# alloc. per year","Kg per allocation","kg_per_year","Fee per fee period","Fee_Period",
        #   "count_dispatch_date_since_0104","kg_recd_since_0104","excl_fruitveg_kg_recd_since_0104","only_fruitveg_kg_recd_since_0104","avg_kg_recd","shortdated_kg_recd_since_0104",
        #   "Last_Spot_Check_Date","Status","Inactive_Date"  #,"Next credit review date"
        # )
        select(
            "cfm_number","sage_number","Name",
            "max_Dispatch_Date","max_invoice","kg_recd_since_invoice","kg_recd_in_last_calr_mth","inv_os_upto_30_days","inv_os_morethan_30_days",
            "# alloc. per year","Kg per allocation","kg_per_year","Fee per fee period","Fee_Period",
            "kg_recd_since_0104","only_fruitveg_kg_recd_since_0104","shortdated_kg_recd_since_0104",
            "Last_Spot_Check_Date","Status","Inactive_Date","Allocation_Contact_Email"
        )
    
    
    #Phil's desired output - match!!!
    # Sage Ref
    # Member BName
    # Last Duispatch date
    # Last Invoice
    # KG Recvd since last invoice
    # KG last calendar month
    # Annual Allocation
    # KG/alloc
    # Kg/year
    # KG since 1/4  With parameters set to 25% and 40% (rather than 50%)
    # F&V recvd[ADDED since 1/4]
    # Short dated[HOLD]
    # Last Spot check
    # Status – active/inactive etc
    # Inactive date
    # Sage renewal date
    
    #old gladage list:
    # cfm_number
    # sage
    # Member
    # last dispatch
    # last invoice
    # kg rec'd since invoice
    # kg rec'd in last cal'r mth
    # invoices £ o/s <=30 days[]
    # invoices £ o/s >30 days <=60 days[]
    # invoices £ o/s >60 days[]
    # # alloc. per year
    # Kg per allocation
    # Kg per year
    # Fee per 'Fee period'[]
    # Fee period[]
    # # alloc. >=1/4/19[]
    # Kg rec  >=1/4/19
    # Kg rec excl. fruit and veg (A) >=1/4/19[]
    # Avg. kg rec (A) per allocation >=1/4/19[]
    # Short dated Kg rec since 1/4/19 (<=2 days)[HOLD]
    # Last Spot Check Date
    # Schedule info (1=Monday etc.)[]
    # Schedule info (Kg)[]
    # Next credit review date
    # Status
    # Inactive Date
    
    
    
    
    str(my_df_final)
    summary(my_df_final)
    #View(my_df_final)
    
    #CHECK OUTPUT
    my_df_final %>%
        dplyr::summarise(
            cfm_number_isna_count = sum(is.na(cfm_number)),
            cfm_number_distinct_count = n_distinct(cfm_number),
            sage_isna_count = sum(is.na(sage_number)),
            sage_distinct_count = n_distinct(sage_number),
            total_n = n()
        )
    n_occur <- data.frame(table(my_df_final$cfm_number))
    duplicates <- n_occur[n_occur$Freq > 1,]
    duplicates_full <- right_join( my_df_final , duplicates , by = c("cfm_number" = "Var1") ) %>% select(sage_number,Name,Freq,kg_per_year)
    duplicates_full %>% nrow()
    duplicates_full
    my_df_final[!is.na(my_df_final$max_invoice),] %>% nrow()
    
    
    #issues:
    #max_invoice time: 1am???
    
    
    original_names=names(my_df_final)
    names(my_df_final) <- c("CFM","Sage","Member","Last dispatch","Last invoice","Kg rec'd since invoice","Kg rec'd in last cal'r mth","inv o/s <= 30 days","inv o/s > 30 days","# alloc. per year","Kg per allocation","Kg per year","Fee per fee period","Fee period","Kg rec  >=0104","F&V rec'd since 0104","Short dated Kg recd since 0104","Last Spot Check Date","Status","Inactive Date","Allocation Contact Email")
    
    #EXCEL OUTPUT
    wb <- createWorkbook()
    addWorksheet(wb, "R_gladage")
    writeDataTable(wb,1,my_df_final,colNames = TRUE,tableStyle = "none",withFilter = TRUE,headerStyle = NULL)
    
    #add a row???
    # newrow <- rep("",ncol(my_df_final))
    # classes <- lapply(my_df_final[1,],class)
    # class(newrow) <- classes
    # names(newrow) <- names(my_df_final)
    # bind_rows( my_df_final, newrow )
    # 
    # temp <- my_df_final[1,]
    # 
    # classes <- map(my_df_final[1,],class())
    # names(temp) <- names(my_df_final)
    # rbind( my_df_final,temp  )  #add 1 empty row
    
    #get col number from name
    which_col <- function(my_name,all_names=original_names) {
        which(all_names==my_name)
    }
    which_col("Name")
    
    #set up styles  [https://rdrr.io/cran/openxlsx/man/createStyle.html]
    h1 <- createStyle(fgFill = "#4F81BD", halign = "LEFT", textDecoration = "Bold", border = "Bottom", fontColour = "white", wrapText = TRUE)
    p_normal <- createStyle(numFmt = "GENERAL", fontSize="9", border=c("bottom"), borderStyle="thin", borderColour="#000000")
    p_date <- createStyle(numFmt = "dd/mm/yyyy", fontSize="9", border=c("bottom"), borderStyle="thin", borderColour="#000000")
    p_ndecimal <- createStyle(numFmt = "#,##0.00", fontSize="9", border=c("bottom"), borderStyle="thin", borderColour="#000000")
    p_ncomma <- createStyle(numFmt = "#,##0", fontSize="9", border=c("bottom"), borderStyle="thin", borderColour="#000000")
    p_ncommadecimal <- createStyle(numFmt = "#,##0.00", fontSize="9", border=c("bottom"), borderStyle="thin", borderColour="#000000")
    p_alarm <- createStyle(bgFill = "#ff1212", textDecoration = "Bold", fontColour = "black", fontSize="9", border=c("bottom"), borderStyle="thin", borderColour="#000000")
    p_warn <- createStyle(bgFill = "#ffbc12", textDecoration = "Bold", fontColour = "black", fontSize="9", border=c("bottom"), borderStyle="thin", borderColour="#000000")
    
    #allocate styles to wb
    addStyle(wb, 1, style = p_normal, rows = 1:nrow(my_df_final), cols=1:ncol(my_df_final), gridExpand = TRUE)
    
    addStyle(wb, 1, style = p_date, rows = 1:nrow(my_df_final), cols=which_col("max_Dispatch_Date"), gridExpand = TRUE)
    addStyle(wb, 1, style = p_date, rows = 1:nrow(my_df_final), cols=which_col("max_invoice"), gridExpand = TRUE)
    addStyle(wb, 1, style = p_date, rows = 1:nrow(my_df_final), cols=which_col("Last_Spot_Check_Date"), gridExpand = TRUE)
    addStyle(wb, 1, style = p_date, rows = 1:nrow(my_df_final), cols=which_col("Inactive_Date"), gridExpand = TRUE)  
    # addStyle(wb, 1, style = p_ndecimal, rows = 1:nrow(my_df_final), cols=which_col("inv_os_upto_30_days"), gridExpand = TRUE)
    # addStyle(wb, 1, style = p_ndecimal, rows = 1:nrow(my_df_final), cols=which_col("inv_os_between_30_60_days"), gridExpand = TRUE)
    # addStyle(wb, 1, style = p_ndecimal, rows = 1:nrow(my_df_final), cols=which_col("inv_os_morethan_60_days"), gridExpand = TRUE)
    # addStyle(wb, 1, style = p_ndecimal, rows = 1:nrow(my_df_final), cols=which_col("Fee per fee period"), gridExpand = TRUE)
    addStyle(wb, 1, style = p_ncomma, rows = 1:nrow(my_df_final), cols=which_col("kg_recd_since_invoice"), gridExpand = TRUE)
    addStyle(wb, 1, style = p_ncomma, rows = 1:nrow(my_df_final), cols=which_col("kg_recd_in_last_calr_mth"), gridExpand = TRUE)
    addStyle(wb, 1, style = p_ncommadecimal, rows = 1:nrow(my_df_final), cols=which_col("inv_os_upto_30_days"), gridExpand = TRUE)
    addStyle(wb, 1, style = p_ncommadecimal, rows = 1:nrow(my_df_final), cols=which_col("inv_os_morethan_30_days"), gridExpand = TRUE)
    addStyle(wb, 1, style = p_ncomma, rows = 1:nrow(my_df_final), cols=which_col("# alloc. per year"), gridExpand = TRUE)
    addStyle(wb, 1, style = p_ncomma, rows = 1:nrow(my_df_final), cols=which_col("Kg per allocation"), gridExpand = TRUE)
    addStyle(wb, 1, style = p_ncomma, rows = 1:nrow(my_df_final), cols=which_col("kg_per_year"), gridExpand = TRUE)
    # addStyle(wb, 1, style = p_ncomma, rows = 1:nrow(my_df_final), cols=which_col("count_dispatch_date_since_0104"), gridExpand = TRUE)
    addStyle(wb, 1, style = p_ncomma, rows = 1:nrow(my_df_final), cols=which_col("kg_recd_since_0104"), gridExpand = TRUE)
    # addStyle(wb, 1, style = p_ncomma, rows = 1:nrow(my_df_final), cols=which_col("excl_fruitveg_kg_recd_since_0104"), gridExpand = TRUE)
    addStyle(wb, 1, style = p_ncomma, rows = 1:nrow(my_df_final), cols=which_col("only_fruitveg_kg_recd_since_0104"), gridExpand = TRUE)
    # addStyle(wb, 1, style = p_ncomma, rows = 1:nrow(my_df_final), cols=which_col("avg_kg_recd"), gridExpand = TRUE)
    addStyle(wb, 1, style = p_ncomma, rows = 1:nrow(my_df_final), cols=which_col("shortdated_kg_recd_since_0104"), gridExpand = TRUE)
    
    addStyle(wb, 1, style = h1, rows = 1, cols = 1:ncol(my_df_final), gridExpand = TRUE)
    
    setColWidths(wb, 1, cols = which_col("Name"), widths = 46)  
    setColWidths(wb, 1, cols = which_col("Allocation_Contact_Email"), widths = 46)
    
    #create conditional formatting in wb
    conditionalFormatting(wb, 1, rows=1:nrow(my_df_final), cols=which_col("max_invoice"), rule="=AND(D1>1,E1>1,VALUE(E1)-VALUE(D1)>0)", style = p_warn)
    conditionalFormatting(wb, 1, rows=1:nrow(my_df_final), cols=which_col("Last_Spot_Check_Date"), rule="=AND(VALUE(R1)>0,VALUE(P1)<DATE(YEAR(TODAY()),MONTH(TODAY())-11,1))", style = p_warn)
    conditionalFormatting(wb, 1, rows=1:nrow(my_df_final), cols=which_col("kg_recd_since_0104"), rule="=OR(O1>((((TODAY()-DATE(2021,4,1))/365)*L1)*1.25),O1<((((TODAY()-DATE(2021,4,1))/365)*L1)*0.75))", style = p_warn)
    conditionalFormatting(wb, 1, rows=1:nrow(my_df_final), cols=which_col("sage_number"), rule="=ISBLANK(B1)", style = p_warn)
    conditionalFormatting(wb, 1, rows=1:nrow(my_df_final), cols=which_col("# alloc. per year"), rule="=AND(J1=\"\",ROW(J1)<=COUNTA(A:A))", style = p_warn)
    # conditionalFormatting(wb, 1, rows=1:nrow(my_df_final), cols=which_col("avg_kg_recd"), rule="=OR(VALUE(S1)>(VALUE(L1)*1.25),VALUE(S1)<(VALUE(L1)*0.75))", style = p_warn)
    # conditionalFormatting(wb, 1, rows=1:nrow(my_df_final), cols=which_col("inv_os_between_30_60_days"), rule="=VALUE(I1)>0", style = p_warn)
    
    conditionalFormatting(wb, 1, rows=1:nrow(my_df_final), cols=which_col("max_Dispatch_Date"), rule="=AND(D1>1,E1>1,VALUE(D1)-VALUE(E1)>60)", style = p_alarm)
    conditionalFormatting(wb, 1, rows=1:nrow(my_df_final), cols=which_col("kg_recd_since_0104"), rule="=OR(O1>((((TODAY()-DATE(2021,4,1))/365)*L1)*1.4),O1<((((TODAY()-DATE(2021,4,1))/365)*L1)*0.6))", style = p_alarm)
    # conditionalFormatting(wb, 1, rows=1:nrow(my_df_final), cols=which_col("avg_kg_recd"), rule="=OR(VALUE(S1)>(VALUE(L1)*1.5),VALUE(S1)<(VALUE(L1)*0.5))", style = p_alarm)
    conditionalFormatting(wb, 1, rows=1:nrow(my_df_final), cols=which_col("inv_os_morethan_30_days"), rule="=VALUE(I1)>0", style = p_warn)
    conditionalFormatting(wb, 1, rows=1:nrow(my_df_final), cols=which_col("max_invoice"), rule="=AND(D1>1,E1>1,VALUE(E1)-VALUE(D1)>60)", style = p_alarm)
    #"Kg rec  >=0104","F&V rec'd since 0104","Short dated Kg recd since 0104","Last Spot Check Date","Status","Inactive Date")
    
    
    #audit checks
    # MISSING_CFMNUMBER_IN_MEMBER_DAT (removed)
    # DUPLICATE_CFMNUMBER_IN_MEMBER_DAT (removed)
    # DUPLICATE_SAGE_NUMBER_IN_FINAL
    # DUPLICATE_CFMNUMBER_IN_FINAL
    # DUPLICATE_CFMNUMBER_IN_CSTLIST (removed)
    h2 <- createStyle(halign = "LEFT", textDecoration = "Bold", wrapText = FALSE)
    addWorksheet(wb, "gladage_checks")
    starting_row <- 1
    writeData(wb,2,c("MISSING_CFMNUMBER_IN_MEMBER_DAT"),colNames = TRUE,withFilter = FALSE,headerStyle = NULL,startRow = starting_row)
    addStyle(wb, 2, style = h2, rows = starting_row, cols = 1, gridExpand = TRUE)
    writeData(wb,2,MISSING_CFMNUMBER_IN_MEMBER_DAT,colNames = TRUE,withFilter = FALSE,headerStyle = NULL,startRow = starting_row+1)
    
    starting_row <- starting_row + MISSING_CFMNUMBER_IN_MEMBER_DAT %>% nrow() +3
    writeData(wb,2,c("DUPLICATE_CFMNUMBER_IN_MEMBER_DAT"),colNames = TRUE,withFilter = FALSE,headerStyle = NULL,startRow = starting_row)
    addStyle(wb, 2, style = h2, rows = starting_row, cols = 1, gridExpand = TRUE)
    writeData(wb,2,DUPLICATE_CFMNUMBER_IN_MEMBER_DAT,colNames = TRUE,withFilter = FALSE,headerStyle = NULL,startRow = starting_row+1)
    
    starting_row <- starting_row + DUPLICATE_CFMNUMBER_IN_MEMBER_DAT %>% nrow() +3
    writeData(wb,2,c("DUPLICATE_SAGE_NUMBER_IN_FINAL"),colNames = TRUE,withFilter = FALSE,headerStyle = NULL,startRow = starting_row)
    addStyle(wb, 2, style = h2, rows = starting_row, cols = 1, gridExpand = TRUE)
    writeData(wb,2,DUPLICATE_SAGE_NUMBER_IN_FINAL,colNames = TRUE,withFilter = FALSE,headerStyle = NULL,startRow = starting_row+1)
    
    starting_row <- starting_row + DUPLICATE_SAGE_NUMBER_IN_FINAL %>% nrow() +3
    writeData(wb,2,c("DUPLICATE_CFMNUMBER_IN_FINAL"),colNames = TRUE,withFilter = FALSE,headerStyle = NULL,startRow = starting_row)
    addStyle(wb, 2, style = h2, rows = starting_row, cols = 1, gridExpand = TRUE)
    writeData(wb,2,DUPLICATE_CFMNUMBER_IN_FINAL,colNames = TRUE,withFilter = FALSE,headerStyle = NULL,startRow = starting_row+1)
    
    starting_row <- starting_row + DUPLICATE_CFMNUMBER_IN_FINAL %>% nrow() +3
    writeData(wb,2,c("DUPLICATE_CFMNUMBER_IN_CSTLIST"),colNames = TRUE,withFilter = FALSE,headerStyle = NULL,startRow = starting_row)
    addStyle(wb, 2, style = h2, rows = starting_row, cols = 1, gridExpand = TRUE)
    writeData(wb,2,DUPLICATE_CFMNUMBER_IN_CSTLIST,colNames = TRUE,withFilter = FALSE,headerStyle = NULL,startRow = starting_row+1)
    
    setColWidths(wb, 2, cols = 1:4, widths = 46)
    
    
    
    #write.csv(CSTLIST, "export_temp/MASTER.csv", row.names = FALSE)
    saveWorkbook(wb, "export_temp/temp.xlsx", overwrite = TRUE)
    list(filepath="export_temp/temp.xlsx")
}
