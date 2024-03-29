all_tests <- all_tests %>% append(list(  #if all tests pass export type=group
    'PAYPAL|PAYPALCSV' = c('filename_test|DOWNLOAD','columnnumber_test|41')
))

all_preprocessing <- all_preprocessing %>% append(list(
    PAYPAL = list(
        PAYPALCSV = c('get_csv_data|HDRTRUE')
    )
))

all_export_rules <- all_export_rules %>% append(list(
    PAYPAL = c('optional_input_types|PAYPALCSV')  #,'maximum_import_type_count|1','unique_import_type_count|1'
    
))

all_export_options <- all_export_options %>% append(list(
    PAYPAL = c('FILE')  #,'QBO'
))

all_export_processing <- all_export_processing %>% append(list(
    PAYPAL = list(
        FILE = c('paypal_export_processing_file')
    )
))


paypal_export_processing_file <- function(selected_imports,param) {
    PAYPALCSV <- tibble()
    selected_imports %>% map(function(x) {
        if (x$import_type=="PAYPALCSV") PAYPALCSV <<- PAYPALCSV %>% rbind(
            sqlite_read_data(x$import_id)
        )
    })
    # write_rds(PAYPALCSV,"../MASTER.rds")
    # PAYPALCSV <- read_rds(file = "../MASTER.rds")

    #rename/select
    paypal_dat <- PAYPALCSV %>%
        dplyr::rename("Date" = PAYPALCSV%>%names()%>%pluck(1)) %>%
        select("Date","Name","Type","Status","Currency","Gross","Fee","Net","From.Email.Address","To.Email.Address","Transaction.ID","Shipping.Address","VAT","Custom.Number","Quantity","Balance","Town.City","County","Postcode","Country","Subject","Country.Code","Balance.Impact")
    
    
    #correct date,amounts
    paypal_dat$Date <- strptime(paypal_dat$Date,format="%d/%m/%Y") %>% as.Date()  #weird title
    paypal_dat$Gross <- paypal_dat$Gross %>% str_remove_all(",") %>% as.numeric()
    paypal_dat$Fee <- paypal_dat$Fee %>% str_remove_all(",") %>% as.numeric()
    paypal_dat$Net <- paypal_dat$Net %>% str_remove_all(",") %>% as.numeric()
    paypal_dat$VAT <- paypal_dat$VAT %>% str_remove_all(",") %>% as.numeric()
    paypal_dat$Balance <- paypal_dat$Balance %>% str_remove_all(",") %>% as.numeric()
    
    
    #QB:expenses - *BillNo,*Supplier,*BillDate,*DueDate,Terms,Location,Memo,*Account,LineDescription,*LineAmount,*LineTaxCode,LineTaxAmount,Currency
    #filter for paypal fees,split by currency
    QBexpenses_paypal <- paypal_dat %>% 
        filter(Fee <0) %>%
        mutate(
            BillNo = Transaction.ID,
            Supplier = str_c("PayPal (",Currency,")"),
            DueDate = Date,
            Terms = "Net 15",
            Location = "",
            Memo = str_c("Charge for payment from ",From.Email.Address),
            Account = "Credit card charges",
            LineDescription = "Paypal payment processing charge",
            LineAmount = (Fee*-1) %>% scales::number(x = .,accuracy = 0.01,big.mark = ""),
            LineTaxCode = "No VAT",
            LineTaxAmount="0"
        ) %>%
        select(BillNo,Supplier,Date,DueDate,Terms,Location,Memo,Account,LineDescription,LineAmount,LineTaxCode,LineTaxAmount,Currency)
    
    #catch other expenses
    QBexpenses <- paypal_dat %>% 
        dplyr::filter(
            Fee ==0 &
                Gross <0 &
                Type !="Payment Refund" &
                Type !="Account Hold for Open Authorisation" &
                Type !="General Currency Conversion" &
                Type !="General Withdrawal"
        ) %>% 
        mutate(
            BillNo = Transaction.ID,
            Supplier = Name,
            DueDate = Date,
            Terms = "Net 15",
            Location = "",
            Memo = str_c("Paid ",To.Email.Address),
            Account = "Sundry expenses",
            LineDescription = Subject,
            LineAmount = Gross %>% scales::number(x = .,accuracy = 0.01,big.mark = ""),
            LineTaxCode = "No VAT",
            LineTaxAmount="0"
        ) %>%
        select(BillNo,Supplier,Date,DueDate,Terms,Location,Memo,Account,LineDescription,LineAmount,LineTaxCode,LineTaxAmount,Currency)
    
    
    #QB:sales receipts - *SalesReceiptNo,Customer,*SalesReceiptDate,*DepositAccount,Location,Memo,Item(Product/Service),ItemDescription,ItemQuantity,ItemRate,*ItemAmount,*ItemTaxCode,ItemTaxAmount,Currency,ServiceDate
    #filter for sales
    QBsalesreceipts <- paypal_dat %>% 
        filter(Type == "Website Payment") %>%
        mutate(
            SalesReceiptNo = Transaction.ID,
            Customer = Name,
            DepositAccount = str_c("Paypal ",Currency),
            Location = "",
            Memo = str_c(Country.Code,": Payment from ",From.Email.Address," Shipping.Address: ",Shipping.Address),
            "Item(Product/Service)" = "Page credit",
            ItemDescription = Subject,
            ItemQuantity = Quantity %>% scales::number(x = .,accuracy = 1,big.mark = ""),
            ItemRate = case_when( as.numeric(Quantity)>0 & as.numeric(Gross)>0 ~ (as.numeric(Gross)-as.numeric(VAT)) / as.numeric(Quantity) , TRUE ~ 0),
            ItemAmount = Gross %>% scales::number(x = .,accuracy = 0.01,big.mark = ""),
            ItemTaxCode = case_when( VAT>0 ~ "20.0% S", TRUE ~ "No VAT" ),
            ItemTaxAmount = VAT,
            ServiceDate = Date
        ) %>%
        select(SalesReceiptNo,Customer,Date,DepositAccount,Location,Memo,"Item(Product/Service)",ItemDescription,ItemQuantity,ItemRate,ItemAmount,ItemTaxCode,ItemTaxAmount,Currency,ServiceDate)
    #check not >1 customer with >1 currencies!!!
    
    #QB:bank - date.description,amount
    #rbind fees to original list, and split into list(currency=tibble())
    bank_by_currency_lst <- paypal_dat %>% 
        filter(
            Type != "Account Hold for Open Authorisation" &
                Type != "Reversal of General Account Hold"
        ) %>%
        mutate(
            Description = str_c(Type,": ",Transaction.ID," ",Country.Code," Payment from ",From.Email.Address," to ",To.Email.Address)
        ) %>%
        select(Currency,Date,Description,Gross) %>%  #Type,Transaction.ID,Shipping.Address,VAT
        rbind(
            QBexpenses_paypal %>% 
                mutate(
                    Description = Memo,
                    Gross = (as.numeric(LineAmount)*-1) %>% scales::number(x = .,accuracy = 0.01,big.mark = "")
                ) %>%
                select(Currency,Date,Description,Gross)
        ) %>%
        group_by(Currency) %>%
        group_split()
    #get currencies
    currencies <- paypal_dat %>% 
        group_by(Currency) %>%
        group_keys()
    names(bank_by_currency_lst) <- currencies %>% as.list() %>% unlist()
    

    #print closing balance/net affect for each currency
    # known missing Type == "Payment Refund" - needs adding manually!!!
    #gross sales receipts-paypal expenses-other expenses+additions from bank account-withdrawals to bank account
    #sales receipts - paypal expenses - other expenses + adjustments = net bank movement (+withdrawals)
    #remainder IS NOT expense or receipt
    QBexpenses_paypal_check <- QBexpenses_paypal %>%  #LineAmount
        select(Currency,LineAmount) %>%
        group_by(Currency) %>%
        dplyr::summarise(.groups="keep",net_value = sum(LineAmount %>% as.numeric())*-1) %>%
        ungroup()
    
    QBexpenses_check <- QBexpenses %>%  #LineAmount
        select(Currency,LineAmount) %>%
        group_by(Currency) %>%
        dplyr::summarise(.groups="keep",net_value = sum(LineAmount %>% as.numeric())) %>%
        ungroup()
    
    QBsalesreceipts_check <- QBsalesreceipts %>%  #$ItemAmount
        select(Currency,ItemAmount) %>%
        group_by(Currency) %>%
        dplyr::summarise(.groups="keep",net_value = sum(ItemAmount %>% as.numeric())) %>%
        ungroup()
    
    bank_by_currency_lst_check <- bank_by_currency_lst %>% lapply(function(x) {sum(x$Gross %>% as.numeric())})
    
    currencies %>% rowwise() %>% unlist() %>% map(function(x) {
        QBsalesreceipts_check_value <- QBsalesreceipts_check %>% filter(Currency==x) %>% pull(net_value) %>% pluck(1)
        QBexpenses_paypal_check_value <- QBexpenses_paypal_check %>% filter(Currency==x) %>% pull(net_value) %>% pluck(1)
        QBexpenses_check_value <- QBexpenses_check %>% filter(Currency==x) %>% pull(net_value) %>% pluck(1)
        bank_by_currency_lst_check_value <- bank_by_currency_lst_check[[x]]
        
        sum( QBsalesreceipts_check_value , QBexpenses_paypal_check_value , QBexpenses_check_value , bank_by_currency_lst_check_value*-1 )
    })
    
    #output to excel
    # QBexpenses_paypal %>% 
    #     mutate(Date = Date %>% as.character(format="%d-%m-%y"),
    #            DueDate = DueDate %>% as.character(format="%d-%m-%y")) %>%
    #     write.csv(.,str_c("../dev/","QBexpenses_paypal",".csv"), row.names = FALSE)
    files <- c()  #filenames for zip
    QBexpenses %>% mutate(LineAmount = (as.numeric(LineAmount)*-1) %>% as.character()) %>% rbind(QBexpenses_paypal) %>% 
        mutate(Date = Date %>% as.character(format="%d-%m-%y"),
               DueDate = DueDate %>% as.character(format="%d-%m-%y")) %>%
        write.csv(.,str_c("export_temp/","QBexpenses_QBexpenses_paypal",".csv"), row.names = FALSE)
    files <- c(files,str_c("export_temp/","QBexpenses_QBexpenses_paypal",".csv"))
    QBsalesreceipts %>% 
        mutate(Date = Date %>% as.character(format="%d-%m-%y"),
               ServiceDate = ServiceDate %>% as.character(format="%d-%m-%y")) %>%
        write.csv(.,str_c("export_temp/","QBsalesreceipts",".csv"), row.names = FALSE)
    files <- c(files,str_c("export_temp/","QBsalesreceipts",".csv"))
    seq_along(bank_by_currency_lst) %>% map(function(x) {
        bank_by_currency_lst[[x]] %>% 
            select(-Currency) %>% 
            mutate(Date = Date %>% as.character(format="%d-%m-%y")) %>%
            write.csv(., str_c("export_temp/",currencies[x,],".csv"), row.names = FALSE)
    })
    seq_along(bank_by_currency_lst) %>% map(function(x) {
        files <<- c(files,str_c("export_temp/",currencies[x,],".csv"))    
    })
    #check missing customers??? Set up new customers with addresses
    
    files
}

