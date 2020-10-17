get_user_credentials <- function(user) {
    user_base <- tibble(
        user="david",
        password="",
        name="David",
        #permissions="admin",
        folder_ignore_already_exported=FALSE,
        user_settings=list(list(
            GLADAGE = list(
                CSTLIST = list(
                    selected_columns_labels = c("col 1","col 3"),
                    selected_columns = c("col_1","col_3"),
                    valuebox_subtitles = list(
                        count_label = "count",sum_label = "sum",min_label = "min",max_label = "max"
                    ),
                    valuebox_icons = list(
                        count_column = "angle-double-left",sum_column = "angle-double-up",min_column = "angle-double-right",max_column = "angle-double-down"
                    ),
                    valuebox_columns = list(
                        count_column = "col_6",sum_column = "selected_ui",min_column = "col_1",max_column = "col_1"
                    )
                ),#end of import_type
                CSTIVDUEDATE = list(
                    selected_columns_labels = c("col 1","col 3"),
                    selected_columns = c("col_1","col_3"),
                    valuebox_subtitles = list(
                        count_label = "count",sum_label = "sum",min_label = "min",max_label = "max"
                    ),
                    valuebox_icons = list(
                        count_column = "angle-double-left",sum_column = "angle-double-up",min_column = "angle-double-right",max_column = "angle-double-down"
                    ),
                    valuebox_columns = list(
                        count_column = "col_6",sum_column = "selected_ui",min_column = "col_1",max_column = "col_1"
                    )
                ),#end of import_type
                INVSUM = list(
                    selected_columns_labels = c("col 1","col 3"),
                    selected_columns = c("col_1","col_3"),
                    valuebox_subtitles = list(
                        count_label = "count",sum_label = "sum",min_label = "min",max_label = "max"
                    ),
                    valuebox_icons = list(
                        count_column = "angle-double-left",sum_column = "angle-double-up",min_column = "angle-double-right",max_column = "angle-double-down"
                    ),
                    valuebox_columns = list(
                        count_column = "col_6",sum_column = "selected_ui",min_column = "col_1",max_column = "col_1"
                    )
                ),#end of import_type
                DISPATCH = list(
                    selected_columns_labels = c("Member","Member Electoral Ward"),
                    selected_columns = c("Member","Member_Electoral_Ward"),
                    valuebox_subtitles = list(
                        count_label = "count",sum_label = "sum",min_label = "min",max_label = "max"
                    ),
                    valuebox_icons = list(
                        count_column = "angle-double-left",sum_column = "angle-double-up",min_column = "angle-double-right",max_column = "angle-double-down"
                    ),
                    valuebox_columns = list(
                        count_column = "Member",sum_column = "selected_ui",min_column = "Member",max_column = "Member"
                    )
                ),#end of import_type
                MEMBER = list(
                    selected_columns_labels = c("Name","Allocation Address Postcode"),
                    selected_columns = c("Name","Allocation_Address_Postcode"),
                    valuebox_subtitles = list(
                        count_label = "count",sum_label = "sum",min_label = "min",max_label = "max"
                    ),
                    valuebox_icons = list(
                        count_column = "angle-double-left",sum_column = "angle-double-up",min_column = "angle-double-right",max_column = "angle-double-down"
                    ),
                    valuebox_columns = list(
                        count_column = "Name",sum_column = "selected_ui",min_column = "Name",max_column = "Name"
                    )
                )#end of import_type
            ),#end of export_type
            PAYPAL = list(
                PAYPALCSV = list(
                    selected_columns_labels = c("Name","Currency","Gross","Fee","Net"),
                    selected_columns = c("Name","Currency","Gross","Fee","Net"),
                    valuebox_subtitles = list(
                        count_label = "count",sum_label = "sum",min_label = "min",max_label = "max"
                    ),
                    valuebox_icons = list(
                        count_column = "angle-double-left",sum_column = "angle-double-up",min_column = "angle-double-right",max_column = "angle-double-down"
                    ),
                    valuebox_columns = list(
                        count_column = "Name",sum_column = "Gross",min_column = "Net",max_column = "Net"
                    )
                ),#end of import_type
                PAYPALAPI = list(
                    selected_columns_labels = c("transaction_id","transaction_event_code"),
                    selected_columns = c("transaction_id","transaction_event_code"),
                    valuebox_subtitles = list(
                        count_label = "count",sum_label = "sum",min_label = "min",max_label = "max"
                    ),
                    valuebox_icons = list(
                        count_column = "angle-double-left",sum_column = "angle-double-up",min_column = "angle-double-right",max_column = "angle-double-down"
                    ),
                    valuebox_columns = list(
                        count_column = "transaction_id",sum_column = "protection_eligibility",min_column = "protection_eligibility",max_column = "protection_eligibility"
                    )
                )#end of import_type
            )#end of export_type
            
        ))
    )

    user_base %>% filter(user==user)
}



