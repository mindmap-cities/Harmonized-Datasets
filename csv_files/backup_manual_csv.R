#if nothing works, do it for each. And go to Opal with the csv's. 

fwrite(record_total, "csv_files/record_total.csv",na = "")

dd_record_xls  <- createWorkbook("csv_files/record_dd.xlsx")
addWorksheet(dd_record_xls, "Variables")
addWorksheet(dd_record_xls, "Categories")
writeData(dd_record_xls, sheet = "Variables",  record_dd$Variables)
writeData(dd_record_xls, sheet = "Categories", record_dd$Categories)
