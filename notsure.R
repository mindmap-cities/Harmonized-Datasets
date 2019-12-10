# csv creation

# last release file

# for(i in 1:length(names_short)){
#   try(write_csv(
#     parceval(paste0(names_short[i],"_total")),
#     paste0("csv_files/",names_short[i],"_Harmo_Table_",last_release,".csv"),
#     col_names = TRUE, na=""))
# }
# 
# # DD xls creation
# save_xls(
#   tbl_var = dd_globe$Variables, 
#   tbl_cat =  dd_globe$Categories, 
#   folder = "xls_files/", 
#   name = 'GLOBE')
# save_xls(dd_hapiee_cz$Variables , "xls_files/", dd_hapiee_cz$Categories , 'HAPIEE_CZ')
# save_xls(dd_hapiee_lt$Variables , "xls_files/", dd_hapiee_lt$Categories , 'HAPIEE_LT')
# save_xls(dd_hapiee_ru$Variables , "xls_files/", dd_hapiee_ru$Categories , 'HAPIEE_RU')
# save_xls(dd_hunt$Variables      , "xls_files/", dd_hunt$Categories      , 'HUNT')
# save_xls(dd_lasa1$Variables     , "xls_files/", dd_lasa1$Categories     , 'LASA1') 
# save_xls(dd_lasa2$Variables     , "xls_files/", dd_lasa2$Categories     , 'LASA2')
# save_xls(dd_lucas$Variables     , "xls_files/", dd_lucas$Categories     , 'LUCAS')
# save_xls(dd_record$Variables    , "xls_files/", dd_record$Categories    , 'RECORD')

# Opal upload files
# library(opalr)
# erasmus_opal = opal.login()
# save_tables <- function(opal_connection, tbl_total, dd_tbl, name_folder, name_file, version){
#   saveOpalTable(
#     opal = opal_connection,
#     tibble = tbl_total,
#     project = name_folder, 
#     table = name_file,
#     variables = dd_tbl$Variables,
#     categories = dd_tbl$Categories,
#     force = TRUE)}

# test var not in DD
# var_not_in_dd(globe_total, dd_globe$Variables)
# var_not_in_dd(hapiee_cz_total, dd_hapiee_cz$Variables)
# var_not_in_dd(hapiee_lt_total, dd_hapiee_lt$Variables)
# var_not_in_dd(hapiee_ru_total, dd_hapiee_ru$Variables)
# var_not_in_dd(hunt_total, dd_hunt$Variables)
# var_not_in_dd(lasa1_total, dd_lasa1$Variables)
# var_not_in_dd(lasa2_total, dd_lasa2$Variables)
# var_not_in_dd(lucas_total, dd_lucas$Variables)
# var_not_in_dd(record_total, dd_record$Variables)

# try(save_tables(erasmus_opal, globe_total,"GLOBE_Harmonized", paste0("globe_DS_", last_release)))
# try(save_tables(erasmus_opal, hapiee_cz_total,dd_hapiee_cz,"HAPIEE_Harmonized", paste0("hapiee_cz_DS_",last_release)))
# try(save_tables(erasmus_opal, hapiee_lt_total,dd_hapiee_lt,"HAPIEE_Harmonized", paste0("hapiee_lt_DS_",last_release)))
# try(save_tables(erasmus_opal, hapiee_ru_total,dd_hapiee_ru,"HAPIEE_Harmonized", paste0("hapiee_ru_DS_",last_release)))
# try(save_tables(erasmus_opal, hunt_total,     dd_hunt,     "HUNT_Harmonized",   paste0("hunt_DS_",     last_release)))
# try(save_tables(erasmus_opal, lasa1_total,    dd_lasa1,    "LASA_Harmonized",   paste0("lasa2_DS_",    last_release)))
# try(save_tables(erasmus_opal, lasa2_total,    dd_lasa2,    "LASA_Harmonized",   paste0("lasa1_DS_",    last_release)))
# try(save_tables(erasmus_opal, lucas_total,    dd_lucas,    "LUCAS_Harmonized",  paste0("lucas_DS_",    last_release)))
# try(save_tables(erasmus_opal, record_total,   dd_record,   "RECORD_Harmonized", paste0("record_DS_",   last_release)))

#dd_physenv <- dd_physenv$Variables %>% mutate (unit = ifelse(unit == "m²", "m2",unit))


# MANUAL CHANGES
# dd_globe$Variables$valueType <- dd_hunt$Variables$valueType
# dd_lasa2$Variables$valueType <- dd_lasa1$Variables$valueType



# save_xls <- function(tbl_var,folder, tbl_cat,name){
#   library(openxlsx)
#   zip    <- zip::zipr 
#   dd_ttl = createWorkbook()
#   addWorksheet(dd_ttl, "Variables")
#   addWorksheet(dd_ttl, "Categories")
#   writeData(dd_ttl, sheet = 'Variables', tbl_var, rowNames = FALSE)
#   writeData(dd_ttl, sheet = 'Categories', tbl_cat, rowNames = FALSE)
#   saveWorkbook(dd_ttl, paste0(folder, "DD_",name,".xlsx"), overwrite = TRUE)}









