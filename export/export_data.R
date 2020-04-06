message("[5 - init]: export to Opal")

message("    [5.1]: application of the dictionary to data sets")

#Reading the last line from the sheet version_release.info (eg. _01_1, _01_2, _02_1) 
#to later assign the name of the latest release to the harmonized tables in Opal

total_release <- readLines("export/version_release.info")
last_release <- total_release[length(total_release)]


# Applying data dictionaries to each harmonized dataset using harmor function
#try({rotterdam_total <- applyDictionary(rotterdam_total,     variables=dd_globe$Variables,     categories=dd_globe$Categories)})
try({clsa_cop_total_dd  <- applyDictionary(clsa_cop_total,  variables = dd_clsa_cop$Variables  %>% mutate(table = paste0("clsa_cop_DS_",  last_release)), categories = dd_clsa_cop$Categories)})
try({clsa_tra_total_dd  <- applyDictionary(clsa_tra_total,  variables = dd_clsa_tra$Variables  %>% mutate(table = paste0("clsa_tra_DS_",  last_release)), categories = dd_clsa_tra$Categories)})
try({globe_total_dd     <- applyDictionary(globe_total,     variables = dd_globe$Variables     %>% mutate(table = paste0("globe_DS_",     last_release)), categories = dd_globe$Categories)})
try({hapiee_cz_total_dd <- applyDictionary(hapiee_cz_total, variables = dd_hapiee_cz$Variables %>% mutate(table = paste0("hapiee_cz_DS_", last_release)), categories = dd_hapiee_cz$Categories)})
try({hapiee_lt_total_dd <- applyDictionary(hapiee_lt_total, variables = dd_hapiee_lt$Variables %>% mutate(table = paste0("hapiee_lt_DS_", last_release)), categories = dd_hapiee_lt$Categories)})
try({hapiee_ru_total_dd <- applyDictionary(hapiee_ru_total, variables = dd_hapiee_ru$Variables %>% mutate(table = paste0("hapiee_ru_DS_", last_release)), categories = dd_hapiee_ru$Categories)})
try({hunt_total_dd      <- applyDictionary(hunt_total,      variables = dd_hunt$Variables      %>% mutate(table = paste0("hunt_DS_",      last_release)), categories = dd_hunt$Categories)})
try({lasa1_total_dd     <- applyDictionary(lasa1_total,     variables = dd_lasa1$Variables     %>% mutate(table = paste0("lasa1_DS_",     last_release)), categories = dd_lasa1$Categories)})
try({lasa2_total_dd     <- applyDictionary(lasa2_total,     variables = dd_lasa2$Variables     %>% mutate(table = paste0("lasa2_DS_",     last_release)), categories = dd_lasa2$Categories)})
try({lucas_total_dd     <- applyDictionary(lucas_total,     variables = dd_lucas$Variables     %>% mutate(table = paste0("lucas_DS_",     last_release)), categories = dd_lucas$Categories)})
try({record_total_dd    <- applyDictionary(record_total,    variables = dd_record$Variables    %>% mutate(table = paste0("record_DS_",    last_release)), categories = dd_record$Categories)})

# uploading the harmonized tables (complete with data dictionaries and with the title including the last release number) 
#to opal directly

options(verbose = FALSE)
message("    [5.2]: upload tibbles to Opal")

# erasmus_opal <- opal.login()
# #try({saveOpalTable(erasmus_opal, rotterdam_total_dd, "ROTTERDAM_Harmonized",  paste0("rotterdam_DS_", last_release),       force = TRUE)})
# try({opal.logout(erasmus_opal)})

erasmus_opal <- opal.login()
try({saveOpalTable(erasmus_opal, clsa_cop_total_dd,  "CLSA_Harmonized",   paste0("clsa_cop_DS_",  last_release), force = TRUE)})
try({opal.logout(erasmus_opal)})

erasmus_opal <- opal.login()
try({saveOpalTable(erasmus_opal, clsa_tra_total_dd,  "CLSA_Harmonized",   paste0("clsa_tra_DS_",  last_release), force = TRUE)})
 try({opal.logout(erasmus_opal)})

erasmus_opal <- opal.login()
try({saveOpalTable(erasmus_opal, globe_total_dd,     "GLOBE_Harmonized",  paste0("globe_DS_",     last_release), force = TRUE)})
try({opal.logout(erasmus_opal)})

erasmus_opal <- opal.login()
try({saveOpalTable(erasmus_opal, hapiee_cz_total_dd, "HAPIEE_Harmonized", paste0("hapiee_cz_DS_", last_release), force = TRUE)})
try({opal.logout(erasmus_opal)})

erasmus_opal <- opal.login()
try({saveOpalTable(erasmus_opal, hapiee_lt_total_dd, "HAPIEE_Harmonized", paste0("hapiee_lt_DS_", last_release), force = TRUE)})
try({opal.logout(erasmus_opal)})

erasmus_opal <- opal.login()
try({saveOpalTable(erasmus_opal, hapiee_ru_total_dd, "HAPIEE_Harmonized", paste0("hapiee_ru_DS_", last_release), force = TRUE)})
try({opal.logout(erasmus_opal)})

erasmus_opal <- opal.login()
try({saveOpalTable(erasmus_opal, hunt_total_dd,      "HUNT_Harmonized",   paste0("hunt_DS_",      last_release), force = TRUE)})
try({opal.logout(erasmus_opal)})

erasmus_opal <- opal.login()
try({saveOpalTable(erasmus_opal, lasa1_total_dd,     "LASA_Harmonized",   paste0("lasa1_DS_",     last_release), force = TRUE)})
try({opal.logout(erasmus_opal)})

erasmus_opal <- opal.login()
try({saveOpalTable(erasmus_opal, lasa2_total_dd,     "LASA_Harmonized",   paste0("lasa2_DS_",     last_release), force = TRUE)})
try({opal.logout(erasmus_opal)})

erasmus_opal <- opal.login()
try({saveOpalTable(erasmus_opal, lucas_total_dd,     "LUCAS_Harmonized",  paste0("lucas_DS_",     last_release), force = TRUE)})
try({opal.logout(erasmus_opal)})

erasmus_opal <- opal.login()
try({saveOpalTable(erasmus_opal, record_total_dd,    "RECORD_Harmonized", paste0("record_DS_",    last_release), force = TRUE)})
try({opal.logout(erasmus_opal)})

save.image(file="src/4_data_with_dd.RData.RData")
message("    [5]: all data have been exported to Opal and saved in src/4_")
