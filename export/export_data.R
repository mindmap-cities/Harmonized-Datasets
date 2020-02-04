message("[5 - init]: export to Opal")

message("    [5.1]: application of the dictionary to data sets")

#Reading the last line from the sheet version_release.info (eg. _01_1, _01_2, _02_1) 
#to later assign the name of the latest release to the harmonized tables in Opal

total_release <- readLines("export/version_release.info")
last_release <- total_release[length(total_release)]


library(harmor)
# Applying data dictionaries to each harmonized dataset using harmor function
#try({rotterdam_total <- applyDictionary(rotterdam_total,     variables=dd_globe$Variables,     categories=dd_globe$Categories)})
try({clsa_cop_total  <- applyDictionary(clsa_cop_total,   variables=dd_globe$Variables,   categories=dd_globe$Categories)})
try({clsa_tra_total  <- applyDictionary(clsa_tra_total,   variables=dd_globe$Variables,   categories=dd_globe$Categories)})
try({globe_total     <- applyDictionary(globe_total,     variables=dd_globe$Variables,     categories=dd_globe$Categories)})
try({hapiee_cz_total <- applyDictionary(hapiee_cz_total, variables=dd_hapiee_cz$Variables, categories=dd_hapiee_cz$Categories)})
try({hapiee_lt_total <- applyDictionary(hapiee_lt_total, variables=dd_hapiee_lt$Variables, categories=dd_hapiee_lt$Categories)})
try({hapiee_ru_total <- applyDictionary(hapiee_ru_total, variables=dd_hapiee_ru$Variables, categories=dd_hapiee_ru$Categories)})
try({hunt_total      <- applyDictionary(hunt_total,      variables=dd_hunt$Variables,      categories=dd_hunt$Categories)})
try({lasa1_total     <- applyDictionary(lasa1_total,     variables=dd_lasa1$Variables,     categories=dd_lasa1$Categories)})
try({lasa2_total     <- applyDictionary(lasa2_total,     variables=dd_lasa2$Variables,     categories=dd_lasa2$Categories)})
try({lucas_total     <- applyDictionary(lucas_total,     variables=dd_lucas$Variables,     categories=dd_lucas$Categories)})
try({record_total    <- applyDictionary(record_total,    variables=dd_record$Variables,    categories=dd_record$Categories)})

# uploading the harmonized tables (complete with data dictionaries and with the title including the last release number) 
#to opal directly

library(opalr)
options(verbose = FALSE)
o <- opal.login()
message("    [5.2]: upload tibbles to Opal")

#try({saveOpalTable(o, rotterdam_total, "ROTTERDAM_Harmonized",  paste0("rotterdam_DS_1_", last_release),       force = TRUE)})
try({saveOpalTable(o, clsa_cop_total,  "CLSA_Harmonized",  paste0("clsa_cop_DS_1_", last_release),  force = TRUE)})
try({saveOpalTable(o, clsa_tra_total,  "CLSA_Harmonized",  paste0("clsa_tra_DS_1_", last_release),  force = TRUE)})
try({saveOpalTable(o, globe_total,     "GLOBE_Harmonized",  paste0("globe_DS_1_", last_release),       force = TRUE)})
try({saveOpalTable(o, hapiee_cz_total, "HAPIEE_Harmonized", paste0("hapiee_cz_DS_1_", last_release), force = TRUE)})
try({saveOpalTable(o, hapiee_lt_total, "HAPIEE_Harmonized", paste0("hapiee_lt_DS_1_", last_release), force = TRUE)})
try({saveOpalTable(o, hapiee_ru_total, "HAPIEE_Harmonized", paste0("hapiee_ru_DS_1_", last_release), force = TRUE)})
try({saveOpalTable(o, hunt_total,      "HUNT_Harmonized",   paste0("hunt_DS_1_", last_release),      force = TRUE)})
try({saveOpalTable(o, lasa1_total,     "LASA_Harmonized",   paste0("lasa1_DS_1_", last_release),     force = TRUE)})
try({saveOpalTable(o, lasa2_total,     "LASA_Harmonized",   paste0("lasa2_DS_1_", last_release),     force = TRUE)})
try({saveOpalTable(o, lucas_total,     "LUCAS_Harmonized",  paste0("lucas_DS_1_", last_release),     force = TRUE)})
try({saveOpalTable(o, record_total,    "RECORD_Harmonized", paste0("record_DS_1_", last_release),    force = TRUE)})

try({opal.logout(o)})
save.image(file="src/4_data_with_dd.RData.RData")
message("    [5]: all data have been exported to Opal and saved in src/4_")
