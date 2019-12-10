
# Fixing the variable classes by
# Changing classes of the variables into the Value Types in the data dictionary, in case they are different in the dataset itself

# try({clsa_total    <- change_class(clsa_total, dd_clasa, "CLSA")})
try({globe_total     <- change_class(globe_total,dd_globe, "GLOBE")})
try({hapiee_lt_total <- change_class(hapiee_lt_total,dd_hapiee_lt, "HAPIEE_LT")})
try({hapiee_cz_total <- change_class(hapiee_cz_total,dd_hapiee_cz, "HAPIEE_CZ")})
try({hapiee_ru_total <- change_class(hapiee_ru_total,dd_hapiee_ru, "HAPIEE_RU")})
try({lasa1_total     <- change_class(lasa1_total,dd_lasa1, "LASA1")})
try({lasa2_total     <- change_class(lasa2_total,dd_lasa2, "LASA2")})
try({lucas_total     <- change_class(lucas_total,dd_lucas, "LUCAS")})
try({hunt_total      <- change_class(hunt_total,dd_hunt, "HUNT")})
try({record_total    <- change_class(record_total,dd_record, "RECORD")})

# Excluding unwanted physenv variable from the harmonized dataset of each study

try({globe_total %<>% select(-starts_with("physenv_cn_bf_facil3000_"))})
try({hapiee_cz_total %<>% select(-starts_with("physenv_cn_bf_facil3000_"))})
try({hapiee_lt_total %<>% select(-starts_with("physenv_cn_bf_facil3000_"))})
try({hapiee_ru_total %<>% select(-starts_with("physenv_cn_bf_facil3000_"))})
try({hunt_total %<>% select(-starts_with("physenv_cn_bf_facil3000_"))})
try({lasa1_total %<>% select(-starts_with("physenv_cn_bf_facil3000_"))})
try({lasa2_total %<>% select(-starts_with("physenv_cn_bf_facil3000_"))})
try({lucas_total %<>% select(-starts_with("physenv_cn_bf_facil3000_"))})
try({record_total %<>% select(-starts_with("physenv_cn_bf_facil3000_"))})

# Excluding unwanted physenv variable from the data dictionary of each study

try({dd_globe$Variables %<>% filter(!str_detect(name,"physenv_cn_bf_facil3000_"))})
try({dd_hapiee_cz$Variables %<>% filter(!str_detect(name,"physenv_cn_bf_facil3000_"))})
try({dd_hapiee_lt$Variables %<>% filter(!str_detect(name,"physenv_cn_bf_facil3000_"))})
try({dd_hapiee_ru$Variables %<>% filter(!str_detect(name,"physenv_cn_bf_facil3000_"))})
try({dd_hunt$Variables %<>% filter(!str_detect(name,"physenv_cn_bf_facil3000_"))})
try({dd_lasa1$Variables %<>% filter(!str_detect(name,"physenv_cn_bf_facil3000_"))})
try({dd_lasa2$Variables %<>% filter(!str_detect(name,"physenv_cn_bf_facil3000_"))})
try({dd_lucas$Variables %<>% filter(!str_detect(name,"physenv_cn_bf_facil3000_"))})
try({dd_record$Variables %<>% filter(!str_detect(name,"physenv_cn_bf_facil3000_"))})


# manual changes: valueType of var in lasa2 is not the good one, 
# so it is replaced by valueType of lasa1
dd_lasa2$Variables$valueType <- dd_lasa1$Variables$valueType
