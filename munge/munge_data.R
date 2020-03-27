message("[4 - init]: munging process")

#################################  DATA DICTIONARY #############################

#Creating specific paths for each study seperately by choosing the domains of interest for each

try({path_clsa_cop  = path_list[str_detect(string = path_list,pattern="CLSA_COP")]})
try({path_clsa_tra  = path_list[str_detect(string = path_list,pattern="CLSA_TRA")]})
try({path_globe     = path_list[str_detect(string = path_list,pattern="GLOBE")]})
try({path_hapiee_cz = path_list[str_detect(string = path_list,pattern="HAPIEE_CZ")]})
try({path_hapiee_lt = path_list[str_detect(string = path_list,pattern="HAPIEE_LT")]})
try({path_hapiee_ru = path_list[str_detect(string = path_list,pattern="HAPIEE_RU")]})
try({path_hunt      = path_list[str_detect(string = path_list,pattern="HUNT")]})
try({path_lasa1     = path_list[str_detect(string = path_list,pattern="LASA1")]})
try({path_lasa2     = path_list[str_detect(string = path_list,pattern="LASA2")]})
try({path_lucas     = path_list[str_detect(string = path_list,pattern="LUCAS")]})
try({path_record    = path_list[str_detect(string = path_list,pattern="RECORD")]})

#For each .Rmd file, detect the first variable. It will exclude all before, and rbind for each study, domain after domain. 
#creation of a dataframe containing 1 column, with rows containing variable information (label, description, status, etc.)
#the following code separates each variable from its information, and put them in a opal-compatible format (CSV)

message("    [4.1]: creation of data dictionaries")
try({dd_clsa_cop  <- create_dd(path_clsa_cop , 'CLSA_COP' )})
try({dd_clsa_tra  <- create_dd(path_clsa_tra , 'CLSA_TRA' )})
try({dd_globe     <- create_dd(path_globe , 'globe' )})
try({dd_hapiee_cz <- create_dd(path_hapiee_cz , 'HAPIEE_CZ' )}) #ok
try({dd_hapiee_lt <- create_dd(path_hapiee_lt , 'HAPIEE_LT' )}) #ok
try({dd_hapiee_ru <- create_dd(path_hapiee_ru , 'HAPIEE_RU' )}) #ok
try({dd_hunt      <- create_dd(path_hunt , 'HUNT' )})
try({dd_lasa1     <- create_dd(path_lasa1 , 'LASA1' )})
try({dd_lasa2     <- create_dd(path_lasa2 , 'LASA2' )})
try({dd_lucas     <- create_dd(path_lucas , 'LUCAS' )})
try({dd_record    <- create_dd(path_record , 'RECORD' )}) #ok


#Creating dd for physical environmental domain  

path_env = "../physical_environmental/PHYSENV_DS.Rmd"
try({dd_physenv <- create_dd(path_env, "physenv")})

#Adding physenv to the Variables sheet of all data dictionaries
try({dd_clsa_cop$Variables  <- dd_clsa_cop$Variables %>% bind_rows(dd_physenv$Variables)}) 
try({dd_clsa_tra$Variables  <- dd_clsa_tra$Variables %>% bind_rows(dd_physenv$Variables)}) 
try({dd_globe$Variables     <- dd_globe$Variables %>% bind_rows(dd_physenv$Variables)}) 
try({dd_hapiee_cz$Variables <- dd_hapiee_cz$Variables %>% bind_rows(dd_physenv$Variables)})
try({dd_hapiee_lt$Variables <- dd_hapiee_lt$Variables %>% bind_rows(dd_physenv$Variables)})
try({dd_hapiee_ru$Variables <- dd_hapiee_ru$Variables %>% bind_rows(dd_physenv$Variables)})
try({dd_hunt$Variables      <- dd_hunt$Variables %>% bind_rows(dd_physenv$Variables)})
try({dd_lasa1$Variables     <- dd_lasa1$Variables %>% bind_rows(dd_physenv$Variables)}) 
try({dd_lasa2$Variables     <- dd_lasa2$Variables %>% bind_rows(dd_physenv$Variables)}) 
try({dd_lucas$Variables     <- dd_lucas$Variables %>% bind_rows(dd_physenv$Variables)})  
try({dd_record$Variables    <- dd_record$Variables %>% bind_rows(dd_physenv$Variables)}) 

#Adding physenv to the Categories sheet of all data dictionaries
try({dd_clsa_cop$Categories  <- dd_clsa_cop$Categories %>% bind_rows(dd_physenv$Categories)})
try({dd_clsa_tra$Categories  <- dd_clsa_tra$Categories %>% bind_rows(dd_physenv$Categories)})
try({dd_globe$Categories     <- dd_globe$Categories %>% bind_rows(dd_physenv$Categories)})
try({dd_hapiee_cz$Categories <- dd_hapiee_cz$Categories %>% bind_rows(dd_physenv$Categories)})
try({dd_hapiee_lt$Categories <- dd_hapiee_lt$Categories %>% bind_rows(dd_physenv$Categories)})
try({dd_hapiee_ru$Categories <- dd_hapiee_ru$Categories %>% bind_rows(dd_physenv$Categories)})
try({dd_hunt$Categories      <- dd_hunt$Categories %>% bind_rows(dd_physenv$Categories)})
try({dd_lasa1$Categories     <- dd_lasa1$Categories %>% bind_rows(dd_physenv$Categories)})
try({dd_lasa2$Categories     <- dd_lasa2$Categories %>% bind_rows(dd_physenv$Categories)})
try({dd_lucas$Categories     <- dd_lucas$Categories %>% bind_rows(dd_physenv$Categories)})
try({dd_record$Categories    <- dd_record$Categories %>% bind_rows(dd_physenv$Categories)})

#### 


#Ordering of data dictionary by waves and domains

message("    [4.1]: ordering of data dictionary by waves and domains")
try({dd_clsa_cop  <- order_dd(dd_clsa_cop)})
try({dd_clsa_tra  <- order_dd(dd_clsa_tra)})
try({dd_globe     <- order_dd(dd_globe)})
try({dd_hapiee_cz <- order_dd(dd_hapiee_cz)})
try({dd_hapiee_lt <- order_dd(dd_hapiee_lt)})
try({dd_hapiee_ru <- order_dd(dd_hapiee_ru)})
try({dd_hunt      <- order_dd(dd_hunt)})
try({dd_lasa1     <- order_dd(dd_lasa1)})
try({dd_lasa2     <- order_dd(dd_lasa2)})
try({dd_lucas     <- order_dd(dd_lucas)})
try({dd_record    <- order_dd(dd_record)})

source("diagnostics/repair_hapiee_ids.R")

###################################  DATA ######################################


# Creating total harmonized datasets including the data from all current domains of interest for each
message("    [4.2]: merging of all data sets")
try({clsa_cop_total <- join_data("CLSA_cop"); base::rm(list=ls(pattern = "CLSA_cop", envir = .GlobalEnv))})
try({clsa_tra_total <- join_data("CLSA_tra"); base::rm(list=ls(pattern = "CLSA_tra", envir = .GlobalEnv))})
try({globe_total <- join_data("GLOBE"); base::rm(list=ls(pattern = "GLOBE", envir = .GlobalEnv))})
try({hapiee_cz_total <- join_data("HAPIEE_CZ"); base::rm(list=ls(pattern = "HAPIEE_CZ", envir = .GlobalEnv))})
try({hapiee_lt_total <- join_data("HAPIEE_LT"); base::rm(list=ls(pattern = "HAPIEE_LT", envir = .GlobalEnv))})
try({hapiee_ru_total <- join_data("HAPIEE_RU"); base::rm(list=ls(pattern = "HAPIEE_RU", envir = .GlobalEnv))})
try({hunt_total <- join_data("HUNT"); base::rm(list=ls(pattern = "HUNT", envir = .GlobalEnv))})
try({lasa1_total <- join_data("LASA1"); base::rm(list=ls(pattern = "LASA1", envir = .GlobalEnv))})
try({lasa2_total <- join_data("LASA2"); base::rm(list=ls(pattern = "LASA2", envir = .GlobalEnv))})
try({lucas_total <- join_data("LUCAS"); base::rm(list=ls(pattern = "LUCAS", envir = .GlobalEnv))})
try({record_total <- join_data("RECORD"); base::rm(list=ls(pattern = "RECORD", envir = .GlobalEnv))})


##### BOTH DATA AND DD #####

#Adding time related variables (baseline_yr, t1, etc) and the status (complete, impossible) of phsyenv variables to the Data Dictionaries
try({dd_clsa_cop  <- complete_dd(dd_clsa_cop ,    clsa_cop_total,      'clsa' )})
try({dd_clsa_tra  <- complete_dd(dd_clsa_tra ,    clsa_tra_total,      'clsa' )})
try({dd_globe     <- complete_dd(dd_globe,     globe_total,     'globe')})
try({dd_hapiee_cz <- complete_dd(dd_hapiee_cz, hapiee_cz_total, 'hapiee_cz' )})
try({dd_hapiee_lt <- complete_dd(dd_hapiee_lt, hapiee_lt_total, 'hapiee_lt' )})
try({dd_hapiee_ru <- complete_dd(dd_hapiee_ru, hapiee_ru_total, 'hapiee_ru')})
try({dd_hunt      <- complete_dd(dd_hunt,      hunt_total,      'hunt')})
try({dd_lasa1     <- complete_dd(dd_lasa1,     lasa1_total,     'lasa1')})
try({dd_lasa2     <- complete_dd(dd_lasa2,     lasa2_total,     'lasa2')})
try({dd_lucas     <- complete_dd(dd_lucas,     lucas_total,     'lucas')})
try({dd_record    <- complete_dd(dd_record,    record_total,    'record')})

message("    [4.3]: repair datasests by renaming and retyping")
source("diagnostics/repair_data.R")


#Adding proper time variables (data collection years and t1, t2, etc) to the merged tables for each study

message("    [4.3]: addition of time (baseline and followups) in data and data dictionaries")

try({clsa_tra_total =  clsa_tra_total %>%
mutate(
  baseline_yr	   = recode(baseline_yr,"0"="2011"),
  followup1_yr	 = recode(followup1_yr,"1"="2015"),
  t1	 = rep(2015 - 2011) %>% as.character) %>%
  select(-one_of(var_not_in_dd(clsa_tra_total, dd_clsa_tra$Variables)))})
try({dd_clsa_tra$Variables <- dd_clsa_tra$Variables %>%
  mutate(
    script = 
      ifelse(name %in% c("followup2_yr","followup3_yr","followup4_yr","followup5_yr","followup6_yr","t2","t3","t4","t5","t6"),NA,script),
    `Mlstr_harmo::status` = 
      ifelse(name %in% c("followup2_yr","followup3_yr","followup4_yr","followup5_yr","followup6_yr","t2","t3","t4","t5","t6"),"impossible",`Mlstr_harmo::status`))})

try({clsa_cop_total =  clsa_cop_total %>%
  mutate(
    baseline_yr	   = recode(baseline_yr,"0"="2012"),
    followup1_yr	 = recode(followup1_yr,"1"="2015"),
    t1	 = rep(2015 - 2012) %>% as.character) %>%
  select(-one_of(var_not_in_dd(clsa_cop_total, dd_clsa_cop$Variables)))})
try({dd_clsa_cop$Variables <- dd_clsa_cop$Variables %>%
  mutate(
    script = 
      ifelse(name %in% c("followup2_yr","followup3_yr","followup4_yr","followup5_yr","followup6_yr","t2","t3","t4","t5","t6"),NA,script),
    `Mlstr_harmo::status` = 
      ifelse(name %in% c("followup2_yr","followup3_yr","followup4_yr","followup5_yr","followup6_yr","t2","t3","t4","t5","t6"),"impossible",`Mlstr_harmo::status`))})

try({globe_total = globe_total %>%
  mutate(
    baseline_yr	   = recode(baseline_yr,"0"="1991"),
    followup1_yr	 = recode(followup1_yr,"1"="1997"),
    followup2_yr	 = recode(followup2_yr,"2"="2004"),
    followup3_yr	 = recode(followup3_yr,"3"="2011"),
    followup4_yr	 = recode(followup4_yr,"4"="2014")) %>%
  mutate(
    t1	 = rep(1997 - 1991) %>% as.character,
    t2	 = rep(2004 - 1997) %>% as.character,
    t3	 = rep(2011 - 2004) %>% as.character,
    t4	 = rep(2014 - 2011) %>% as.character) %>%
  select(-one_of(var_not_in_dd(globe_total, dd_globe$Variables)))})
try({dd_globe$Variables <- dd_globe$Variables %>%
  mutate(
    script = 
      ifelse(name %in% c("followup5_yr","followup6_yr","t5","t6"),NA,script),
    `Mlstr_harmo::status` = 
      ifelse(name %in% c("followup5_yr","followup6_yr","t5","t6"),"impossible",`Mlstr_harmo::status`))})

try({hapiee_cz_total = hapiee_cz_total %>%
  mutate(
    baseline_yr	   = recode(baseline_yr,"0"="2002"),
    followup1_yr	 = recode(followup1_yr,"1"="2006")) %>%
  mutate(
  #   baseline_yr	   = rep(2002) %>% as.character,
  #   followup1_yr	 = rep(2006) %>% as.character,
    t1	 = rep(2006 - 2002) %>% as.character) %>%
  select(-one_of(var_not_in_dd(hapiee_cz_total, dd_hapiee_cz$Variables)))})
try({dd_hapiee_cz$Variables <- dd_hapiee_cz$Variables %>%
  mutate(
    script = 
      ifelse(name %in% c("followup2_yr","followup3_yr","followup4_yr","followup5_yr","followup6_yr","t2","t3","t4","t5","t6"),NA,script),
    `Mlstr_harmo::status` = 
      ifelse(name %in% c("followup2_yr","followup3_yr","followup4_yr","followup5_yr","followup6_yr","t2","t3","t4","t5","t6"),"impossible",`Mlstr_harmo::status`))})

try({hapiee_ru_total = hapiee_ru_total %>%
  mutate(
    baseline_yr	   = recode(baseline_yr,"0"="2002"),
    followup1_yr	 = recode(followup1_yr,"1"="2006")) %>%
  mutate(
  #   baseline_yr	   = rep(2002) %>% as.character,
  #   followup1_yr	 = rep(2006) %>% as.character,
    t1	 = rep(2006 - 2002) %>% as.character) %>%
  select(-one_of(var_not_in_dd(hapiee_ru_total, dd_hapiee_ru$Variables)))})
try({dd_hapiee_ru$Variables <- dd_hapiee_ru$Variables %>%
  mutate(
    script = 
      ifelse(name %in% c("followup2_yr","followup3_yr","followup4_yr","followup5_yr","followup6_yr","t2","t3","t4","t5","t6"),NA,script),
    `Mlstr_harmo::status` = 
      ifelse(name %in% c("followup2_yr","followup3_yr","followup4_yr","followup5_yr","followup6_yr","t2","t3","t4","t5","t6"),"impossible",`Mlstr_harmo::status`))})


try({hapiee_lt_total = hapiee_lt_total %>%
  mutate(
    baseline_yr	   = recode(baseline_yr,"0"="2005")) %>%
  # mutate(
  #   baseline_yr	   = rep(2005) %>% as.character) %>%
  select(-one_of(var_not_in_dd(hapiee_lt_total, dd_hapiee_lt$Variables)))})
try({dd_hapiee_lt$Variables <- dd_hapiee_lt$Variables %>%
  mutate(
    script = 
      ifelse(name %in% c("followup1_yr","followup2_yr","followup3_yr","followup4_yr","followup5_yr","followup6_yr","t1","t2","t3","t4","t5","t6"),NA,script),
    `Mlstr_harmo::status` = 
      ifelse(name %in% c("followup1_yr","followup2_yr","followup3_yr","followup4_yr","followup5_yr","followup6_yr","t1","t2","t3","t4","t5","t6"),"impossible",`Mlstr_harmo::status`))})



try({hunt_total = hunt_total %>%
  mutate(
    baseline_yr	   = recode(baseline_yr,"0"="1984"),
    followup1_yr	 = recode(followup1_yr,"1"="1995"),
    followup2_yr	 = recode(followup2_yr,"2"="2006")) %>%
  
  mutate(
  #   baseline_yr	   = rep(1984) %>% as.character,
  #   followup1_yr	 = rep(1995) %>% as.character,
  #   followup2_yr	 = rep(2006) %>% as.character,
    t1	 = rep(1995 - 1984) %>% as.character,
    t2	 = rep(2006 - 1995) %>% as.character) %>%
  select(-one_of(var_not_in_dd(hunt_total, dd_hunt$Variables)))})
try({dd_hunt$Variables <- dd_hunt$Variables %>%
  mutate(
    script = 
      ifelse(name %in% c("followup3_yr","followup4_yr","followup5_yr","followup6_yr","t3","t4","t5","t6"),NA,script),
    `Mlstr_harmo::status` = 
      ifelse(name %in%  c("followup3_yr","followup4_yr","followup5_yr","followup6_yr","t3","t4","t5","t6"),"impossible",`Mlstr_harmo::status`))})


try({lasa1_total = lasa1_total %>%
  mutate(
    baseline_yr	   = recode(baseline_yr,"0"="1992"),
    followup1_yr	 = recode(followup1_yr,"1"="1995"),
    followup2_yr	 = recode(followup2_yr,"2"="1998"),
    followup3_yr	 = recode(followup3_yr,"3"="2001"),
    followup4_yr	 = recode(followup4_yr,"4"="2005"),
    followup5_yr	 = recode(followup5_yr,"5"="2008"),
    followup6_yr	 = recode(followup6_yr,"6"="2011")) %>%
  # 
  mutate(
  #   baseline_yr	   = rep(1992) %>% as.character,
  #   followup1_yr	 = rep(1995) %>% as.character,
  #   followup2_yr	 = rep(1998) %>% as.character,
  #   followup3_yr	 = rep(2001) %>% as.character,
  #   followup4_yr	 = rep(2005) %>% as.character,
  #   followup5_yr	 = rep(2008) %>% as.character,
  #   followup6_yr	 = rep(2011) %>% as.character,
    t1	 = rep(1995-1992) %>% as.character,
    t2	 = rep(1998-1995) %>% as.character,
    t3	 = rep(2001-1998) %>% as.character,
    t4	 = rep(2005-2001) %>% as.character,
    t5	 = rep(2008-2005) %>% as.character,
    t6	 = rep(2011-2008) %>% as.character) %>%
  select(-one_of(var_not_in_dd(lasa1_total, dd_lasa1$Variables)))})
try({dd_lasa1$Variables <- dd_lasa1$Variables })


try({lasa2_total = lasa2_total %>%
  mutate(
    baseline_yr	   = recode(baseline_yr,"0"="2002"),
    followup1_yr	 = recode(followup1_yr,"1"="2005"),
    followup2_yr	 = recode(followup2_yr,"2"="2008"),
    followup3_yr	 = recode(followup3_yr,"3"="2011")) %>%
  
  mutate(
  #   baseline_yr	   = rep(2002) %>% as.character,
  #   followup1_yr	 = rep(2005) %>% as.character,
  #   followup2_yr	 = rep(2008) %>% as.character,
  #   followup3_yr	 = rep(2011) %>% as.character,
    t1	 = rep(2005 - 2002) %>% as.character,
    t2	 = rep(2008 - 2005) %>% as.character,
    t3	 = rep(2011 - 2008) %>% as.character) %>%
  select(- one_of(var_not_in_dd(lasa2_total, dd_lasa2$Variables)))})
try({dd_lasa2$Variables <- dd_lasa2$Variables %>%
  mutate(
    script = 
      ifelse(name %in% c("followup4_yr","followup5_yr","followup6_yr","t4","t5","t6"),NA,script),
    `Mlstr_harmo::status` = 
      ifelse(name %in% c("followup4_yr","followup5_yr","followup6_yr","t4","t5","t6"),"impossible",`Mlstr_harmo::status`))})


try({lucas_total = lucas_total %>%
  mutate(
    baseline_yr	   = recode(baseline_yr,"0"="2000")) %>%
  # 
  # mutate(
  #   baseline_yr	   = rep(2000) %>% as.character) %>%
  select(-one_of(var_not_in_dd(lucas_total, dd_lucas$Variables)))})
try({dd_lucas$Variables <- dd_lucas$Variables %>%
  mutate(
    script = 
      ifelse(name %in% c("followup1_yr","followup2_yr","followup3_yr","followup4_yr","followup5_yr","followup6_yr","t1","t2","t3","t4","t5","t6"),NA,script),
    `Mlstr_harmo::status` = 
      ifelse(name %in% c("followup1_yr","followup2_yr","followup3_yr","followup4_yr","followup5_yr","followup6_yr","t1","t2","t3","t4","t5","t6"),"impossible",`Mlstr_harmo::status`))})


try({record_total = record_total %>%
  mutate(
    baseline_yr	   = recode(baseline_yr,"0"="2007"),
    followup1_yr	 = recode(followup1_yr,"1"="2011")) %>%
  # 
  mutate(
  #   baseline_yr	   = rep(2007) %>% as.character,
  #   followup1_yr	 = rep(2011) %>% as.character,
    t1	 = rep(2011 - 2007) %>% as.character) %>%
  select(-one_of(var_not_in_dd(record_total, dd_record$Variables)))})
try({dd_record$Variables <- dd_record$Variables %>%
  mutate(
    script = 
      ifelse(name %in% c("followup2_yr","followup3_yr","followup4_yr","followup5_yr","followup6_yr","t2","t3","t4","t5","t6"),NA,script),
    `Mlstr_harmo::status` = 
      ifelse(name %in% c("followup2_yr","followup3_yr","followup4_yr","followup5_yr","followup6_yr","t2","t3","t4","t5","t6"),"impossible",`Mlstr_harmo::status`))})


message("    [4.4]: add the index at the end of the tibble")

dd_clsa_cop$Variables %<>% add_column(`index` = 1:nrow(dd_clsa_cop$Variables))
dd_clsa_tra$Variables %<>% add_column(`index` = 1:nrow(dd_clsa_tra$Variables))
dd_globe$Variables %<>% add_column(`index` = 1:nrow(dd_globe$Variables))
dd_hapiee_cz$Variables %<>% add_column(`index` = 1:nrow(dd_hapiee_cz$Variables))
dd_hapiee_lt$Variables %<>% add_column(`index` = 1:nrow(dd_hapiee_lt$Variables))
dd_hapiee_ru$Variables %<>% add_column(`index` = 1:nrow(dd_hapiee_ru$Variables))
dd_hunt$Variables %<>% add_column(`index` = 1:nrow(dd_hunt$Variables))
dd_lasa1$Variables %<>% add_column(`index` = 1:nrow(dd_lasa1$Variables))
dd_lasa2$Variables %<>% add_column(`index` = 1:nrow(dd_lasa2$Variables))
dd_lucas$Variables %<>% add_column(`index` = 1:nrow(dd_lucas$Variables))
dd_record$Variables %<>% add_column(`index` = 1:nrow(dd_record$Variables))


save.image(file="src/2_all_final_data.RData")
message("    [4.5]: all data munged are saved in src/2_")

##################################################################################################
##################################################################################################
######################           Unique IDs in Harmonized DS           ########################### 
##################################################################################################
##################################################################################################

#Since IDs were not unique across studies, to make each ID unique, study names are added at the beginning of the IDs


message("    [4.6]: assign new id which is unique across all datasets")

try({clsa_cop_total %<>% mutate(study= "CLSA_COP_") %>%  
    mutate(
      new_id = paste0(study,id)) %>% 
    select(new_id,everything(),-id, -study) %>% 
    rename(.,id=new_id)})
try({clsa_tra_total %<>% mutate(study= "CLSA_TRA_") %>%  
    mutate(
      new_id = paste0(study,id)) %>% 
    select(new_id,everything(),-id, -study) %>% 
    rename(.,id=new_id)})
 
try({globe_total %<>% mutate(study= "GLOBE_") %>%  
  mutate(
    new_id = paste0(study,id)) %>% 
  select(new_id,everything(),-id, -study) %>% 
  rename(.,id=new_id)})
try({hapiee_cz_total %<>% mutate(study= "HAPIEE_CZ_") %>%
  mutate(
    new_id = paste0(study,id)) %>% 
  select(new_id,everything(),-id, -study) %>% 
  rename(.,id=new_id)})
try({hapiee_lt_total %<>% mutate(study= "HAPIEE_LT_") %>%  
  mutate(
    new_id = paste0(study,id)) %>% 
  select(new_id,everything(),-id, -study) %>% 
  rename(.,id=new_id)})
try({hapiee_ru_total %<>% mutate(study= "HAPIEE_RU_") %>%  
  mutate(
    new_id = paste0(study,id)) %>% 
  select(new_id,everything(),-id, -study) %>% 
  rename(.,id=new_id)})
try({hunt_total %<>% mutate(study= "HUNT_") %>%  
  mutate(
    new_id = paste0(study,id)) %>% 
  select(new_id,everything(),-id, -study) %>% 
  rename(.,id=new_id)})
try({lasa1_total %<>% mutate(study= "LASA1_") %>%  
  mutate(
    new_id = paste0(study,id)) %>% 
  select(new_id,everything(),-id, -study) %>% 
  rename(.,id=new_id)})
try({lasa2_total %<>% mutate(study= "LASA2_") %>%  
  mutate(
    new_id = paste0(study,id)) %>% 
  select(new_id,everything(),-id, -study) %>% 
  rename(.,id=new_id)})
try({lucas_total %<>% mutate(study= "LUCAS_") %>%  
  mutate(
    new_id = paste0(study,id)) %>% 
  select(new_id,everything(),-id, -study) %>% 
  rename(.,id=new_id)})
try({record_total %<>% mutate(study= "RECORD_") %>%  
  mutate(
    new_id = paste0(study,id)) %>% 
  select(new_id,everything(),-id, -study) %>% 
  rename(.,id=new_id)})


save(list = ls(pattern = "_total"), file = "src/3_all_data_new_id.RData")
message("    [4.7]: all data munged are saved in src/3_")


message("[4 - end]: all data have been munged")

