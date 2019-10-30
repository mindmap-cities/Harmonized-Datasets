library(naniar)
library(epiDisplay)
library(tidyverse)
library(stringr)
library(lubridate)

filter <- dplyr::filter
select <- dplyr::select
recode <- dplyr::recode

#################   FUNCTIONS    ###############################################
#################                ###############################################

ksource <- function(x, ...) {
  library(knitr)
  source(purl(x, output = tempfile()), ...)}

parceval <- function(text){
  eval(parse(text =text))}

MyMerge <- function(x,y){
  df <- merge(x,y, by="id",all=TRUE)
  return(df)}

join_data <- function(pattrn){
  
  domain_short <- c(
    'sdc_',
    'lsb_',
    'oth_',
    'bio_',
    'mho_',
    'soc_',
    'env_',
    'socenv_',
    'physenv_')
  
  listed <- tibble(nvar = ls(pattern = pattrn, envir = .GlobalEnv))
  
  listed <- listed %>% filter(str_detect(nvar, "_0$")) %>%
    bind_rows(listed %>% filter(str_detect(nvar, "_1$"))) %>%
    bind_rows(listed %>% filter(str_detect(nvar, "_2$"))) %>%
    bind_rows(listed %>% filter(str_detect(nvar, "_3$"))) %>%
    bind_rows(listed %>% filter(str_detect(nvar, "_4$"))) %>%
    bind_rows(listed %>% filter(str_detect(nvar, "_5$"))) %>%
    bind_rows(listed %>% filter(str_detect(nvar, "_6$")))
  
  listed <- listed %>% filter(str_detect(nvar, domain_short[1])) %>%
    bind_rows(listed %>% filter(str_detect(nvar, paste0("^",domain_short[2])))) %>%
    bind_rows(listed %>% filter(str_detect(nvar, paste0("^",domain_short[3])))) %>%
    bind_rows(listed %>% filter(str_detect(nvar, paste0("^",domain_short[4])))) %>%
    bind_rows(listed %>% filter(str_detect(nvar, paste0("^",domain_short[5])))) %>%
    bind_rows(listed %>% filter(str_detect(nvar, paste0("^",domain_short[6])))) %>%
    bind_rows(listed %>% filter(str_detect(nvar, paste0("^",domain_short[7])))) %>%
    bind_rows(listed %>% filter(str_detect(nvar, paste0("^",domain_short[8])))) %>%
    bind_rows(listed %>% filter(str_detect(nvar, paste0("^",domain_short[9]))))
  
  jointure <- parceval(listed[1,1]) %>% 
    as_tibble() %>%  
    mutate_at("id",funs(as.character))
  for(i in 2:nrow(listed)){
    print(paste0(pattrn,": ",i,"/",nrow(listed)))
    jointure <- full_join(jointure ,
                          parceval(listed[i,1]) %>% 
                            as_tibble() %>%  
                            mutate_at("id",funs(as.character)), 
                          by = "id")}
  return(jointure)}

change_class <- function(tbl, dd){
#   tbl <- globe_total
#   dd <- dd_globe
  tbl <- tbl 
  class_dd <- dd$Variables %>% select(name,valueType) %>% group_split(valueType)
  names(class_dd) = c(
    class_dd[[1]]$valueType %>% unique,
    class_dd[[2]]$valueType %>% unique,
    class_dd[[3]]$valueType %>% unique)
  
  # get classes :
  message("before: \n")
  print(tbl %>% 
    dplyr::summarise_all(class) %>% 
    tidyr::gather(variable, class) %>% select(class) %>% unique)
  
  tbl <- tbl %>%
    modify_at(class_dd$text$name, as.character) %>%    
    modify_at(class_dd$integer$name, as.integer) %>%
    modify_at(class_dd$decimal$name, as.numeric)
  
  # get classes :
  message("after: \n")
  print(tbl %>% 
    dplyr::summarise_all(class) %>% 
    tidyr::gather(variable, class) %>% select(class) %>% unique)
    return(tbl)
  }

var_not_in_dd <- function(tbl,dd){
  left <- tibble(name = tbl %>% names)
  right <- dd %>% select(name)
  test <- anti_join(left, right, by = "name") %>% filter(name != "id") %>% .[[1]]
  return(test)}




#################     OBJECTS    ###############################################
#################                ###############################################




{
path_list = c(

### BIO ### .
# NOT READY YET 
#    "../biomarkers_genetics/BIO_DS_LASA1.Rmd",
#   "../biomarkers_genetics/BIO_DS_LASA2.Rmd", 

### MHO ### 
    "../mental_health_outcomes/MHO_DS_GLOBE.Rmd", 
    "../mental_health_outcomes/MHO_DS_HAPIEE_CZ.Rmd",
    "../mental_health_outcomes/MHO_DS_HAPIEE_LT.Rmd",
    "../mental_health_outcomes/MHO_DS_HAPIEE_RU.Rmd",
    "../mental_health_outcomes/MHO_DS_HUNT.Rmd",
    "../mental_health_outcomes/MHO_DS_LASA1.Rmd",
    "../mental_health_outcomes/MHO_DS_LASA2.Rmd",
    "../mental_health_outcomes/MHO_DS_LUCAS.Rmd",
    "../mental_health_outcomes/MHO_DS_RECORD.Rmd",

    
### SDC ### 
    # NOT READY YET "../sociodem_characteristics/SDC_DS_CLSA.Rmd",
    "../sociodem_characteristics/SDC_DS_GLOBE.Rmd",
    "../sociodem_characteristics/SDC_DS_HAPIEE_CZ.Rmd",  
    "../sociodem_characteristics/SDC_DS_HAPIEE_LT.Rmd",
    "../sociodem_characteristics/SDC_DS_HAPIEE_RU.Rmd",
    "../sociodem_characteristics/SDC_DS_HUNT.Rmd",
    "../sociodem_characteristics/SDC_DS_LASA1.Rmd",
    "../sociodem_characteristics/SDC_DS_LASA2.Rmd", 
    "../sociodem_characteristics/SDC_DS_LUCAS.Rmd",
    "../sociodem_characteristics/SDC_DS_RECORD.Rmd",

### OTH ### 
    # NOT READY YET  "../other_outcomes/OTH_DS_CLSA.Rmd",  
    "../other_outcomes/OTH_DS_GLOBE.Rmd",  
    "../other_outcomes/OTH_DS_HAPIEE_CZ.Rmd",        
    "../other_outcomes/OTH_DS_HAPIEE_LT.Rmd",       
    "../other_outcomes/OTH_DS_HAPIEE_RU.Rmd",      
    "../other_outcomes/OTH_DS_HUNT.Rmd",     
    "../other_outcomes/OTH_DS_LASA1.Rmd",    
    "../other_outcomes/OTH_DS_LASA2.Rmd",   
    "../other_outcomes/OTH_DS_LUCAS.Rmd",  
    "../other_outcomes/OTH_DS_RECORD.Rmd",

### ENV ### 
    "../perceptions_urban_env/ENV_DS_GLOBE.Rmd",
    "../perceptions_urban_env/ENV_DS_HAPIEE_CZ.Rmd",
    "../perceptions_urban_env/ENV_DS_HAPIEE_LT.Rmd",
    "../perceptions_urban_env/ENV_DS_HAPIEE_RU.Rmd",
    "../perceptions_urban_env/ENV_DS_HUNT.Rmd",
    "../perceptions_urban_env/ENV_DS_LASA1.Rmd",
    "../perceptions_urban_env/ENV_DS_LASA2.Rmd",
    "../perceptions_urban_env/ENV_DS_LUCAS.Rmd",
    "../perceptions_urban_env/ENV_DS_RECORD.Rmd",
    
### SOCENV ###    
    #NOT READY YET "../social_environmental/SOCENV_DS_GLOBE.Rmd",
    #NOT READY YET "../social_environmental/SOCENV_DS_HAPIEE_CZ.Rmd",
    #NOT READY YET "../social_environmental/SOCENV_DS_HAPIEE_LT.Rmd",
    #NOT READY YET "../social_environmental/SOCENV_DS_HAPIEE_RU.Rmd",
    #NOT READY YET "../social_environmental/SOCENV_DS_HUNT.Rmd",
    #NOT READY YET "../social_environmental/SOCENV_DS_LASA1.Rmd",
    #NOT READY YET "../social_environmental/SOCENV_DS_LASA2.Rmd",
    #NOT READY YET "../social_environmental/SOCENV_DS_LUCAS.Rmd",
    #NOT READY YET "../social_environmental/SOCENV_DS_RECORD.Rmd",
    
    
### LSB ###
    #NOT READY YET "../lifestyle_behaviours/LSB_DS_CLSA.Rmd",
    "../lifestyle_behaviours/LSB_DS_GLOBE.Rmd",
    "../lifestyle_behaviours/LSB_DS_HAPIEE_CZ.Rmd",
    "../lifestyle_behaviours/LSB_DS_HAPIEE_LT.Rmd",
    "../lifestyle_behaviours/LSB_DS_HAPIEE_RU.Rmd",
    "../lifestyle_behaviours/LSB_DS_HUNT.Rmd",
    "../lifestyle_behaviours/LSB_DS_LASA1.Rmd",
    "../lifestyle_behaviours/LSB_DS_LASA2.Rmd",
    "../lifestyle_behaviours/LSB_DS_LUCAS.Rmd",
    "../lifestyle_behaviours/LSB_DS_RECORD.Rmd",

### PHYSENV ###
    # special file to do so

### SOC ### 
   "../social_factors/SOC_DS_GLOBE.Rmd",
   "../social_factors/SOC_DS_HAPIEE_CZ.Rmd",
   "../social_factors/SOC_DS_HAPIEE_LT.Rmd",
   "../social_factors/SOC_DS_HAPIEE_RU.Rmd",
   "../social_factors/SOC_DS_HUNT.Rmd",
    #NOT READY YET "../social_factors/SOC_DS_LASA1.Rmd",
    #NOT READY YET "../social_factors/SOC_DS_LASA2.Rmd",
   "../social_factors/SOC_DS_LUCAS.Rmd",
   "../social_factors/SOC_DS_RECORD.Rmd"
 )
}



###############################       PROCESS        ###########################
################################################################################



#   for (i in 1:length(path_list)) {
#     try(ksource(path_list[i]))}
#   source("Recoding data in R_physenv.r")
#   }
# save.image(file="all_data_from_sourcing_0.RData")
# load("all_data_from_sourcing_0.RData")
 
rm(erasmus_opal, smk_table, MyMerge2, med_table,
   GLOBE1991,
    GLOBE1997,GLOBE2004,GLOBE2011,GLOBE2014,
   HAPIEE_postal2009,
    HAPIEE_postal2012,HAPIEE_postal2013,
    HAPIEE_postal2015,HAPIEE_postal201718,
    HAPIEE_W1,HAPIEE_W1_hl,HAPIEE_W2,HAPIEE_W2_hl,
    HAPIEE_Corine_0,HAPIEE_Corine_1,HAPIEE_UA_1,
   HUNT1,HUNT2,HUNT3,
    med_table_hunt,
    table_hunt_all,
   LASA_extravar_PA, LASA1B,LASA1C,LASA1D,
    LASA1E,LASA1F,LASA1G,LASA1H,med_table_lasa1, lasa1_lsb_table,
    LASA2B,LASA2F,LASA2G,LASA2H,med_table_lasa2, lasa2_lsb_table,
   LUCAS_Wave2, lucas_w2,
   RECORD_w1_1, RECORD_w1_2, RECORD_w2,
    RECORD_W2_2,  alc_table_record, med_table_record,
   physenv_GLOBE_1997, physenv_GLOBE_2004, physenv_GLOBE_2011,
   physenv_GLOBE_2014, physenv_HUNT3_2000, physenv_HUNT3_2006,
   physenv_HUNT3_2012, physenv_LASA2_2006, physenv_LASA2_2012,
   physenv_RECORD_2006, physenv_RECORD_2012, 
   physenv_LASA1_2006, physenv_LASA1_2012
   )
# 
# 


# save.image(file="all_dom_data_1.RData")
# load("all_dom_data_1.RData")


try({clsa_total <- join_data("CLSA"); base::rm(list=ls(pattern = "CLSA", envir = .GlobalEnv))})
try({globe_total <- join_data("GLOBE"); base::rm(list=ls(pattern = "GLOBE", envir = .GlobalEnv))})
try({hapiee_cz_total <- join_data("HAPIEE_CZ"); base::rm(list=ls(pattern = "HAPIEE_CZ", envir = .GlobalEnv))})
try({hapiee_lt_total <- join_data("HAPIEE_LT"); base::rm(list=ls(pattern = "HAPIEE_LT", envir = .GlobalEnv))})
try({hapiee_ru_total <- join_data("HAPIEE_RU"); base::rm(list=ls(pattern = "HAPIEE_RU", envir = .GlobalEnv))})
try({hunt_total <- join_data("HUNT"); base::rm(list=ls(pattern = "HUNT", envir = .GlobalEnv))})
try({lasa1_total <- join_data("LASA1"); base::rm(list=ls(pattern = "LASA1", envir = .GlobalEnv))})
try({lasa2_total <- join_data("LASA2"); base::rm(list=ls(pattern = "LASA2", envir = .GlobalEnv))})
try({lucas_total <- join_data("LUCAS"); base::rm(list=ls(pattern = "LUCAS", envir = .GlobalEnv))})
try({record_total <- join_data("RECORD"); base::rm(list=ls(pattern = "RECORD", envir = .GlobalEnv))})


# save.image(file="all_total_data_2.RData")
#   load("all_total_data_2.RData")


source("Get_data_dictionnary.R")

# clsa_total <- change_class(clsa_total,clsa_globe)
globe_total <- change_class(globe_total,dd_globe)
hapiee_lt_total <- change_class(hapiee_lt_total,dd_hapiee_lt)
hapiee_cz_total <- change_class(hapiee_cz_total,dd_hapiee_cz)
hapiee_ru_total <- change_class(hapiee_ru_total,dd_hapiee_ru)
lasa1_total <- change_class(lasa1_total,dd_lasa1)
lasa2_total <- change_class(lasa2_total,dd_lasa2)
lucas_total <- change_class(lucas_total,dd_lucas)
hunt_total <- change_class(hunt_total,dd_hunt)
record_total <- change_class(record_total,dd_record)

# test var not in DD
var_not_in_dd(globe_total, dd_globe$Variables) #physenv_cn_bf_facil300_1
var_not_in_dd(hapiee_cz_total, dd_hapiee_cz$Variables)
var_not_in_dd(hapiee_lt_total, dd_hapiee_lt$Variables)
var_not_in_dd(hapiee_ru_total, dd_hapiee_ru$Variables)
var_not_in_dd(hunt_total, dd_hunt$Variables) #physenv_cn_bf_lu3000_facil_2
var_not_in_dd(lasa1_total, dd_lasa1$Variables)
var_not_in_dd(lasa2_total, dd_lasa2$Variables)
var_not_in_dd(lucas_total, dd_lucas$Variables)
var_not_in_dd(record_total, dd_record$Variables)
# "physenv_cn_ne_facil_as_0"     "physenv._cn_ne_facil_0"      
# "physenv._cn_bf_ttbsgr800_0"   "physenv_cn_lu100_agri_0"     
# "physenv_cn_bf_lu100_infra_0"  "physenv._cn_bf_lu100_on_0"   
# "physenv_cn_bf_lu3000_facil_0" "physenv_ua_bf_forests100_0"  
# "physenv_ua_bf_lu3000_facil_0" "physenv_ua_ne_facil_as_0"    
# "physenv_ua_ne_facil_0"        "phenv_ua_ne_water_as_1"      
# "phenv_ua_ne_water_1"          "commune"                     
# "codepostal"                   "pays"                        
# "IN_PARIS"                     "area"


# clsa_total =  clsa_total %>% 
# mutate(
#   baseline_yr	   = rep(2008) %>% as.character,
#   followup1_yr	 = rep(2015) %>% as.character,
#   t1	 = rep(2015 - 2008) %>% as.character)


globe_total = globe_total %>%
  mutate(
    baseline_yr	   = rep(1991) %>% as.character,
    followup1_yr	 = rep(1997) %>% as.character,
    followup2_yr	 = rep(2004) %>% as.character,
    followup3_yr	 = rep(2011) %>% as.character,
    followup4_yr	 = rep(2014) %>% as.character,
    t1	 = rep(1997 - 1991) %>% as.character,
    t2	 = rep(2004 - 1997) %>% as.character,
    t3	 = rep(2011 - 2004) %>% as.character,
    t4	 = rep(2014 - 2011) %>% as.character) %>%
  select(-one_of(var_not_in_dd(globe_total, dd_globe$Variables)))
dd_globe$Variables <- dd_globe$Variables %>%
  mutate(
    script = 
      ifelse(name %in% c("followup5_yr","followup6_yr","t5","t6"),NA,script),
    `Mlstr_harmo::status` = 
      ifelse(name %in% c("followup5_yr","followup6_yr","t5","t6"),"impossible",`Mlstr_harmo::status`))
# a = tibble(name = globe_total %>% select(-id) %>%names)
# b = dd_globe$Variables %>% filter(`Mlstr_harmo::status` == "complete") %>% select(name)
# anti_join(b,a)
# anti_join(a,b)


hapiee_cz_total = hapiee_cz_total %>%
  mutate(
    baseline_yr	   = rep(2002) %>% as.character,
    followup1_yr	 = rep(2006) %>% as.character,
    t1	 = rep(2006 - 2002) %>% as.character) %>%
  select(-one_of(var_not_in_dd(hapiee_cz_total, dd_hapiee_cz$Variables)))
dd_hapiee_cz$Variables <- dd_hapiee_cz$Variables %>%
  mutate(
    script = 
      ifelse(name %in% c("followup2_yr","followup3_yr","followup4_yr","followup5_yr","followup6_yr","t2","t3","t4","t5","t6"),NA,script),
    `Mlstr_harmo::status` = 
      ifelse(name %in% c("followup2_yr","followup3_yr","followup4_yr","followup5_yr","followup6_yr","t2","t3","t4","t5","t6"),"impossible",`Mlstr_harmo::status`))
# a = tibble(name = hapiee_cz_total %>% select(-id) %>%names)
# b = dd_hapiee_cz$Variables %>% filter(`Mlstr_harmo::status` == "complete") %>% select(name)
# anti_join(b,a)
# anti_join(a,b)


hapiee_ru_total = hapiee_ru_total %>%
  mutate(
    baseline_yr	   = rep(2002) %>% as.character,
    followup1_yr	 = rep(2006) %>% as.character,
    t1	 = rep(2006 - 2002) %>% as.character) %>%
  select(-one_of(var_not_in_dd(hapiee_ru_total, dd_hapiee_ru$Variables)))
dd_hapiee_ru$Variables <- dd_hapiee_ru$Variables %>%
  mutate(
    script = 
      ifelse(name %in% c("followup2_yr","followup3_yr","followup4_yr","followup5_yr","followup6_yr","t2","t3","t4","t5","t6"),NA,script),
    `Mlstr_harmo::status` = 
      ifelse(name %in% c("followup2_yr","followup3_yr","followup4_yr","followup5_yr","followup6_yr","t2","t3","t4","t5","t6"),"impossible",`Mlstr_harmo::status`))

# a = tibble(name = hapiee_ru_total %>% select(-id) %>%names)
# b = dd_hapiee_ru$Variables %>% filter(`Mlstr_harmo::status` == "complete") %>% select(name)
# anti_join(b,a)
# anti_join(a,b)

hapiee_lt_total = hapiee_lt_total %>%
  mutate(
    baseline_yr	   = rep(2005) %>% as.character) %>%
  select(-one_of(var_not_in_dd(hapiee_lt_total, dd_hapiee_lt$Variables)))
dd_hapiee_lt$Variables <- dd_hapiee_lt$Variables %>%
  mutate(
    script = 
      ifelse(name %in% c("followup1_yr","followup2_yr","followup3_yr","followup4_yr","followup5_yr","followup6_yr","t1","t2","t3","t4","t5","t6"),NA,script),
    `Mlstr_harmo::status` = 
      ifelse(name %in% c("followup1_yr","followup2_yr","followup3_yr","followup4_yr","followup5_yr","followup6_yr","t1","t2","t3","t4","t5","t6"),"impossible",`Mlstr_harmo::status`))

# a = tibble(name = hapiee_lt_total %>% select(-id) %>%names)
# b = dd_hapiee_lt$Variables %>% filter(`Mlstr_harmo::status` == "complete") %>% select(name)
# anti_join(b,a)
# anti_join(a,b)

hunt_total = hunt_total %>%
  mutate(
    baseline_yr	   = rep(1984) %>% as.character,
    followup1_yr	 = rep(1995) %>% as.character,
    followup2_yr	 = rep(2006) %>% as.character,
    t1	 = rep(1995 - 1984) %>% as.character,
    t2	 = rep(2006 - 1995) %>% as.character) %>%
  select(-one_of(var_not_in_dd(hunt_total, dd_hunt$Variables)))
dd_hunt$Variables <- dd_hunt$Variables %>%
  mutate(
    script = 
      ifelse(name %in% c("followup3_yr","followup4_yr","followup5_yr","followup6_yr","t3","t4","t5","t6"),NA,script),
    `Mlstr_harmo::status` = 
      ifelse(name %in%  c("followup3_yr","followup4_yr","followup5_yr","followup6_yr","t3","t4","t5","t6"),"impossible",`Mlstr_harmo::status`))

# a = tibble(name = hunt_total %>% select(-id) %>%names)
# b = dd_hunt$Variables %>% filter(`Mlstr_harmo::status` == "complete") %>% select(name)
# anti_join(b,a)
# anti_join(a,b)

lasa1_total = lasa1_total %>%
  mutate(
    baseline_yr	   = rep(1992) %>% as.character,
    followup1_yr	 = rep(1995) %>% as.character,
    followup2_yr	 = rep(1998) %>% as.character,
    followup3_yr	 = rep(2001) %>% as.character,
    followup4_yr	 = rep(2005) %>% as.character,
    followup5_yr	 = rep(2008) %>% as.character,
    followup6_yr	 = rep(2011) %>% as.character,
    t1	 = rep(1995-1992) %>% as.character,
    t2	 = rep(1998-1995) %>% as.character,
    t3	 = rep(2001-1998) %>% as.character,
    t4	 = rep(2005-2001) %>% as.character,
    t5	 = rep(2008-2005) %>% as.character,
    t6	 = rep(2011-2008) %>% as.character) %>%
  select(-one_of(var_not_in_dd(lasa1_total, dd_lasa1$Variables)))
dd_lasa1$Variables <- dd_lasa1$Variables 

# a = tibble(name = lasa1_total %>% select(-id) %>%names)
# b = dd_lasa1$Variables %>% filter(`Mlstr_harmo::status` == "complete") %>% select(name)
# anti_join(b,a)
# anti_join(a,b)

lasa2_total = lasa2_total %>%
  mutate(
    baseline_yr	   = rep(2002) %>% as.character,
    followup1_yr	 = rep(2005) %>% as.character,
    followup2_yr	 = rep(2008) %>% as.character,
    followup3_yr	 = rep(2011) %>% as.character,
    t1	 = rep(2005 - 2002) %>% as.character,
    t2	 = rep(2008 - 2005) %>% as.character,
    t3	 = rep(2011 - 2008) %>% as.character) %>%
  select(- one_of(var_not_in_dd(lasa2_total, dd_lasa2$Variables)))
dd_lasa2$Variables <- dd_lasa2$Variables %>%
  mutate(
    script = 
      ifelse(name %in% c("followup4_yr","followup5_yr","followup6_yr","t4","t5","t6"),NA,script),
    `Mlstr_harmo::status` = 
      ifelse(name %in% c("followup4_yr","followup5_yr","followup6_yr","t4","t5","t6"),"impossible",`Mlstr_harmo::status`))

# a = tibble(name = lasa2_total %>% select(-id) %>%names)
# b = dd_lasa2$Variables %>% filter(`Mlstr_harmo::status` == "complete") %>% select(name)
# anti_join(b,a)
# anti_join(a,b)

lucas_total = lucas_total %>%
  mutate(
    baseline_yr	   = rep(2000) %>% as.character) %>%
  select(-one_of(var_not_in_dd(lucas_total, dd_lucas$Variables)))
dd_lucas$Variables <- dd_lucas$Variables %>%
  mutate(
    script = 
      ifelse(name %in% c("followup1_yr","followup2_yr","followup3_yr","followup4_yr","followup5_yr","followup6_yr","t1","t2","t3","t4","t5","t6"),NA,script),
    `Mlstr_harmo::status` = 
      ifelse(name %in% c("followup1_yr","followup2_yr","followup3_yr","followup4_yr","followup5_yr","followup6_yr","t1","t2","t3","t4","t5","t6"),"impossible",`Mlstr_harmo::status`))

# a = tibble(name = lucas_total %>% select(-id) %>%names)
# b = dd_lucas$Variables %>% filter(`Mlstr_harmo::status` == "complete") %>% select(name)
# anti_join(b,a)
# anti_join(a,b)



record_total = record_total %>%
  mutate(
    baseline_yr	   = rep(2007) %>% as.character,
    followup1_yr	 = rep(2011) %>% as.character,
    t1	 = rep(2011 - 2007) %>% as.character) %>%
  select(-one_of(var_not_in_dd(record_total, dd_record$Variables)))
dd_record$Variables <- dd_record$Variables %>%
  mutate(
    script = 
      ifelse(name %in% c("followup2_yr","followup3_yr","followup4_yr","followup5_yr","followup6_yr","t2","t3","t4","t5","t6"),NA,script),
    `Mlstr_harmo::status` = 
      ifelse(name %in% c("followup2_yr","followup3_yr","followup4_yr","followup5_yr","followup6_yr","t2","t3","t4","t5","t6"),"impossible",`Mlstr_harmo::status`))

# a = tibble(name = record_total %>% select(-id) %>%names)
# b = dd_record$Variables %>% filter(`Mlstr_harmo::status` == "complete") %>% select(name)
# anti_join(b,a)
# anti_join(a,b)


save.image(file="all_total_3.RData")
# 
# answer_TotalData = menu(c("Yes", "No"), title="Do you want to load all_total.Rdata?")
# if(answer_TotalData == 1){
#   load("all_total_3.Rdata")
# }


names_short = c(
  #'clsa',
  'globe',
  'hapiee_cz',
  'hapiee_lt',
  'hapiee_ru',
  'hunt',
  'lasa1',
  'lasa2',
  'lucas',
  'record')

names_opal_proj = c(
  # 'CLSA',
  'GLOBE',
  'HAPIEE',
  'HAPIEE',
  'HAPIEE',
  'HUNT',
  'LASA',
  'LASA',
  'LUCAS',
  'RECORD')


# csv creation
for(i in 1:length(names_short)){
  try(write_csv(
    parceval(paste0(names_short[i],"_total")),
    paste0("csv_files/",names_short[i],"_Harmo_Table_",str_replace_all(today(),"-",""),".csv"),
    col_names = TRUE, na=""))
}


rm(answer_sourceData,i,
   path_clsa,path_globe, path_env, path_hapiee_cz, 
   path_hapiee_lt, path_hapiee_ru, path_hunt, path_lasa1,
   path_lasa2, path_lucas,path_record, nbDomains)




# Opal upload files
library(opalr)
erasmus_opal = opal.login()

for(i in 1:length(names_short)){
  try(opal.file_upload(erasmus_opal,
    paste0("csv_files/",names_short[i],"_Harmo_Table_",
           str_replace_all(today(),"-",""),".csv"),
    paste0("/projects/",names_opal_proj[i])))
}


# TRY 
# csv creation
for(i in 1:length(name_tbl)){
  try(write_csv(
    parceval(name_tbl[i]),
    paste0("csv_files/",name_tbl[i],"_Harmo_Table_",str_replace_all(today(),"-",""),".csv"),
    col_names = TRUE, na=""))}

save_xls(dd_globe$Variables     , "xls_files/", dd_globe$Categories     , 'GLOBE' )
save_xls(dd_hapiee_cz$Variables , "xls_files/", dd_hapiee_cz$Categories , 'HAPIEE_CZ' )
save_xls(dd_hapiee_lt$Variables , "xls_files/", dd_hapiee_lt$Categories , 'HAPIEE_LT' )
save_xls(dd_hapiee_ru$Variables , "xls_files/", dd_hapiee_ru$Categories , 'HAPIEE_RU' )
save_xls(dd_hunt$Variables      , "xls_files/", dd_hunt$Categories      , 'HUNT' )
save_xls(dd_lasa1$Variables     , "xls_files/", dd_lasa1$Categories     , 'LASA1' ) 
save_xls(dd_lasa2$Variables     , "xls_files/", dd_lasa2$Categories     , 'LASA2' )
save_xls(dd_lucas$Variables     , "xls_files/", dd_lucas$Categories     , 'LUCAS' )
save_xls(dd_record$Variables    , "xls_files/", dd_record$Categories    , 'RECORD' )




# try with create table opal
#try(opal.file_upload(erasmus_opal,"DD_HUNT.xlsx",paste0("/projects/",names_opal_proj[5])))


hunt_total %>% select(baseline_yr)
dd_hunt$Variables %>% filter(name =="baseline_yr") 


message("hunt has multiple id entry")

# path_file = list()
# 
# for (i in 1:nbDomains){
#   path_file[[i]] = list.files(path = paste0("../",list_domain[i]), pattern = "*.Rmd", all.files = FALSE,
#                               full.names = TRUE, recursive = TRUE,
#                               ignore.case = FALSE, include.dirs = FALSE )
#   path_file[[i]] = path_file[[i]][!str_detect(string=path_file[[i]],pattern="validation|Validation")]
#   path_file[[i]] = path_file[[i]][!str_detect(string=path_file[[i]],pattern="DS.Rmd")]
#   path_file[[i]] = path_file[[i]][!str_detect(string=path_file[[i]],pattern="PHYSENV_DS")]
#   
# }
# 
# 
# 
# names(path_file) = list_domain
# 
# path_list = path_file %>% unlist
# list_domain = c(
#' 'sociodem_characteristics',
#' 'lifestyle_behaviours',
#' 'other_outcomes',
#' # 'biomarkers_genetics',   
#' 'mental_health_outcomes',
#' 'social_factors',
#' 'perceptions_urban_env',
#' #'social_environmental',
#' 'physical_environmental')
#' nbDomains = length(list_domain)



