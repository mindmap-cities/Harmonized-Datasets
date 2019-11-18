library("devtools")
devtools::install_local("harmor-master", dependencies = TRUE, force=TRUE)
library(harmor)
library(naniar)
library(epiDisplay)
library(tidyverse)
library(stringr)
library(lubridate)
library(opalr)
library(scales)
library(car)
library(sjmisc)
library(magrittr)


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

change_class <- function(tbl, dd, name_stdy){
  # tbl <- globe_total
  # dd <- dd_globe
  # name_stdy <- "GLOBE"
  class_dd <- dd$Variables %>% select(variable = name, class_dd = valueType)
  class_tbl <- tbl %>% 
    dplyr::summarise_all(typeof) %>% 
    tidyr::gather(variable, class_tbl) %>% select(variable, class_tbl)
  
  # test_same_class   
  odd_classes <- inner_join(class_tbl,class_dd) %>%
    # filter(class_tbl != class_dd) %>%
    # filter(!(class_tbl == 'numeric' & class_dd == 'decimal')) %>% 
    # filter(!(class_tbl == 'integer' & class_dd == 'decimal')) %>% 
    add_column(study = rep(name_stdy))
  write_csv(odd_classes, paste0("csv_files/odd_",name_stdy,".csv"))
  
  class_dd <- dd$Variables %>% select(name,valueType) %>% group_split(valueType)
  names(class_dd) = c(
    class_dd[[1]]$valueType %>% unique,
    class_dd[[2]]$valueType %>% unique,
    class_dd[[3]]$valueType %>% unique)
  
  # get classes :
  message("before: \n")
  print(tbl %>% 
          dplyr::summarise_all(typeof) %>% 
          tidyr::gather(variable, class) %>% select(class) %>% select(class) %>%
          group_by(class) %>% summarise(count = n()))
  
  tbl <- tbl %>%
    modify_at(class_dd$text$name, as.character) %>%    
    modify_at(class_dd$integer$name, as.integer) %>%
    modify_at(class_dd$decimal$name, as.numeric)
  
  # get classes :
  message("after: \n")
  print(tbl %>% 
          dplyr::summarise_all(typeof) %>% 
          tidyr::gather(variable, class) %>% select(class) %>%
          group_by(class) %>% summarise(count = n()))
  return(tbl)}

var_not_in_dd <- function(tbl,dd){
  left <- tibble(name = tbl %>% names)
  right <- dd %>% select(name)
  test <- anti_join(left, right, by = "name") %>% filter(name != "id") %>% .[[1]]
  return(test)}



#################     OBJECTS    ###############################################
#################                ###############################################



{
  path_list = c(
    
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
    
    ### SOC ### 
    "../social_factors/SOC_DS_GLOBE.Rmd",
    "../social_factors/SOC_DS_HAPIEE_CZ.Rmd",
    "../social_factors/SOC_DS_HAPIEE_LT.Rmd",
    "../social_factors/SOC_DS_HAPIEE_RU.Rmd",
    "../social_factors/SOC_DS_HUNT.Rmd",
    #NOT READY YET "../social_factors/SOC_DS_LASA1.Rmd",
    #NOT READY YET "../social_factors/SOC_DS_LASA2.Rmd",
    "../social_factors/SOC_DS_LUCAS.Rmd",
    "../social_factors/SOC_DS_RECORD.Rmd",
    
    ### ENV ### 
    "../perceptions_urban_env/ENV_DS_GLOBE.Rmd",
    "../perceptions_urban_env/ENV_DS_HAPIEE_CZ.Rmd",
    "../perceptions_urban_env/ENV_DS_HAPIEE_LT.Rmd",
    "../perceptions_urban_env/ENV_DS_HAPIEE_RU.Rmd",
    "../perceptions_urban_env/ENV_DS_HUNT.Rmd",
    "../perceptions_urban_env/ENV_DS_LASA1.Rmd",
    "../perceptions_urban_env/ENV_DS_LASA2.Rmd",
    "../perceptions_urban_env/ENV_DS_LUCAS.Rmd",
    "../perceptions_urban_env/ENV_DS_RECORD.Rmd"
    
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
    
    ### PHYSENV ###
    # special file to do so
  )
}



###############################       PROCESS        ###########################
################################################################################



for (i in 1:length(path_list)) {
  try(ksource(path_list[i]))}
source("Recoding data in R_physenv.r")

if(!file.exists("rdata_files/")){dir.create("rdata_files")}
save.image(file="rdata_files/0_all_data_from_sourcing.RData")
load("rdata_files/0_all_data_from_sourcing.RData")
data_to_save <- c(
  ls(pattern = "^[sdc_|_|lsb_|oth_|bio_|mho_|soc_|env_|socenv_|physenv_].*_[0|1-6]$"),
  "join_data",
  "change_class",
  "var_not_in_dd",
  "path_list",
  "filter",
  "ksource",
  "MyMerge",
  "parceval",
  "recode",
  "select")
if(!file.exists("rdata_files/")){dir.create("rdata_files")}
save(list = data_to_save, file="rdata_files/1_all_dom_data.RData")
rm(list = ls())
load("rdata_files/1_all_dom_data.RData")

# try({clsa_total <- join_data("CLSA"); base::rm(list=ls(pattern = "CLSA", envir = .GlobalEnv))})
try({globe_total <- join_data("GLOBE"); base::rm(list=ls(pattern = "GLOBE", envir = .GlobalEnv))})
try({hapiee_cz_total <- join_data("HAPIEE_CZ"); base::rm(list=ls(pattern = "HAPIEE_CZ", envir = .GlobalEnv))})
try({hapiee_lt_total <- join_data("HAPIEE_LT"); base::rm(list=ls(pattern = "HAPIEE_LT", envir = .GlobalEnv))})
try({hapiee_ru_total <- join_data("HAPIEE_RU"); base::rm(list=ls(pattern = "HAPIEE_RU", envir = .GlobalEnv))})
try({hunt_total <- join_data("HUNT"); base::rm(list=ls(pattern = "HUNT", envir = .GlobalEnv))})
try({lasa1_total <- join_data("LASA1"); base::rm(list=ls(pattern = "LASA1", envir = .GlobalEnv))})
try({lasa2_total <- join_data("LASA2"); base::rm(list=ls(pattern = "LASA2", envir = .GlobalEnv))})
try({lucas_total <- join_data("LUCAS"); base::rm(list=ls(pattern = "LUCAS", envir = .GlobalEnv))})
try({record_total <- join_data("RECORD"); base::rm(list=ls(pattern = "RECORD", envir = .GlobalEnv))})

source("Get_data_dictionnary.R")

rm(list = ls(pattern = "^path_"))
if(!file.exists("rdata_files/")){dir.create("rdata_files")}
save.image(file="rdata_files/2_all_total_data.RData")
load("rdata_files/2_all_total_data.RData")

# clsa_total <- change_class(clsa_total,clsa_globe)
globe_total     <- change_class(globe_total,dd_globe, "GLOBE")
hapiee_lt_total <- change_class(hapiee_lt_total,dd_hapiee_lt, "HAPIEE_LT")
hapiee_cz_total <- change_class(hapiee_cz_total,dd_hapiee_cz, "HAPIEE_CZ")
hapiee_ru_total <- change_class(hapiee_ru_total,dd_hapiee_ru, "HAPIEE_RU")
lasa1_total     <- change_class(lasa1_total,dd_lasa1, "LASA1")
lasa2_total     <- change_class(lasa2_total,dd_lasa2, "LASA2")
lucas_total     <- change_class(lucas_total,dd_lucas, "LUCAS")
hunt_total      <- change_class(hunt_total,dd_hunt, "HUNT")
record_total    <- change_class(record_total,dd_record, "RECORD")

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

globe_total %<>% select(-starts_with("physenv_cn_bf_facil3000_"))
hapiee_cz_total %<>% select(-starts_with("physenv_cn_bf_facil3000_"))
hapiee_lt_total %<>% select(-starts_with("physenv_cn_bf_facil3000_"))
hapiee_ru_total %<>% select(-starts_with("physenv_cn_bf_facil3000_"))
hunt_total %<>% select(-starts_with("physenv_cn_bf_facil3000_"))
lasa1_total %<>% select(-starts_with("physenv_cn_bf_facil3000_"))
lasa2_total %<>% select(-starts_with("physenv_cn_bf_facil3000_"))
lucas_total %<>% select(-starts_with("physenv_cn_bf_facil3000_"))
record_total %<>% select(-starts_with("physenv_cn_bf_facil3000_"))

dd_globe$Variables %<>% filter(!str_detect(name,"physenv_cn_bf_facil3000_"))
dd_hapiee_cz$Variables %<>% filter(!str_detect(name,"physenv_cn_bf_facil3000_"))
dd_hapiee_lt$Variables %<>% filter(!str_detect(name,"physenv_cn_bf_facil3000_"))
dd_hapiee_ru$Variables %<>% filter(!str_detect(name,"physenv_cn_bf_facil3000_"))
dd_hunt$Variables %<>% filter(!str_detect(name,"physenv_cn_bf_facil3000_"))
dd_lasa1$Variables %<>% filter(!str_detect(name,"physenv_cn_bf_facil3000_"))
dd_lasa2$Variables %<>% filter(!str_detect(name,"physenv_cn_bf_facil3000_"))
dd_lucas$Variables %<>% filter(!str_detect(name,"physenv_cn_bf_facil3000_"))
dd_record$Variables %<>% filter(!str_detect(name,"physenv_cn_bf_facil3000_"))




if(!file.exists("rdata_files/")){dir.create("rdata_files")}
save.image(file="rdata_files/3_all_final_data.RData")
load("rdata_files/3_all_final_data.RData")

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

# 
# for(i in 1:length(names_short)){
#   try(opal.file_upload(erasmus_opal,
#     paste0("csv_files/",names_short[i],"_Harmo_Table_",
#            str_replace_all(today(),"-",""),".csv"),
#     paste0("/projects/",names_opal_proj[i])))}

# a = 0
# a = a + globe_total %>% select(id) %>% unique %>% nrow()
# a = a + hapiee_cz_total %>% select(id) %>% unique %>% nrow()
# a = a + hapiee_lt_total %>% select(id) %>% unique %>% nrow()
# a = a + hapiee_ru_total %>% select(id) %>% unique %>% nrow()
# a = a + hunt_total %>% select(id) %>% unique %>% nrow()
# a = a + lasa1_total %>% select(id) %>% unique %>% nrow()
# a = a + lasa2_total %>% select(id) %>% unique %>% nrow()
# a = a + lucas_total %>% select(id) %>% unique %>% nrow()
# a = a + record_total %>% select(id) %>% unique %>% nrow()
# 
# TRY
# csv creation (to trash?)
# for(i in 1:length(name_tbl)){
#   try(write_csv(
#     parceval(name_tbl[i]),
#     paste0("csv_files/",name_tbl[i],"_Harmo_Table_",str_replace_all(today(),"-",""),".csv"),
#     col_names = TRUE, na=""))}

--------------------------------------------------------------------------------

# dd_globe$Variables %>% group_by(`Mlstr_harmo::status`) %>% summarise(n())
# dd_hapiee_cz$Variables %>% group_by(`Mlstr_harmo::status`) %>% summarise(n())
# dd_hapiee_lt$Variables %>% group_by(`Mlstr_harmo::status`) %>% summarise(n())
# dd_hapiee_ru$Variables %>% group_by(`Mlstr_harmo::status`) %>% summarise(n())
# dd_hunt$Variables %>% group_by(`Mlstr_harmo::status`) %>% summarise(n())
# dd_lasa1$Variables %>% group_by(`Mlstr_harmo::status`) %>% summarise(n())
# dd_lasa2$Variables %>% group_by(`Mlstr_harmo::status`) %>% summarise(n())
# dd_lucas$Variables %>% group_by(`Mlstr_harmo::status`) %>% summarise(n())
# dd_record$Variables %>% group_by(`Mlstr_harmo::status`) %>% summarise(n())
# try with create table opal
# try(opal.file_upload(erasmus_opal,"DD_HUNT.xlsx",paste0("/projects/",names_opal_proj[5])))

#message("[ERR]: globe_total$soc_ss_Zscore_perceived_emo_0, globe_total$soc_ss_Zscore_perceived_emo_0")
#message("[ERR]: c'est quoi :physenv_HUNT3_2000 - physenv_HUNT3_2012")
#message("[ERR]: physenv_RECORD_1$physenv_cn_bf_facil3000_1")


total_release <- readLines("~/Harmonized-Datasets/version_release.info")
last_release <- total_release[length(total_release)]

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


# try(save_tables(erasmus_opal, globe_total,"GLOBE_Harmonized", paste0("globe_DS_", last_release)))
# try(save_tables(erasmus_opal, hapiee_cz_total,dd_hapiee_cz,"HAPIEE_Harmonized", paste0("hapiee_cz_DS_",last_release)))
# try(save_tables(erasmus_opal, hapiee_lt_total,dd_hapiee_lt,"HAPIEE_Harmonized", paste0("hapiee_lt_DS_",last_release)))
# try(save_tables(erasmus_opal, hapiee_ru_total,dd_hapiee_ru,"HAPIEE_Harmonized", paste0("hapiee_ru_DS_",last_release)))
# try(save_tables(erasmus_opal, hunt_total,     dd_hunt,     "HUNT_Harmonized",   paste0("hunt_DS_",     last_release)))
# try(save_tables(erasmus_opal, lasa1_total,    dd_lasa1,    "LASA_Harmonized",   paste0("lasa2_DS_",    last_release)))
# try(save_tables(erasmus_opal, lasa2_total,    dd_lasa2,    "LASA_Harmonized",   paste0("lasa1_DS_",    last_release)))
# try(save_tables(erasmus_opal, lucas_total,    dd_lucas,    "LUCAS_Harmonized",  paste0("lucas_DS_",    last_release)))
# try(save_tables(erasmus_opal, record_total,   dd_record,   "RECORD_Harmonized", paste0("record_DS_",   last_release)))


# hunt_total %>% select not sure what this line is for

# upload to opal step:
devtools::install_local(path = "harmor", dependencies = TRUE, force = TRUE)
library(opalr)
library(harmor)
o <- opal.login()
globe_total     <- applyDictionary(globe_total,     variables=dd_globe$Variables,     categories=dd_globe$Categories)
hapiee_cz_total <- applyDictionary(hapiee_cz_total, variables=dd_hapiee_cz$Variables, categories=dd_hapiee_cz$Categories)
hapiee_lt_total <- applyDictionary(hapiee_lt_total, variables=dd_hapiee_lt$Variables, categories=dd_hapiee_lt$Categories)
hapiee_ru_total <- applyDictionary(hapiee_ru_total, variables=dd_hapiee_ru$Variables, categories=dd_hapiee_ru$Categories)
hunt_total      <- applyDictionary(hunt_total,      variables=dd_hunt$Variables,      categories=dd_hunt$Categories)
lasa1_total     <- applyDictionary(lasa1_total,     variables=dd_lasa1$Variables,     categories=dd_lasa1$Categories)
lasa2_total     <- applyDictionary(lasa2_total,     variables=dd_lasa2$Variables,     categories=dd_lasa2$Categories)
lucas_total     <- applyDictionary(lucas_total,     variables=dd_lucas$Variables,     categories=dd_lucas$Categories)
record_total    <- applyDictionary(record_total,    variables=dd_record$Variables,    categories=dd_record$Categories)

summary(as.factor(dd_hunt$Variables$repeatable))

saveOpalTable(o, globe_total,     "GLOBE_Harmonized",  "globe_DS_1_01",     force = TRUE)
saveOpalTable(o, hapiee_cz_total, "HAPIEE_Harmonized", "hapiee_cz_DS_1_01", force = TRUE)
saveOpalTable(o, hapiee_lt_total, "HAPIEE_Harmonized", "hapiee_lt_DS_1_01", force = TRUE)
saveOpalTable(o, hapiee_ru_total, "HAPIEE_Harmonized", "hapiee_ru_DS_1_01", force = TRUE)
saveOpalTable(o, hunt_total,      "HUNT_Harmonized",   "hunt_DS_1_01",      force = TRUE)
saveOpalTable(o, lasa1_total,     "LASA_Harmonized",   "lasa1_DS_1_01",     force = TRUE)
saveOpalTable(o, lasa2_total,     "LASA_Harmonized",   "lasa2_DS_1_01",     force = TRUE)
saveOpalTable(o, lucas_total,     "LUCAS_Harmonized",  "lucas_DS_1_01",     force = TRUE)
saveOpalTable(o, record_total,    "RECORD_Harmonized", "record_DS_1_01",    force = TRUE)


opal.logout(o)


save.image(file="dd_globe/dd_globe.RData")

