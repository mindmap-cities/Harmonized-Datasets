answer_getData = menu(c("Yes", "No"), title="Do you want to load all_total.Rdata?")

if(answer_getData == 1){
  load("all_total.Rdata")
}

library(naniar)
library(epiDisplay)
library(tidyverse)
library(stringr)
library(lubridate)

filter <- dplyr::filter
select <- dplyr::select
recode <- dplyr::recode
## 1. knit all variable included ##

ksource <- function(x, ...) {
  library(knitr)
  source(purl(x, output = tempfile()), ...)
}

parceval <- function(text){
  eval(parse(text =text))
}

MyMerge <- function(x,y){
  df <- merge(x,y, by="id",all=TRUE)
  return(df)
}


# change_class <- function(tbl){
#   for(i in 2:ncol(tbl)){
# # tbl = physenv_lasa1_total
# # i = 439
#     temp_col <- tbl[i]
#     temp_col_int <- sapply(tbl[i], as.integer) %>% as_tibble()
#     comparison <- mean(temp_col == temp_col_int, na.rm = TRUE)
# 
#     tbl[i] <- temp_col
# 
#     tbl[i] <- ifelse(
#       comparison == 1, temp_col_int, ifelse(
#         is.nan(comparison),temp_col,temp_col))}
# 
#   return(tbl)}
# 

# change_into_char <- function(tbl){
#   for(i in 2:ncol(tbl)){
#     tbl[i] <- lapply(tbl[i], as.character)}
#   return(tbl)}






list_domain = c(
  'sociodem_characteristics',
  'lifestyle_behaviours',
  'other_outcomes',
  # 'biomarkers_genetics',   
  'mental_health_outcomes',
  'social_factors',
  'perceptions_urban_env',
  #'social_environmental',
  'physical_environmental')

nbDomains = length(list_domain)

domain_short = c(
  
  'sdc_',
  'lsb_',
  'oth_',
#  'bio_',
  'mho_',
  'soc_',
  'env_',
 # 'socenv_',
  'physenv_')

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
  'record'
  )


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
  'RECORD'
)




path_file = list()

for (i in 1:nbDomains){
  path_file[[i]] = list.files(path = paste0("../",list_domain[i]), pattern = "*.Rmd", all.files = FALSE,
                              full.names = TRUE, recursive = TRUE,
                              ignore.case = FALSE, include.dirs = FALSE )
  path_file[[i]] = path_file[[i]][!str_detect(string=path_file[[i]],pattern="validation|Validation")]
  path_file[[i]] = path_file[[i]][!str_detect(string=path_file[[i]],pattern="DS.Rmd")]
  path_file[[i]] = path_file[[i]][!str_detect(string=path_file[[i]],pattern="PHYSENV_DS")]
  
}  



names(path_file) = list_domain

path_list = path_file %>% unlist

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

answer_sourceData = menu(c("Yes", "No"), title="Do you want to ksource data?")

if(answer_sourceData == 1){
  for (i in 1:length(path_list)) {
    try(ksource(path_list[i]))}
  source("Recoding data in R_physenv.r")}


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


save.image(file="all_dom_data.RData")
answer_domData = menu(c("Yes", "No"), title="Do you want to load all_dom_data.RData?")
if(answer_domData == 1){
  load("all_dom_data.RData")
}



message("make it fonctions")
# bio_hunt_total          =  Reduce(MyMerge, list()) ; rm()
# bio_record_total        =  Reduce(MyMerge, list()) ; rm()
# bio_lasa1_total           =  Reduce(MyMerge, list(bio_LASA1_0,bio_LASA1_1,bio_LASA1_6)) ; rm(bio_LASA1_0,bio_LASA1_1,bio_LASA1_6)
# bio_lasa2_total           =  Reduce(MyMerge, list(bio_LASA2_0,bio_LASA2_2)) ; rm(bio_LASA2_0,bio_LASA2_2)
# bio_lucas_total         =  Reduce(MyMerge, list()) ; rm()
# bio_hapiee_lt_total     =  Reduce(MyMerge, list()) ; rm()
# bio_hapiee_ru_total     =  Reduce(MyMerge, list()) ; rm()
# bio_hapiee_cz_total     =  Reduce(MyMerge, list()) ; rm()
# bio_globe_total         =  Reduce(MyMerge, list()) ; rm()
# bio_clsa_total          =  Reduce(MyMerge, list()) ; rm()

mho_hunt_total            =  Reduce(MyMerge, list(mho_HUNT_0, mho_HUNT_1, mho_HUNT_2)) %>% as_tibble() ; rm(mho_HUNT_0, mho_HUNT_1, mho_HUNT_2)
mho_record_total          =  Reduce(MyMerge, list(mho_RECORD_0,mho_RECORD_1)) %>% as_tibble() ; rm(mho_RECORD_0,mho_RECORD_1)
mho_lasa1_total           =  Reduce(MyMerge, list(mho_LASA1_0,mho_LASA1_1,mho_LASA1_2,mho_LASA1_3,mho_LASA1_4,mho_LASA1_5,mho_LASA1_6)) %>% as_tibble() ; rm(mho_LASA1_0,mho_LASA1_1,mho_LASA1_2,mho_LASA1_3,mho_LASA1_4,mho_LASA1_5,mho_LASA1_6)
mho_lasa2_total           =  Reduce(MyMerge, list(mho_LASA2_0,mho_LASA2_1,mho_LASA2_2,mho_LASA2_3)) %>% as_tibble() ; rm(mho_LASA2_0,mho_LASA2_1,mho_LASA2_2,mho_LASA2_3)
mho_lucas_total           =  Reduce(MyMerge, list(mho_LUCAS_0)) %>% as_tibble() ; rm(mho_LUCAS_0)
mho_hapiee_lt_total       =  Reduce(MyMerge, list(mho_HAPIEE_LT_0,mho_HAPIEE_LT_1,mho_HAPIEE_LT_2,mho_HAPIEE_LT_3)) %>% as_tibble() ; rm(mho_HAPIEE_LT_0,mho_HAPIEE_LT_1,mho_HAPIEE_LT_2,mho_HAPIEE_LT_3)
mho_hapiee_ru_total       =  Reduce(MyMerge, list(mho_HAPIEE_RU_0,mho_HAPIEE_RU_1,mho_HAPIEE_RU_2,mho_HAPIEE_RU_3)) %>% as_tibble() ; rm(mho_HAPIEE_RU_0,mho_HAPIEE_RU_1,mho_HAPIEE_RU_2,mho_HAPIEE_RU_3)
mho_hapiee_cz_total       =  Reduce(MyMerge, list(mho_HAPIEE_CZ_0,mho_HAPIEE_CZ_1,mho_HAPIEE_CZ_2,mho_HAPIEE_CZ_3,mho_HAPIEE_CZ_4,mho_HAPIEE_CZ_5,mho_HAPIEE_CZ_6)) %>% as_tibble() ; rm(mho_HAPIEE_CZ_0,mho_HAPIEE_CZ_1,mho_HAPIEE_CZ_2,mho_HAPIEE_CZ_3,mho_HAPIEE_CZ_4,mho_HAPIEE_CZ_5,mho_HAPIEE_CZ_6)
mho_globe_total           =  Reduce(MyMerge, list(mho_GLOBE_0,mho_GLOBE_1,mho_GLOBE_2,mho_GLOBE_3,mho_GLOBE_4)) %>% as_tibble() ; rm(mho_GLOBE_0,mho_GLOBE_1,mho_GLOBE_2,mho_GLOBE_3,mho_GLOBE_4)
#mho_clsa_total          =  Reduce(MyMerge, list()) %>% as_tibble() ; rm()


sdc_hunt_total            =  Reduce(MyMerge, list(sdc_HUNT_0, sdc_HUNT_1, sdc_HUNT_2)) %>% as_tibble() ; rm(sdc_HUNT_0, sdc_HUNT_1, sdc_HUNT_2)
sdc_record_total          =  Reduce(MyMerge, list(sdc_RECORD_0,sdc_RECORD_1)) %>% as_tibble() ; rm(sdc_RECORD_0,sdc_RECORD_1)
sdc_lasa1_total           =  Reduce(MyMerge, list(sdc_LASA1_0,sdc_LASA1_1,sdc_LASA1_2,sdc_LASA1_3,sdc_LASA1_4,sdc_LASA1_5,sdc_LASA1_6)) %>% as_tibble() ; rm(sdc_LASA1_0,sdc_LASA1_1,sdc_LASA1_2,sdc_LASA1_3,sdc_LASA1_4,sdc_LASA1_5,sdc_LASA1_6)
sdc_lasa2_total           =  Reduce(MyMerge, list(sdc_LASA2_0,sdc_LASA2_1,sdc_LASA2_2,sdc_LASA2_3)); rm(sdc_LASA2_0,sdc_LASA2_1,sdc_LASA2_2,sdc_LASA2_3)
sdc_lucas_total           =  Reduce(MyMerge, list(sdc_LUCAS_0)) %>% as_tibble() ; rm(sdc_LUCAS_0)
sdc_hapiee_lt_total       =  Reduce(MyMerge, list(sdc_HAPIEE_LT_0)) %>% as_tibble() ; rm(sdc_HAPIEE_LT_0)
sdc_hapiee_ru_total       =  Reduce(MyMerge, list(sdc_HAPIEE_RU_0,sdc_HAPIEE_RU_1)) %>% as_tibble() ; rm(sdc_HAPIEE_RU_0,sdc_HAPIEE_RU_1)
sdc_hapiee_cz_total       =  Reduce(MyMerge, list(sdc_HAPIEE_CZ_0,sdc_HAPIEE_CZ_1)) %>% as_tibble() ; rm(sdc_HAPIEE_CZ_0,sdc_HAPIEE_CZ_1)
sdc_globe_total           =  Reduce(MyMerge, list(sdc_GLOBE_0,sdc_GLOBE_1,sdc_GLOBE_2,sdc_GLOBE_3,sdc_GLOBE_4)) %>% as_tibble() ; rm(sdc_GLOBE_0,sdc_GLOBE_1,sdc_GLOBE_2,sdc_GLOBE_3,sdc_GLOBE_4)
#sdc_clsa_total          =  Reduce(MyMerge, list()) %>% as_tibble() ; rm()



oth_hunt_total            =  Reduce(MyMerge, list(oth_HUNT_0, oth_HUNT_1, oth_HUNT_2)) %>% as_tibble() ; rm(oth_HUNT_0, oth_HUNT_1, oth_HUNT_2)
oth_record_total          =  Reduce(MyMerge, list(oth_RECORD_0,oth_RECORD_1)) %>% as_tibble() ; rm(oth_RECORD_0,oth_RECORD_1)
oth_lasa1_total           =  Reduce(MyMerge, list(oth_LASA1_0,oth_LASA1_1,oth_LASA1_2,oth_LASA1_3,oth_LASA1_4,oth_LASA1_5,oth_LASA1_6)) %>% as_tibble() ; rm(oth_LASA1_0,oth_LASA1_1,oth_LASA1_2,oth_LASA1_3,oth_LASA1_4,oth_LASA1_5,oth_LASA1_6)
oth_lasa2_total           =  Reduce(MyMerge, list(oth_LASA2_0,oth_LASA2_1,oth_LASA2_2,oth_LASA2_3)) %>% as_tibble() ; rm(oth_LASA2_0,oth_LASA2_1,oth_LASA2_2,oth_LASA2_3)
oth_lucas_total           =  Reduce(MyMerge, list(oth_LUCAS_0)) %>% as_tibble() ; rm(oth_LUCAS_0)
oth_hapiee_lt_total       =  Reduce(MyMerge, list(oth_HAPIEE_LT_0)) %>% as_tibble() ; rm(oth_HAPIEE_LT_0)
oth_hapiee_ru_total       =  Reduce(MyMerge, list(oth_HAPIEE_RU_0,oth_HAPIEE_RU_1)) %>% as_tibble() ; rm(oth_HAPIEE_RU_0,oth_HAPIEE_RU_1)
oth_hapiee_cz_total       =  Reduce(MyMerge, list(oth_HAPIEE_CZ_0,oth_HAPIEE_CZ_1)) %>% as_tibble() ; rm(oth_HAPIEE_CZ_0,oth_HAPIEE_CZ_1)
oth_globe_total           =  Reduce(MyMerge, list(oth_GLOBE_0,oth_GLOBE_1,oth_GLOBE_2,oth_GLOBE_3,oth_GLOBE_4)) %>% as_tibble() ; rm(oth_GLOBE_0,oth_GLOBE_1,oth_GLOBE_2,oth_GLOBE_3,oth_GLOBE_4)
#oth_clsa_total          =  Reduce(MyMerge, list()) %>% as_tibble() ; rm()



env_hunt_total            =  Reduce(MyMerge, list(env_HUNT_0, env_HUNT_1, env_HUNT_2)) %>% as_tibble() ; rm(env_HUNT_0, env_HUNT_1, env_HUNT_2)
env_record_total          =  Reduce(MyMerge, list(env_RECORD_0,env_RECORD_1)) %>% as_tibble() ; rm(env_RECORD_0,env_RECORD_1)
env_lasa1_total           =  Reduce(MyMerge, list(env_LASA1_0,env_LASA1_1,env_LASA1_2,env_LASA1_3,env_LASA1_4,env_LASA1_5,env_LASA1_6)) %>% as_tibble() ; rm(env_LASA1_0,env_LASA1_1,env_LASA1_2,env_LASA1_3,env_LASA1_4,env_LASA1_5,env_LASA1_6)
env_lasa2_total           =  Reduce(MyMerge, list(env_LASA2_0,env_LASA2_1,env_LASA2_2,env_LASA2_3)) %>% as_tibble() ; rm(env_LASA2_0,env_LASA2_1,env_LASA2_2,env_LASA2_3)
env_lucas_total           =  Reduce(MyMerge, list(env_LUCAS_0)) %>% as_tibble() ; rm(env_LUCAS_0)
env_hapiee_lt_total       =  Reduce(MyMerge, list(env_HAPIEE_LT_0)) %>% as_tibble() ; rm(env_HAPIEE_LT_0)
env_hapiee_ru_total       =  Reduce(MyMerge, list(env_HAPIEE_RU_0,env_HAPIEE_RU_1)) %>% as_tibble() ; rm(env_HAPIEE_RU_0,env_HAPIEE_RU_1)
env_hapiee_cz_total       =  Reduce(MyMerge, list(env_HAPIEE_CZ_0,env_HAPIEE_CZ_1)) %>% as_tibble() ; rm(env_HAPIEE_CZ_0,env_HAPIEE_CZ_1)
env_globe_total           =  Reduce(MyMerge, list(env_GLOBE_0,env_GLOBE_1,env_GLOBE_2,env_GLOBE_3,env_GLOBE_4)) %>% as_tibble() ; rm(env_GLOBE_0,env_GLOBE_1,env_GLOBE_2,env_GLOBE_3,env_GLOBE_4)
#env_clsa_total          =  Reduce(MyMerge, list()) %>% as_tibble() ; rm()

# socenv_hunt_total       =  Reduce(MyMerge, list()) %>% as_tibble() ; rm()
# socenv_record_total     =  Reduce(MyMerge, list()) %>% as_tibble() ; rm()
# socenv_lasa1_total      =  Reduce(MyMerge, list()) %>% as_tibble() ; rm()
# socenv_lasa2_total      =  Reduce(MyMerge, list()) %>% as_tibble() ; rm()
# socenv_lucas_total      =  Reduce(MyMerge, list(socenv_LUCAS_0)) %>% as_tibble() ; rm(socenv_LUCAS_0)
# socenv_hapiee_lt_total  =  Reduce(MyMerge, list(socenv_HAPIEE_LT_0)) %>% as_tibble() ; rm(socenv_HAPIEE_LT_0)
# socenv_hapiee_ru_total  =  Reduce(MyMerge, list(socenv_HAPIEE_RU_0,socenv_HAPIEE_RU_1)) %>% as_tibble() ; rm(socenv_HAPIEE_RU_0,socenv_HAPIEE_RU_1)
# socenv_hapiee_cz_total  =  Reduce(MyMerge, list(socenv_HAPIEE_CZ_0,socenv_HAPIEE_CZ_1)) %>% as_tibble() ; rm(socenv_HAPIEE_CZ_0,socenv_HAPIEE_CZ_1)
# socenv_globe_total      =  Reduce(MyMerge, list(socenv_GLOBE_0,socenv_GLOBE_1,socenv_GLOBE_2,socenv_GLOBE_3,socenv_GLOBE_4)) %>% as_tibble() ; rm(socenv_GLOBE_0,socenv_GLOBE_1,socenv_GLOBE_2,socenv_GLOBE_3,socenv_GLOBE_4)
# socenv_clsa_total       =  Reduce(MyMerge, list()) %>% as_tibble() ; rm()

lsb_hunt_total            =  Reduce(MyMerge, list(lsb_HUNT_0, lsb_HUNT_1, lsb_HUNT_2)) %>% as_tibble() ; rm(lsb_HUNT_0, lsb_HUNT_1, lsb_HUNT_2)
lsb_record_total          =  Reduce(MyMerge, list(lsb_RECORD_0,lsb_RECORD_1)) %>% as_tibble() ; rm(lsb_RECORD_0,lsb_RECORD_1)
lsb_lasa1_total           =  Reduce(MyMerge, list(lsb_LASA1_0,lsb_LASA1_1,lsb_LASA1_2,lsb_LASA1_3,lsb_LASA1_4,lsb_LASA1_5,lsb_LASA1_6)) %>% as_tibble() ; rm(lsb_LASA1_0,lsb_LASA1_1,lsb_LASA1_2,lsb_LASA1_3,lsb_LASA1_4,lsb_LASA1_5,lsb_LASA1_6)
lsb_lasa2_total           =  Reduce(MyMerge, list(lsb_LASA2_0,lsb_LASA2_1,lsb_LASA2_2,lsb_LASA2_3)) %>% as_tibble() ; rm(lsb_LASA2_0,lsb_LASA2_1,lsb_LASA2_2,lsb_LASA2_3)
lsb_lucas_total           =  Reduce(MyMerge, list(lsb_LUCAS_0)) %>% as_tibble() ; rm(lsb_LUCAS_0)
lsb_hapiee_lt_total       =  Reduce(MyMerge, list(lsb_HAPIEE_LT_0)) %>% as_tibble() ; rm(lsb_HAPIEE_LT_0)
lsb_hapiee_ru_total       =  Reduce(MyMerge, list(lsb_HAPIEE_RU_0,lsb_HAPIEE_RU_1)) %>% as_tibble() ; rm(lsb_HAPIEE_RU_0,lsb_HAPIEE_RU_1)
lsb_hapiee_cz_total       =  Reduce(MyMerge, list(lsb_HAPIEE_CZ_0,lsb_HAPIEE_CZ_1)) %>% as_tibble() ; rm(lsb_HAPIEE_CZ_0,lsb_HAPIEE_CZ_1)
lsb_globe_total           =  Reduce(MyMerge, list(lsb_GLOBE_0,lsb_GLOBE_1,lsb_GLOBE_2,lsb_GLOBE_3,lsb_GLOBE_4)) %>% as_tibble() ; rm(lsb_GLOBE_0,lsb_GLOBE_1,lsb_GLOBE_2,lsb_GLOBE_3,lsb_GLOBE_4)
#lsb_clsa_total          =  Reduce(MyMerge, list()) %>% as_tibble() ; rm()

physenv_hunt_total        =  Reduce(MyMerge, list(physenv_HUNT_2)) %>% as_tibble() ; rm(physenv_HUNT_2)
physenv_record_total      =  Reduce(MyMerge, list(physenv_RECORD_0,physenv_RECORD_1)) %>% as_tibble() ; rm(physenv_RECORD_0,physenv_RECORD_1)
physenv_lasa1_total       =  Reduce(MyMerge, list(physenv_LASA1_4,physenv_LASA1_6)) %>% as_tibble() ; rm(physenv_LASA1_4,physenv_LASA1_6)
physenv_lasa2_total       =  Reduce(MyMerge, list(physenv_LASA2_1,physenv_LASA2_3)) %>% as_tibble() ; rm(physenv_LASA2_1,physenv_LASA2_3)
physenv_lucas_total       =  Reduce(MyMerge, list()) %>% as_tibble() ; rm()
physenv_hapiee_lt_total   =  Reduce(MyMerge, list()) %>% as_tibble() ; rm()
physenv_hapiee_ru_total   =  Reduce(MyMerge, list()) %>% as_tibble() ; rm()
physenv_hapiee_cz_total   =  Reduce(MyMerge, list(physenv_HAPIEE_CZ_0,physenv_HAPIEE_CZ_1)) %>% as_tibble() ; rm(physenv_HAPIEE_CZ_0,physenv_HAPIEE_CZ_1)
physenv_globe_total       =  Reduce(MyMerge, list(physenv_GLOBE_1,physenv_GLOBE_2,physenv_GLOBE_3,physenv_GLOBE_4)) %>% as_tibble() ; rm(physenv_GLOBE_1,physenv_GLOBE_2,physenv_GLOBE_3,physenv_GLOBE_4)
#physenv_clsa_total      =  Reduce(MyMerge, list()) %>% as_tibble() ; rm()


soc_hunt_total            =  Reduce(MyMerge, list(soc_HUNT_0, soc_HUNT_1, soc_HUNT_2)) %>% as_tibble() ; rm(soc_HUNT_0, soc_HUNT_1, soc_HUNT_2)
soc_record_total          =  Reduce(MyMerge, list(soc_RECORD_0,soc_RECORD_1)) %>% as_tibble() ; rm(soc_RECORD_0,soc_RECORD_1)
soc_lasa1_total         =  Reduce(MyMerge, list()) %>% as_tibble() ; rm()
soc_lasa2_total         =  Reduce(MyMerge, list()) %>% as_tibble() ; rm()
soc_lucas_total           =  Reduce(MyMerge, list(soc_LUCAS_0)) %>% as_tibble() ; rm(soc_LUCAS_0)
soc_hapiee_lt_total       =  Reduce(MyMerge, list(soc_HAPIEE_LT_0)) %>% as_tibble() ; rm(soc_HAPIEE_LT_0)
soc_hapiee_ru_total       =  Reduce(MyMerge, list(soc_HAPIEE_RU_0,soc_HAPIEE_RU_1)) %>% as_tibble() ; rm(soc_HAPIEE_RU_0,soc_HAPIEE_RU_1)
soc_hapiee_cz_total       =  Reduce(MyMerge, list(soc_HAPIEE_CZ_0,soc_HAPIEE_CZ_1)) %>% as_tibble() ; rm(soc_HAPIEE_CZ_0,soc_HAPIEE_CZ_1)
soc_globe_total           =  Reduce(MyMerge, list(soc_GLOBE_0, soc_GLOBE_1, soc_GLOBE_2, soc_GLOBE_3, soc_GLOBE_4)) %>% as_tibble() ; rm(soc_GLOBE_0, soc_GLOBE_1, soc_GLOBE_2, soc_GLOBE_3, soc_GLOBE_4)
soc_clsa_total         =  Reduce(MyMerge, list()) %>% as_tibble() ; rm()



# physenv_globe_total <- change_class(physenv_globe_total)
# physenv_lasa1_total2 <- change_class(physenv_lasa1_total)
# physenv_lasa2_total <- change_class(physenv_lasa2_total)
# physenv_hapiee_cz_total<-change_class(physenv_hapiee_cz_total)
# physenv_hapiee_lt_total<-change_class(physenv_hapiee_lt_total)
# physenv_hapiee_ru_total<-change_class(physenv_hapiee_ru_total)
# physenv_lucas_total<-change_class(physenv_lucas_total)
# physenv_hunt_total<-change_class(physenv_hunt_total)
# physenv_record_total<-change_class(physenv_record_total)




save.image(file="all_total_data.RData")

answer_getTotalData = menu(c("Yes", "No"), title="Do you want to load all_total_data.RData?")
if(answer_getTotalData == 1){
  load("all_total_data.RData")
}


hunt_total   = Reduce(MyMerge, list(
  sdc_hunt_total,
  lsb_hunt_total,
  oth_hunt_total,
  #  bio_hunt_total,
  mho_hunt_total,
  soc_hunt_total,
  env_hunt_total,
  # socenv_hunt_total,
  physenv_hunt_total
)) %>% as_tibble ; rm(
  sdc_hunt_total,
  lsb_hunt_total,
  oth_hunt_total,
  #bio_hunt_total,
  mho_hunt_total,
  soc_hunt_total,
  env_hunt_total,
  #socenv_hunt_total,
  physenv_hunt_total
)

record_total = Reduce(MyMerge, list(
  sdc_record_total,
  lsb_record_total,
  oth_record_total,
  #  bio_record_total,
  mho_record_total,
  soc_record_total,
  env_record_total,
  # socenv_record_total,
  physenv_record_total
)) %>% as_tibble ; rm(
  sdc_record_total,
  lsb_record_total,
  oth_record_total,
  #bio_record_total,
  mho_record_total,
  soc_record_total,
  env_record_total,
  # socenv_record_total,
  physenv_record_total
)


lasa1_total = Reduce(MyMerge, list(
  sdc_lasa1_total,
  lsb_lasa1_total,
  oth_lasa1_total,
  #  bio_lasa1_total,
  mho_lasa1_total,
  # soc_lasa1_total,
  env_lasa1_total,
  # socenv_lasa1_total,
  physenv_lasa1_total
)) %>% as_tibble ; rm(
  sdc_lasa1_total,
  lsb_lasa1_total,
  oth_lasa1_total,
 # bio_lasa1_total,
  mho_lasa1_total,
  soc_lasa1_total,
  env_lasa1_total,
 # socenv_lasa1_total,
  physenv_lasa1_total
)


lasa2_total = Reduce(MyMerge, list(
  sdc_lasa2_total,
  lsb_lasa2_total,
  oth_lasa2_total,
  #  bio_lasa2_total,
  mho_lasa2_total,
  # soc_lasa2_total,
  env_lasa2_total,
  # socenv_lasa2_total,
  physenv_lasa2_total
)) %>% as_tibble ; rm(
  sdc_lasa2_total,
  lsb_lasa2_total,
  oth_lasa2_total,
  #bio_lasa2_total,
  mho_lasa2_total,
  soc_lasa2_total,
  env_lasa2_total,
 # socenv_lasa2_total,
  physenv_lasa2_total
)


lucas_total = Reduce(MyMerge, list(
  sdc_lucas_total,
  lsb_lucas_total,
  oth_lucas_total,
  # bio_lucas_total,
  mho_lucas_total,
  soc_lucas_total,
  env_lucas_total,
  # socenv_lucas_total,
  physenv_lucas_total
)) %>% as_tibble ; rm(
  sdc_lucas_total,
  lsb_lucas_total,
  oth_lucas_total,
  #bio_lucas_total,
  mho_lucas_total,
  soc_lucas_total,
  env_lucas_total,
  #socenv_lucas_total,
  physenv_lucas_total
)


hapiee_lt_total = Reduce(MyMerge, list(
  sdc_hapiee_lt_total,
  lsb_hapiee_lt_total,
  oth_hapiee_lt_total,
  #  bio_hapiee_lt_total,
  mho_hapiee_lt_total,
  soc_hapiee_lt_total,
  env_hapiee_lt_total,
  # socenv_hapiee_lt_total,
  physenv_hapiee_lt_total
)) %>% as_tibble ; rm(
  sdc_hapiee_lt_total,
  lsb_hapiee_lt_total,
  oth_hapiee_lt_total,
  #bio_hapiee_lt_total,
  mho_hapiee_lt_total,
  soc_hapiee_lt_total,
  env_hapiee_lt_total,
  #socenv_hapiee_lt_total,
  physenv_hapiee_lt_total
)


hapiee_ru_total = Reduce(MyMerge, list(
  sdc_hapiee_ru_total,
  lsb_hapiee_ru_total,
  oth_hapiee_ru_total,
  #  bio_hapiee_ru_total,
  mho_hapiee_ru_total,
  soc_hapiee_ru_total,
  env_hapiee_ru_total,
  # socenv_hapiee_ru_total,
  physenv_hapiee_ru_total
)) %>% as_tibble ; rm(
  sdc_hapiee_ru_total,
  lsb_hapiee_ru_total,
  oth_hapiee_ru_total,
  #bio_hapiee_ru_total,
  mho_hapiee_ru_total,
  soc_hapiee_ru_total,
  env_hapiee_ru_total,
  #socenv_hapiee_ru_total,
  physenv_hapiee_ru_total
)



hapiee_cz_total = Reduce(MyMerge, list(
  sdc_hapiee_cz_total,
  lsb_hapiee_cz_total,
  oth_hapiee_cz_total,
  #  bio_hapiee_cz_total,
  mho_hapiee_cz_total,
  soc_hapiee_cz_total,
  env_hapiee_cz_total,
  # socenv_hapiee_cz_total,
  physenv_hapiee_cz_total
)) %>% as_tibble ; rm(
  sdc_hapiee_cz_total,
  lsb_hapiee_cz_total,
  oth_hapiee_cz_total,
  #bio_hapiee_cz_total,
  mho_hapiee_cz_total,
  soc_hapiee_cz_total,
  env_hapiee_cz_total,
  #socenv_hapiee_cz_total,
  physenv_hapiee_cz_total
)



globe_total = Reduce(MyMerge, list(
  sdc_globe_total,
  lsb_globe_total,
  oth_globe_total,
  #  bio_globe_total,
  mho_globe_total,
  soc_globe_total,
  env_globe_total,
  # socenv_globe_total,
  physenv_globe_total
 )) %>% as_tibble 
# #rm(
#   sdc_globe_total,
#   lsb_globe_total,
#   oth_globe_total,
#   #bio_globe_total,
#   mho_globe_total,
#   soc_globe_total,
#   env_globe_total,
#   #socenv_globe_total,
#   physenv_globe_total
# )



# clsa_total = Reduce(MyMerge, list(
  # sdc_clsa_total,
  # lsb_clsa_total,
  # oth_clsa_total,
  #  bio_clsa_total,
  # mho_clsa_total,
  # soc_clsa_total,
  # env_clsa_total
  # socenv_clsa_total,
  # physenv_clsa_total
# # )) %>% as_tibble ; 
# rm(
#   sdc_clsa_total,
#   lsb_clsa_total,
#   oth_clsa_total,
#   #bio_clsa_total,
#   mho_clsa_total,
#   soc_clsa_total,
#   env_clsa_total,
#   socenv_clsa_total,
#   physenv_clsa_total
# )
# 



hunt_total = as_tibble(data.frame(
  id = hunt_total$id,
  hunt_total[str_subset(names(hunt_total), "_0")],
  hunt_total[str_subset(names(hunt_total), "_1")],
  hunt_total[str_subset(names(hunt_total), "_2")],
  hunt_total[str_subset(names(hunt_total), "_3")],
  hunt_total[str_subset(names(hunt_total), "_4")],
  hunt_total[str_subset(names(hunt_total), "_5")],
  hunt_total[str_subset(names(hunt_total), "_6")],
  baseline_yr	   = rep(1984) %>% as.character,
  followup1_yr	 = rep(1995) %>% as.character,
  followup2_yr	 = rep(2006) %>% as.character,
  t1	 = rep(1995 - 1984) %>% as.character,
  t2	 = rep(2006 - 1995) %>% as.character
)
)

record_total = as_tibble(data.frame(
  id = record_total$id,
  record_total[str_subset(names(record_total), "_0")],
  record_total[str_subset(names(record_total), "_1")],
  record_total[str_subset(names(record_total), "_2")],
  record_total[str_subset(names(record_total), "_3")],
  record_total[str_subset(names(record_total), "_4")],
  record_total[str_subset(names(record_total), "_5")],
  record_total[str_subset(names(record_total), "_6")],
  baseline_yr	   = rep(2007) %>% as.character,
  followup1_yr	 = rep(2011) %>% as.character,
  t1	 = rep(2011 - 2007) %>% as.character
)
)

lasa1_total = as_tibble(data.frame(
  id = lasa1_total$id,
  lasa1_total[str_subset(names(lasa1_total), "_0")],
  lasa1_total[str_subset(names(lasa1_total), "_1")],
  lasa1_total[str_subset(names(lasa1_total), "_2")],
  lasa1_total[str_subset(names(lasa1_total), "_3")],
  lasa1_total[str_subset(names(lasa1_total), "_4")],
  lasa1_total[str_subset(names(lasa1_total), "_5")],
  lasa1_total[str_subset(names(lasa1_total), "_6")],
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
  t6	 = rep(2011-2008) %>% as.character
)
)


lasa2_total = as_tibble(data.frame(
  id = lasa2_total$id,
  lasa2_total[str_subset(names(lasa2_total), "_0")],
  lasa2_total[str_subset(names(lasa2_total), "_1")],
  lasa2_total[str_subset(names(lasa2_total), "_2")],
  lasa2_total[str_subset(names(lasa2_total), "_3")],
  lasa2_total[str_subset(names(lasa2_total), "_4")],
  lasa2_total[str_subset(names(lasa2_total), "_5")],
  lasa2_total[str_subset(names(lasa2_total), "_6")],
  baseline_yr	   = rep(2002) %>% as.character,
  followup1_yr	 = rep(2005) %>% as.character,
  followup2_yr	 = rep(2008) %>% as.character,
  followup3_yr	 = rep(2011) %>% as.character,
  t1	 = rep(2005 - 2002) %>% as.character,
  t2	 = rep(2008 - 2005) %>% as.character,
  t3	 = rep(2011 - 2008) %>% as.character
  
)
)


lucas_total = as_tibble(data.frame(
  id = lucas_total$id,
  lucas_total[str_subset(names(lucas_total), "_0")],
  lucas_total[str_subset(names(lucas_total), "_1")],
  lucas_total[str_subset(names(lucas_total), "_2")],
  lucas_total[str_subset(names(lucas_total), "_3")],
  lucas_total[str_subset(names(lucas_total), "_4")],
  lucas_total[str_subset(names(lucas_total), "_5")],
  lucas_total[str_subset(names(lucas_total), "_6")],
  baseline_yr	   = rep(2000) %>% as.character
)
)


hapiee_lt_total = as_tibble(data.frame(
  id = hapiee_lt_total$id,
  hapiee_lt_total[str_subset(names(hapiee_lt_total), "_0")],
  hapiee_lt_total[str_subset(names(hapiee_lt_total), "_1")],
  hapiee_lt_total[str_subset(names(hapiee_lt_total), "_2")],
  hapiee_lt_total[str_subset(names(hapiee_lt_total), "_3")],
  hapiee_lt_total[str_subset(names(hapiee_lt_total), "_4")],
  hapiee_lt_total[str_subset(names(hapiee_lt_total), "_5")],
  hapiee_lt_total[str_subset(names(hapiee_lt_total), "_6")],
  baseline_yr	   = rep(2005) %>% as.character
)
)


hapiee_ru_total = as_tibble(data.frame(
  id = lasa2_total$id,
  hapiee_ru_total[str_subset(names(hapiee_ru_total), "_0")],
  hapiee_ru_total[str_subset(names(hapiee_ru_total), "_1")],
  hapiee_ru_total[str_subset(names(hapiee_ru_total), "_2")],
  hapiee_ru_total[str_subset(names(hapiee_ru_total), "_3")],
  hapiee_ru_total[str_subset(names(hapiee_ru_total), "_4")],
  hapiee_ru_total[str_subset(names(hapiee_ru_total), "_5")],
  hapiee_ru_total[str_subset(names(hapiee_ru_total), "_6")],
  baseline_yr	   = rep(2002) %>% as.character,
  followup1_yr	 = rep(2006) %>% as.character,
  t1	 = rep(2006 - 2002) %>% as.character
)
)

hapiee_cz_total = as_tibble(data.frame(
  id = hapiee_cz_total$id,
  hapiee_cz_total[str_subset(names(hapiee_cz_total), "_0")],
  hapiee_cz_total[str_subset(names(hapiee_cz_total), "_1")],
  hapiee_cz_total[str_subset(names(hapiee_cz_total), "_2")],
  hapiee_cz_total[str_subset(names(hapiee_cz_total), "_3")],
  hapiee_cz_total[str_subset(names(hapiee_cz_total), "_4")],
  hapiee_cz_total[str_subset(names(hapiee_cz_total), "_5")],
  hapiee_cz_total[str_subset(names(hapiee_cz_total), "_6")],
  baseline_yr	   = rep(2002) %>% as.character,
  followup1_yr	 = rep(2006) %>% as.character,
  t1	 = rep(2006 - 2002) %>% as.character
)
)


globe_total = as_tibble(data.frame(
  id = globe_total$id,
  globe_total[str_subset(names(globe_total), "_0")],
  globe_total[str_subset(names(globe_total), "_1")],
  globe_total[str_subset(names(globe_total), "_2")],
  globe_total[str_subset(names(globe_total), "_3")],
  globe_total[str_subset(names(globe_total), "_4")],
  globe_total[str_subset(names(globe_total), "_5")],
  globe_total[str_subset(names(globe_total), "_6")],
  baseline_yr	   = rep(1991) %>% as.character,
  followup1_yr	 = rep(1997) %>% as.character,
  followup2_yr	 = rep(2004) %>% as.character,
  followup3_yr	 = rep(2011) %>% as.character,
  followup4_yr	 = rep(2014) %>% as.character,
  t1	 = rep(1997 - 1991) %>% as.character,
  t2	 = rep(2004 - 1997) %>% as.character,
  t3	 = rep(2011 - 2004) %>% as.character,
  t4	 = rep(2014 - 2011) %>% as.character
)
)

# 
# clsa_total = as_tibble(data.frame(
#   id = clsa_total$id,
#   clsa_total[str_subset(names(clsa_total), "_0")],
#   clsa_total[str_subset(names(clsa_total), "_1")],
#   clsa_total[str_subset(names(clsa_total), "_2")],
#   clsa_total[str_subset(names(clsa_total), "_3")],
#   clsa_total[str_subset(names(clsa_total), "_4")],
#   clsa_total[str_subset(names(clsa_total), "_5")],
#   clsa_total[str_subset(names(clsa_total), "_6")],
#   baseline_yr	   = rep(2008) %>% as.character,
#   followup1_yr	 = rep(2015) %>% as.character,
#   t1	 = rep(2015 - 2008) %>% as.character
# )
# )




save.image(file="all_total.Rdata")

answer_TotalData = menu(c("Yes", "No"), title="Do you want to load all_total.Rdata?")
if(answer_TotalData == 1){
  load("all_total.Rdata")
}




# csv creation
for(i in 1:length(names_short)){
  try(write_csv(
    parceval(paste0(names_short[i],"_total")),
    paste0("csv_files/",names_short[i],"_Harmo_Table_",str_replace_all(today(),"-",""),".csv"),
    col_names = TRUE, na=""))
}

a<-read_csv("csv_files/globe_Harmo_Table_20191011.csv")


rm(path_file,
   i,
#   list_domain,
#   domain_short,
#   names_short,
   nbDomains,
#   path_list,
#   path_list_todo,
   ksource,
   MyMerge, 
   recode,
   select)



# rm(hunt_total,
#    record_total,
#    lasa1_total,
#    lasa2_total,
#    lucas_total,
#    hapiee_cz_total,
#    hapiee_lt_total, 
#    hapiee_ru_total,
#    globe_total,
#    clsa_total)

#Opal upload files



#opal.file_upload(erasmus_opal,paste0("csv_files/",names_short[1],"_Harmo_Table_",str_replace_all(today(),"-",""),".csv"),paste0("/projects/",names_opal_proj[i]))  


#Opal upload files
library(opalr)
erasmus_opal = opal.login()


for(i in 1:length(names_short)){
  try(opal.file_upload(erasmus_opal,paste0("csv_files/",names_short[i],"_Harmo_Table_",str_replace_all(today(),"-",""),".csv"),paste0("/projects/",names_opal_proj[i])))  
}


#opal.file_upload(erasmus_opal,paste0("csv_files/",names_short[1],"_Harmo_Table_",str_replace_all(today(),"-",""),".csv"),paste0("/projects/",names_opal_proj[i]))  
# opal.file_upload(erasmus_opal, "lasa1_Harmo_Table.csv", "/projects/LASA")
