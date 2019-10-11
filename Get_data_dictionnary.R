rm(list = ls())
########################################################################################################################
################################################## DD ##################################################################
########################################################################################################################
library(naniar)
library(epiDisplay)
library(tidyverse)
library(stringr)
select <- dplyr::select
recode <- dplyr::recode
library(zoo)
#library(anchors)
#library(pipeR)
library(dplyr)
library(data.table)
library(lubridate)

load("all_total.Rdata")

###########################
###### FUNCTIONS ##########
###########################

create_dd <- function(path_study,name_study,harmo_data_set){
  #run variables
  
  
 {
  dd_allvar = list()
  for(i in 1:length(path_study)){
    dd_allvar_i = fread(path_study[i],colClasses = 'characters',na.strings = '', fill = TRUE, sep ='')
    detection_of_first_variable_i = which(str_detect(string = t(dd_allvar_i), pattern="Variable label"))[1]
    dd_allvar[[i]] = dd_allvar_i[detection_of_first_variable_i:dim(dd_allvar_i)[1]]}
  dd_allvar = Reduce(rbind,dd_allvar)
  names(dd_allvar) = "Text"
  
  
  #dd_allvar = tibble("Text" = dd_allvar) %>% select(Text = contains("Text"))
  dd_allvar[172:195]
  
  ####__2__ FROM ALL_VAR, SEPARATE EACH ROW BY ITS CHUNK - CREATE VARIABLE PART OF DD ########
  label       <- dd_allvar %>% filter(str_detect(Text,'Variable label\\*')) %>% mutate(Text = str_trim(gsub('.*\\*\\:', '',Text))) %>% as_tibble() 
  name        <- dd_allvar %>% filter(str_detect(Text,'Variable name\\*')) %>% mutate(Text = str_trim(gsub('.*\\*\\:', '',Text))) %>% as_tibble() 
  description <- dd_allvar %>% filter(str_detect(Text,'Variable description\\*')) %>% mutate(Text = str_trim(gsub('.*\\*\\:', '',Text))) %>% as_tibble() 
  type        <- dd_allvar %>% filter(str_detect(Text,'Value type|Variable type\\*')) %>% mutate(Text = str_trim(gsub('.*\\*\\:', '',Text))) %>% as_tibble() 
  unit        <- dd_allvar %>% filter(str_detect(Text,'Variable unit\\*')) %>% mutate(Text = str_trim(gsub('.*\\*\\:', '',Text))) %>% as_tibble() 
  comment     <- dd_allvar %>% filter(str_detect(Text,'Harmonization comment\\*')) %>% mutate(Text = str_trim(gsub('.*\\*\\:', '',Text))) %>% as_tibble() 
  status      <- dd_allvar %>% filter(str_detect(Text,'nization status\\*')) %>% mutate(Text = str_trim(gsub('.*\\*\\:', '',Text))) %>% as_tibble() 
  #test
  label %>% nrow %>% print
  name %>% nrow %>% print
  description %>% nrow %>% print
  type %>% nrow %>% print
  unit %>% nrow %>% print
  comment %>% nrow %>% print
  status %>% nrow %>% print

 } 
  #run variables
  study.variable <- tibble(label$Text,name$Text,description$Text,type$Text,unit$Text,comment$Text,status$Text) 
  study.variable <- study.variable %>%
    add_column(
      table =  paste("DS",name_study,gsub("-","",today()), sep = "_"), 
      script = ifelse(status$Text=="complete", paste("$('",name$Text[], "')", sep="" ),  "")) %>%
    select(
      `table`                = table,
      `name`                 = `name$Text`,	
      `label:en`             = `label$Text`,
      `description:en`       = `description$Text`,	
      `script`               =  script,	
      `valueType`	           = `type$Text`,
      `unit`                 = `unit$Text`,	
      `Mlstr_harmo::status`  = `status$Text`,
      `Mlstr_harmo::comment` = `comment$Text`
    )
  
  ####__3__ FROM ALL_VAR, COLLECT NAMES AND CATEGORIES - CREATE CATEGORIES PART OF DD ########
  #run categories
  study.categories <- dd_allvar %>% 
    filter(str_detect(Text,'Variable name|^[0-9] +\\|'))  %>% as_tibble() %>%
    separate(Text,into = c("cat","variable"), sep = "\\*:") %>%
    mutate(
      variable = str_trim(variable),
      cat = ifelse(str_detect(cat,"Variable name"), NA, cat)) %>%
    fill(variable, .direction="down") %>%
    filter(!is.na(cat)) %>%
    separate(cat,into = c("name","label"), sep = "\\|") %>%
    mutate(
      name = str_trim(name),
      label = str_trim(label)) %>%
    add_column(
      table = paste("DS",name_study,gsub("-","",today()), sep = "_"),
      missing = 0) %>%
    select(
      `table` = table,
      `variable` = variable,	
      `name` = name,
      `missing` = missing,
      `label:en` = label
    )
  
  dd <- list(study.variable,study.categories)
  names(dd) = c("Variables", "Categories")
  print(dd)
  return(dd)}

add_row_in_dd <-function(dd,name_var,name_study,label_var, harmo_data_set){

  temp <- dd$Variable %>%
  add_row(.,
    table = paste("DS",name_study,gsub("-","",today()), sep = "_"),            
    name =  name_var,  
    `label:en` =  label_var,
    `description:en` = "",
    script =   paste0("$('",name_var, "')"),
    valueType = "text",
    unit    =    "",
    `Mlstr_harmo::status` = "complete",
    `Mlstr_harmo::comment` = "")

  # to_complete = temp %>%
  #   filter(name == name_var) %>% .$name %>% tibble() %>%
  #   left_join(.,harmo_data_set %>%
  #                select(name_var) %>% names %>% tibble())
  # temp <-
  #   temp %>%
  #   mutate(
  #     `Mlstr_harmo::status` =  ifelse(!name %in% to_complete[[1]], "complete","impossible"),
  #     script = ifelse(!name %in% to_complete[[1]], paste0("$('",name, "')") , ""))
  
  return(temp)}

complete_dd <- function(dd, harmo_data_set,name_study){

  
  
  to_complete = dd$Variables %>%
    filter(str_detect(name,"physenv_")) %>% .$name %>% tibble() %>%
    right_join(.,harmo_data_set %>%
                 select(contains("physenv_")) %>% names %>% tibble())
  
  dd$Variables <- 
  dd$Variables %>%
    mutate(
      `Mlstr_harmo::status` =  ifelse(name %in% to_complete[[1]], "complete","impossible"),
      script = ifelse(name %in% to_complete[[1]], paste0("$('",name, "')") , ""))
  dd$Variables$table =  paste("DS",name_study,gsub("-","",today()), sep = "_")
  dd$Categories$table =  paste("DS",name_study,gsub("-","",today()), sep = "_")

  
  dd$Variables <- add_row_in_dd(dd,'baseline_yr',name_study, "Baseline Year",   harmo_data_set)
  dd$Variables <- add_row_in_dd(dd,'followup1_yr',name_study,"Follow-up 1 year",harmo_data_set)
  dd$Variables <- add_row_in_dd(dd,'followup2_yr',name_study,"Follow-up 2 year",harmo_data_set)
  dd$Variables <- add_row_in_dd(dd,'followup3_yr',name_study,"Follow-up 3 year",harmo_data_set)
  dd$Variables <- add_row_in_dd(dd,'followup4_yr',name_study,"Follow-up 4 year",harmo_data_set)
  dd$Variables <- add_row_in_dd(dd,'followup5_yr',name_study,"Follow-up 5 year",harmo_data_set)
  dd$Variables <- add_row_in_dd(dd,'followup6_yr',name_study,"Follow-up 6 year",harmo_data_set)
  
  dd$Variables <- add_row_in_dd(dd,'t1',name_study,"Years between follow-up 1 and baseline",   harmo_data_set)
  dd$Variables <- add_row_in_dd(dd,'t2',name_study,"Years between follow-up 2 and follow-up 1",harmo_data_set)
  dd$Variables <- add_row_in_dd(dd,'t3',name_study,"Years between follow-up 3 and follow-up 2",harmo_data_set)
  dd$Variables <- add_row_in_dd(dd,'t4',name_study,"Years between follow-up 4 and follow-up 3",harmo_data_set)
  dd$Variables <- add_row_in_dd(dd,'t5',name_study,"Years between follow-up 5 and follow-up 4",harmo_data_set)
  dd$Variables <- add_row_in_dd(dd,'t6',name_study,"Years between follow-up 6 and follow-up 5",harmo_data_set)
  
  return(dd)}

save_xls <- function(tbl_var,tbl_cat,name){
  library(openxlsx)
  zip    <- zip::zipr 
  dd_ttl = createWorkbook()
  addWorksheet(dd_ttl, "Variables")
  addWorksheet(dd_ttl, "Categories")
  writeData(dd_ttl, sheet = 'Variables', tbl_var, rowNames = FALSE)
  writeData(dd_ttl, sheet = 'Categories', tbl_cat, rowNames = FALSE)
  saveWorkbook(dd_ttl, paste0("DD_",name,".xlsx"), overwrite = TRUE)}


###########################
###### PROCEDURE ##########
###########################


#set the liste of domains of interest according to MS 
# go get "../Harmonized-Datasets/Get_data_merged_for_longitudinal_analysis.R"
path_list_todo



#separate each study to work separately with. Here Hunt and Record
path_clsa      = try(path_list_todo[str_detect(string = path_list_todo,pattern="CLSA")])
path_globe     = try(path_list_todo[str_detect(string = path_list_todo,pattern="GLOBE")])
path_hapiee_cz = try(path_list_todo[str_detect(string = path_list_todo,pattern="HAPIEE_CZ")])
path_hapiee_lt = try(path_list_todo[str_detect(string = path_list_todo,pattern="HAPIEE_LT")])
path_hapiee_ru = try(path_list_todo[str_detect(string = path_list_todo,pattern="HAPIEE_RU")])
path_hunt      = try(path_list_todo[str_detect(string = path_list_todo,pattern="HUNT")])
path_lasa1     = try(path_list_todo[str_detect(string = path_list_todo,pattern="LASA1")])
path_lasa2     = try(path_list_todo[str_detect(string = path_list_todo,pattern="LASA2")])
path_lucas     = try(path_list_todo[str_detect(string = path_list_todo,pattern="LUCAS")])
path_record    = try(path_list_todo[str_detect(string = path_list_todo,pattern="RECORD")])
 



#For each .Rmd file, detect the first d variable. It will exclude all before, et rbind for each study, domain after domain. 
#creation of a dataframe containing 1 column, with rows containing variable information (label, description, status, etc.)
#the followin code separates each variable from its information, and put them in a opal-compatible format (CSV)

#dd_clsa      <- create_dd(path_clsa , 'CLSA' )
dd_globe     <- create_dd(path_globe , 'GLOBE' )
dd_hapiee_cz <- create_dd(path_hapiee_cz , 'HAPIEE_CZ' ) #ok
dd_hapiee_lt <- create_dd(path_hapiee_lt , 'HAPIEE_LT' ) #ok
dd_hapiee_ru <- create_dd(path_hapiee_ru , 'HAPIEE_RU' ) #ok
dd_hunt      <- create_dd(path_hunt , 'HUNT' )
dd_lasa1     <- create_dd(path_lasa1 , 'LASA1' )
dd_lasa2     <- create_dd(path_lasa2 , 'LASA2' )
dd_lucas     <- create_dd(path_lucas , 'LUCAS' ) #ok
dd_record    <- create_dd(path_record , 'RECORD' ) #ok


path_env = "../physical_environmental/PHYSENV_DS.Rmd"
dd_physenv <- create_dd(path_env, "PHYSENV")
  message("replace m²")
  #dd_physenv <- dd_physenv$Variables %>% mutate (unit = ifelse(unit == "m²", "m2",unit))


#dd_clsa      <- create_dd(path_clsa , 'CLSA' )
dd_globe$Variables     <- dd_globe$Variables %>% bind_rows(dd_physenv$Variables)
dd_hapiee_cz$Variables <- dd_hapiee_cz$Variables %>% bind_rows(dd_physenv$Variables)
dd_hapiee_lt$Variables <- dd_hapiee_lt$Variables %>% bind_rows(dd_physenv$Variables)
dd_hapiee_ru$Variables <- dd_hapiee_ru$Variables %>% bind_rows(dd_physenv$Variables)
dd_hunt$Variables      <- dd_hunt$Variables %>% bind_rows(dd_physenv$Variables)
dd_lasa1$Variables     <- dd_lasa1$Variables %>% bind_rows(dd_physenv$Variables)
dd_lasa2$Variables     <- dd_lasa2$Variables %>% bind_rows(dd_physenv$Variables)
dd_lucas$Variables     <- dd_lucas$Variables %>% bind_rows(dd_physenv$Variables) %>% filter(str_detect(name,"_0"))
dd_record$Variables    <- dd_record$Variables %>% bind_rows(dd_physenv$Variables)
  
dd_globe$Categories     <- dd_globe$Categories %>% bind_rows(dd_physenv$Categories)
dd_hapiee_cz$Categories <- dd_hapiee_cz$Categories %>% bind_rows(dd_physenv$Categories)
dd_hapiee_lt$Categories <- dd_hapiee_lt$Categories %>% bind_rows(dd_physenv$Categories)
dd_hapiee_ru$Categories <- dd_hapiee_ru$Categories %>% bind_rows(dd_physenv$Categories)
dd_hunt$Categories      <- dd_hunt$Categories %>% bind_rows(dd_physenv$Categories)
dd_lasa1$Categories     <- dd_lasa1$Categories %>% bind_rows(dd_physenv$Categories)
dd_lasa2$Categories     <- dd_lasa2$Categories %>% bind_rows(dd_physenv$Categories)
dd_lucas$Categories     <- dd_lucas$Categories %>% bind_rows(dd_physenv$Categories) %>% filter(str_detect(variable,"_0"))
dd_record$Categories    <- dd_record$Categories %>% bind_rows(dd_physenv$Categories)
  

#### 


#dd_clsa     <- complete_dd(dd_clsa ,    clsa_total,      'CLSA' )
dd_globe     <- complete_dd(dd_globe,     globe_total,     'GLOBE'   )
dd_hapiee_cz <- complete_dd(dd_hapiee_cz, hapiee_cz_total, 'HAPIEE_CZ' )
dd_hapiee_lt <- complete_dd(dd_hapiee_lt, hapiee_lt_total, 'HAPIEE_LT' )
dd_hapiee_ru <- complete_dd(dd_hapiee_ru, hapiee_ru_total, 'HAPIEE_RU')
dd_hunt      <- complete_dd(dd_hunt,      hunt_total,      'HUNT')
dd_lasa1     <- complete_dd(dd_lasa1,     lasa1_total,     'LASA1')
dd_lasa2     <- complete_dd(dd_lasa2,     lasa2_total,     'LASA2')
dd_lucas     <- complete_dd(dd_lucas,     lucas_total,     'LUCAS')
dd_record    <- complete_dd(dd_record,    record_total,    'RECORD')


##checking for any issues
dd_record$Variables %>% select(table) %>% unique 
dd_record$Variables %>% filter(script == "") %>% select(`Mlstr_harmo::status`) %>% unique
  #if any trouble
  dd_record$Variables %>% filter(`Mlstr_harmo::status` == "") %>% select(table,name) %>%
    filter(!str_detect(name,"physenv_"))
dd_record$Variables %>% filter(script != "") %>% select(`Mlstr_harmo::status`) %>% unique


dd_globe$Variables %>% select(table) %>% unique 
dd_globe$Variables %>% filter(script == "") %>% select(`Mlstr_harmo::status`) %>% unique
dd_globe$Variables %>% filter(`Mlstr_harmo::status` == "") %>% select(table,name) %>%
  filter(!str_detect(name,"physenv_"))
dd_globe$Variables %>% filter(script != "") %>% select(`Mlstr_harmo::status`) %>% unique


dd_lasa1$Variables %>% select(table) %>% unique 
dd_lasa1$Variables %>% filter(script == "") %>% select(`Mlstr_harmo::status`) %>% unique
dd_lasa1$Variables %>% filter(`Mlstr_harmo::status` == "") %>% select(table,name) %>%
  filter(!str_detect(name,"physenv_"))
dd_lasa1$Variables %>% filter(script != "") %>% select(`Mlstr_harmo::status`) %>% unique


dd_lasa2$Variables %>% select(table) %>% unique 
dd_lasa2$Variables %>% filter(script == "") %>% select(`Mlstr_harmo::status`) %>% unique
dd_lasa2$Variables %>% filter(`Mlstr_harmo::status` == "") %>% select(table,name) %>%
  filter(!str_detect(name,"physenv_"))
dd_lasa2$Variables %>% filter(script != "") %>% select(`Mlstr_harmo::status`) %>% unique


dd_lucas$Variables %>% select(table) %>% unique 
dd_lucas$Variables %>% filter(script == "") %>% select(`Mlstr_harmo::status`) %>% unique
dd_lucas$Variables %>% filter(`Mlstr_harmo::status` == "") %>% select(table,name) %>%
  filter(!str_detect(name,"physenv_"))
#if any trouble
dd_lucas$Variables %>% filter(script != "") %>% select(`Mlstr_harmo::status`) %>% unique


dd_hapiee_cz$Variables %>% select(table) %>% unique 
dd_hapiee_cz$Variables %>% filter(script == "") %>% select(`Mlstr_harmo::status`) %>% unique
dd_hapiee_cz$Variables %>% filter(`Mlstr_harmo::status`== "impossible  impossible") %>% select(table,name) %>%
  filter(!str_detect(name,"physenv_"))
#if any trouble
dd_hapiee_cz$Variables %>% filter(script != "") %>% select(`Mlstr_harmo::status`) %>% unique


dd_hapiee_lt$Variables %>% select(table) %>% unique 
dd_hapiee_lt$Variables %>% filter(script == "") %>% select(`Mlstr_harmo::status`) %>% unique
#if any trouble
dd_hapiee_lt$Variables %>% filter(`Mlstr_harmo::status`== "") %>% select(table,name) %>%
  filter(!str_detect(name,"physenv_"))
dd_hapiee_lt$Variables %>% filter(script != "") %>% select(`Mlstr_harmo::status`) %>% unique


dd_hapiee_ru$Variables %>% select(table) %>% unique 
dd_hapiee_ru$Variables %>% filter(script == "") %>% select(`Mlstr_harmo::status`) %>% unique
#if any trouble
dd_hapiee_ru$Variables %>% filter(`Mlstr_harmo::status`== "") %>% select(table,name) %>%
  filter(!str_detect(name,"physenv_"))
dd_hapiee_ru$Variables %>% filter(script != "") %>% select(`Mlstr_harmo::status`) %>% unique


dd_hunt$Variables %>% select(table) %>% unique 
dd_hunt$Variables %>% filter(script == "") %>% select(`Mlstr_harmo::status`) %>% unique
#if any trouble
dd_hunt$Variables %>% filter(`Mlstr_harmo::status`== "") %>% select(table,name) %>%
  filter(!str_detect(name,"physenv_"))
dd_hunt$Variables %>% filter(script != "") %>% select(`Mlstr_harmo::status`) %>% unique


#SAVE WORK  

save_xls(dd_globe$Variables     , dd_globe$Categories     , 'GLOBE' )
save_xls(dd_hapiee_cz$Variables , dd_hapiee_cz$Categories , 'HAPIEE_CZ' )
save_xls(dd_hapiee_lt$Variables , dd_hapiee_lt$Categories , 'HAPIEE_LT' )
save_xls(dd_hapiee_ru$Variables , dd_hapiee_ru$Categories , 'HAPIEE_RU' )
save_xls(dd_hunt$Variables      , dd_hunt$Categories      , 'HUNT' )
save_xls(dd_lasa1$Variables     , dd_lasa1$Categories     , 'LASA1' ) 
save_xls(dd_lasa2$Variables     , dd_lasa2$Categories     , 'LASA2' )
save_xls(dd_lucas$Variables     , dd_lucas$Categories     , 'LUCAS' )
save_xls(dd_record$Variables    , dd_record$Categories    , 'RECORD' )

# message("> dd_hunt  [1378,]
#         # A tibble: 1 x 1
#         Text                                                                                            
#         <chr>                                                                                           
#         1 ### **Variable label**: Neighbourhood traffic noise     **Variable name**:  env_nbh_traf_noise_1")
# 

# 1 lsb_pa_walk_trans_3  in lasa1 and 2 missing
# 2 lsb_pa_walk_leis_1   in lasa1 and 2 missing
# 3 lsb_water_3          in lasa1 and 2 missing

