
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

###########################
###### FUNCTIONS ##########
###########################

create_dd <- function(path_study,name_study){
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
      table = paste("DS",name_study,gsub("-","",today()), sep = "_"), script=paste("$('",name$Text, "')", sep="" )) %>%
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
path_list


#separate each study to work separately with. Here Hunt and Record
path_clsa      = try(path_list[str_detect(string = path_list,pattern="CLSA")])
path_globe     = try(path_list[str_detect(string = path_list,pattern="GLOBE")])
path_hapiee_cz = try(path_list[str_detect(string = path_list,pattern="HAPIEE_CZ")])
path_hapiee_lt = try(path_list[str_detect(string = path_list,pattern="HAPIEE_LT")])
path_hapiee_ru = try(path_list[str_detect(string = path_list,pattern="HAPIEE_RU")])
path_hunt      = try(path_list[str_detect(string = path_list,pattern="HUNT")])
path_lasa1     = try(path_list[str_detect(string = path_list,pattern="LASA1")])
path_lasa2     = try(path_list[str_detect(string = path_list,pattern="LASA2")])
path_lucas     = try(path_list[str_detect(string = path_list,pattern="LUCAS")])
path_record    = try(path_list[str_detect(string = path_list,pattern="RECORD")])

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
  message("problem with names")
  dd_physenv$Variables %>% filter(!str_detect(name,"physenv_"))
  message("replace m²")
  dd_physenv$Variables %>% mutate (unit = ifelse(unit == "m²", "m2",unit))


  
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
save_xls(dd_physenv$Variables   , dd_physenv$Categories    , 'RECORD' )

# message("> dd_hunt  [1378,]
#         # A tibble: 1 x 1
#         Text                                                                                            
#         <chr>                                                                                           
#         1 ### **Variable label**: Neighbourhood traffic noise     **Variable name**:  env_nbh_traf_noise_1")
# 

# 1 lsb_pa_walk_trans_3  in lasa1 and 2 missing
# 2 lsb_pa_walk_leis_1   in lasa1 and 2 missing
# 3 lsb_water_3          in lasa1 and 2 missing

