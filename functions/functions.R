message("[1 - init]: creating all functions")

################################  COMMON FUNCTIONS #############################

ksource <- function(x, ...) {
  library(knitr)
  source(purl(x, output = tempfile()), ...)}

parceval <- function(text){
  eval(parse(text =text))}

MyMerge <- function(x,y){
  df <- merge(x,y, by="id",all=TRUE)
  return(df)}



###################################  DATA ######################################

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


#################################  DATA DICTIONARY #############################

create_dd <- function(path_study,name_study){
  # run variables
  # 
  # path_study = path_globe
  # name_study = 'GLOBE'
  
  {
    dd_allvar = list()
    for(i in 1:length(path_study)){
      dd_allvar_i = fread(path_study[i],colClasses = 'character',na.strings = '', fill = TRUE, sep ='')
      detection_of_first_variable_i = which(str_detect(string = t(dd_allvar_i), pattern="Variable label"))[1]
      dd_allvar[[i]] = dd_allvar_i[detection_of_first_variable_i:dim(dd_allvar_i)[1]]}
    dd_allvar = Reduce(rbind,dd_allvar)
    names(dd_allvar) = "Text"
    
    dd_allvar[172:195]
    
    ####__2__ FROM ALL_VAR, SEPARATE EACH ROW BY ITS CHUNK - CREATE VARIABLE PART OF DD ########
    label       <- dd_allvar %>% filter(str_detect(Text,'Variable label\\*')) %>% mutate(Text = str_trim(gsub('.*\\*\\:', '',Text))) %>% as_tibble() 
    name        <- dd_allvar %>% filter(str_detect(Text,'Variable name\\*')) %>% mutate(Text = str_trim(gsub('.*\\*\\:', '',Text))) %>% as_tibble() 
    description <- dd_allvar %>% filter(str_detect(Text,'Variable description\\*')) %>% mutate(Text = str_trim(gsub('.*\\*\\:', '',Text))) %>% as_tibble() 
    type        <- dd_allvar %>% filter(str_detect(Text,'Value type|Variable type\\*')) %>% mutate(Text = str_trim(gsub('.*\\*\\:', '',Text))) %>% as_tibble() 
    type    <- type %>% mutate(Text = tolower(Text))
    unit        <- dd_allvar %>% filter(str_detect(Text,'Variable unit\\*')) %>% mutate(Text = str_trim(gsub('.*\\*\\:', '',Text))) %>% as_tibble() 
    unit    <- unit %>% mutate(Text = ifelse( Text == "N/A",NA,Text))
    comment     <- dd_allvar %>% filter(str_detect(Text,'Harmonization comment\\*')) %>% mutate(Text = str_trim(gsub('.*\\*\\:', '',Text))) %>% as_tibble() 
    comment <- comment %>% mutate(Text = ifelse( Text == "",NA,Text))
    status      <- dd_allvar %>% filter(str_detect(Text,'nization status\\*')) %>% mutate(Text = str_trim(gsub('.*\\*\\:', '',Text))) %>% as_tibble() 
    status  <- status %>% mutate(Text = ifelse( Text == "","impossible",Text))
    
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
      # repeatable = 0,
      script_r = ifelse(status$Text=="complete", paste("$('",name$Text[], "')", sep = "" ),  NA)) %>%
    select(
      # `repeatable`           = repeatable,
      `name`                 = `name$Text`,	
      `label:en`             = `label$Text`,
      `description:en`       = `description$Text`,	
      `script`               =  script_r,	
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
      # table = paste(name_study, "harmo_table",gsub("-","",last_release), sep = "_"),
      missing = 0) %>%
    select(
      # `table` = table,
      `variable` = variable,	
      `name` = name,
      `missing` = missing,
      `label:en` = label
    )
  
  dd <- list(study.variable,study.categories)
  names(dd) = c("Variables", "Categories")
  return(dd)
}

order_dd <- function(dd){
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
  
  dd$Variables <-   dd$Variables %>% filter(str_detect(name, "_0$")) %>%
    bind_rows(dd$Variables %>% filter(str_detect(name, "_1$"))) %>% 
    bind_rows(dd$Variables %>% filter(str_detect(name, "_2$"))) %>%
    bind_rows(dd$Variables %>% filter(str_detect(name, "_3$"))) %>%
    bind_rows(dd$Variables %>% filter(str_detect(name, "_4$"))) %>%
    bind_rows(dd$Variables %>% filter(str_detect(name, "_5$"))) %>%
    bind_rows(dd$Variables %>% filter(str_detect(name, "_6$")))
  
  dd$Categories <-    dd$Categories %>% filter(str_detect(variable, "_0$")) %>%
    bind_rows(dd$Categories %>% filter(str_detect(variable, "_1$"))) %>% 
    bind_rows(dd$Categories %>% filter(str_detect(variable, "_2$"))) %>%
    bind_rows(dd$Categories %>% filter(str_detect(variable, "_3$"))) %>%
    bind_rows(dd$Categories %>% filter(str_detect(variable, "_4$"))) %>%
    bind_rows(dd$Categories %>% filter(str_detect(variable, "_5$"))) %>%
    bind_rows(dd$Categories %>% filter(str_detect(variable, "_6$")))
  print(dd)
  return(dd)
}


add_row_in_dd <-function(dd,name_var,name_study,label_var, harmo_data_set){
  
  total_release <- readLines("~/Harmonized-Datasets/export/version_release.info")
  last_release <- total_release[length(total_release)]
  
  temp <- dd$Variable %>%
    add_row(.,
            # repeatable = 0,
            name =  name_var,  
            `label:en` =  label_var,
            `description:en` = label_var,
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
  
  #   dd = dd_hunt
  #   harmo_data_set = hunt_total
  #   name_study = 'HUNT'
  total_release <- readLines("~/Harmonized-Datasets/export/version_release.info")
  last_release <- total_release[length(total_release)]
  
  to_complete = dd$Variables %>%
    filter(str_detect(name,"physenv_")) %>% .$name %>% tibble() %>%
    inner_join(.,harmo_data_set %>%
                 select(contains("physenv_")) %>% names %>% tibble())
  
  dd$Variables <- dd$Variables %>%
    mutate(
      `Mlstr_harmo::status` = ifelse(str_detect(name,"physenv_"), 
                                     ifelse(name %in% to_complete[[1]], "complete","impossible"),`Mlstr_harmo::status`),
      script =                ifelse(str_detect(name,"physenv_"),
                                     ifelse(name %in% to_complete[[1]], paste0("$('",name, "')"),NA), script))
  
  
  # dd$Variables$repeatable =  0
  # dd$Categories$table =  paste(name_study, "harmo_table",gsub("-","",last_release), sep = "_")
  
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

var_not_in_dd <- function(tbl,dd){
  left <- tibble(name = tbl %>% names)
  right <- dd %>% select(name)
  test <- anti_join(left, right, by = "name") %>% filter(name != "id") %>% .[[1]]
  return(test)}




message("[1 - end]: all functions created")
