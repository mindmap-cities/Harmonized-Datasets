summarize_value <- function(old_tab,old_val){
  #old_val <- enquo(!!str2lang(old_val))
  old_val = str2lang(eval(old_val))
  old_tab <- eval(str2lang(old_tab))
  # 
  old_tab %>% select(!!old_val) %>%
    group_by(!!old_val) %>%
    summarise(nb_factors = n()) %>% print
}

add_harmo_col <- function (tbl, var, name) {
  
  var = var %>% select(var_to_add)
  names(var) <- name
  
  tbl_updated <- parceval(tbl,"tbl") %>%
    bind_cols(var)
  #  return(tbl_updated)
  assign(tbl,tbl_updated, envir = .GlobalEnv)
}

kill_harmo_col <- function (tbl, var) {
  tbl_updated = parceval(tbl,"tbl") %>%
    select(-var)
  assign(tbl,tbl_updated, envir = .GlobalEnv)
}

parceval <- function(object,type=c("tbl","val")){
  ifelse(type == "tbl", 
         return(eval(str2lang(object))),
         return(str2lang(eval(object))))
}

direct_mapping_harmo <- function(old_tab,old_val,new_tab,new_val){
  
  new_tab_name = new_tab
  
  old_val <- enquo(old_val)
  old_tab <- eval(str2lang(old_tab))
  new_tab <- eval(str2lang(new_tab))
  
  t <- new_tab %>% select(id) %>% 
    inner_join(old_tab %>% select(id,!!old_val), by="id") 
  t  <- t %>% 
    bind_cols(
      t %>% select(var_to_add = !!old_val)
    )
  add_harmo_col(new_tab_name, t, new_val) 
}

