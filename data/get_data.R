message("[3 - init]: get the data from RData or Opal, and physenv")

###########################      DATA   ########################################

#choosing between loading data from Opal or from existing Rdata in Src

answer_dd = menu(c("Yes", "No"), title="Do you want to load all_total.Rdata?")

if(answer_dd == 1){
       load("Harmonized_dataset_new/src/0_all_data_from_sourcing.RData")}else{
         
         for (i in 1:length(path_list)) {
           try(ksource(path_list[i]))}
         message("    [3.1 - init]: physenv")
         try({source("Harmonized_dataset_new/data/Recoding data in R_physenv.r")})
         message("    [3.1 - end]: physenv")
         file.remove("Harmonized_dataset_new/src/0_all_data_from_sourcing.RData")
         save.image(file="Harmonized_dataset_new/src/0_all_data_from_sourcing.RData")
         message("    [3.2]: all raw data saved in src/0_")}
       


###############################       PROCESS        ###########################
################################################################################

#Keeping only the necessary objects and cleaning up the environment from unused objects

data_to_save <- c(
  ls(pattern = "^[sdc_|_|lsb_|oth_|bio_|mho_|soc_|env_|socenv_|physenv_].*_[0|1-6]$"),
  "join_data",
  "change_class",
  "var_not_in_dd",
  "path_list",
  "create_dd",
  "order_dd",
  "complete_dd",
  "add_row_in_dd",
  "filter",
  "ksource",
  "MyMerge",
  "parceval",
  "recode",
  "select")
save(list = data_to_save, file="Harmonized_dataset_new/src/1_all_dom_data.RData")
rm(list = ls())
load("Harmonized_dataset_new/src/1_all_dom_data.RData")

message("[3 - end]: all raw data created. Only used are saved in src/1_")
