# choice <- menu(c("Launch harmonization", "do not launch harmonization"),
#            title = "do you want to launch the full process of harmonization?")
# ifelse(choice == 1, source("Harmonized_dataset_new/autolauncher.R"),"that's ok!")

#try({file.remove("Harmonized_dataset_new/src/0_all_data_from_sourcing_backup.RData")})
try({file.remove("Harmonized_dataset_new/src/1_all_dom_data_backup.RData")})
try({file.remove("Harmonized_dataset_new/src/2_all_final_data_backup.RData")})
try({file.remove("Harmonized_dataset_new/src/3_all_data_new_id_backup.RData")})
try({file.remove("Harmonized_dataset_new/src/4_data_with_dd.RData_backup.RData")})

# file.copy(from = "Harmonized_dataset_new/src/0_all_data_from_sourcing_backup.RData",
#          "Harmonized_dataset_new/src/0_all_data_from_sourcing.RData")

file.copy(from = "Harmonized_dataset_new/src/1_all_dom_data.RData",
          "Harmonized_dataset_new/src/1_all_dom_data_backup.RData")

file.copy(from = "Harmonized_dataset_new/src/2_all_final_data.RData",
          "Harmonized_dataset_new/src/2_all_final_data_backup.RData")

file.copy(from = "Harmonized_dataset_new/src/3_all_data_new_id.RData",
          "Harmonized_dataset_new/src/3_all_data_new_id_backup.RData")

file.copy(from = "Harmonized_dataset_new/src/4_data_with_dd.RData.RData",
          "Harmonized_dataset_new/src/4_data_with_dd.RData_backup.RData")

file.remove("Harmonized_dataset_new/src/1_all_dom_data.RData")
file.remove("Harmonized_dataset_new/src/2_all_final_data.RData")
file.remove("Harmonized_dataset_new/src/3_all_data_new_id.RData")
file.remove("Harmonized_dataset_new/src/4_data_with_dd.RData.RData")


################### TO REMOVE THIS ABSOLUTELY ##################################
rm(list = ls())
################### TO REMOVE THAT ABSOLUTELY ##################################


source("Harmonized_dataset_new/config.R")
source("Harmonized_dataset_new/functions/functions.R")
source("Harmonized_dataset_new/data/list_Rmd_files.R")
source("Harmonized_dataset_new/data/get_data.R")
# make a pause here to answer the choice


source("Harmonized_dataset_new/munge/munge_data.R")
source("Harmonized_dataset_new/export/export_data.R")
# source("Harmonized_dataset_new/")

file.remove("Harmonized_dataset_new/src/1_all_dom_data_backup.RData")
file.remove("Harmonized_dataset_new/src/2_all_final_data_backup.RData")
file.remove("Harmonized_dataset_new/src/3_all_data_new_id_backup.RData")
file.remove("Harmonized_dataset_new/src/4_data_with_dd.RData_backup.RData")
