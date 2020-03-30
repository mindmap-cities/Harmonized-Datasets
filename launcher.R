# choice <- menu(c("Launch harmonization", "do not launch harmonization"),
#            title = "do you want to launch the full process of harmonization?")
# ifelse(choice == 1, source("autolauncher.R"),"that's ok!")

#try({file.remove("src/0_all_data_from_sourcing_backup.RData")})
try({file.remove("src/1_all_dom_data_backup.RData")})
try({file.remove("src/2_all_final_data_backup.RData")})
try({file.remove("src/3_all_data_new_id_backup.RData")})
try({file.remove("src/4_data_with_dd.RData_backup.RData")})

# file.copy(from = "src/0_all_data_from_sourcing_backup.RData",
#          "src/0_all_data_from_sourcing.RData")

file.copy(from = "src/1_all_dom_data.RData",
          "src/1_all_dom_data_backup.RData")

file.copy(from = "src/2_all_final_data.RData",
          "src/2_all_final_data_backup.RData")

file.copy(from = "src/3_all_data_new_id.RData",
          "src/3_all_data_new_id_backup.RData")

file.copy(from = "src/4_data_with_dd.RData.RData",
          "src/4_data_with_dd.RData_backup.RData")

file.remove("src/1_all_dom_data.RData")
file.remove("src/2_all_final_data.RData")
file.remove("src/3_all_data_new_id.RData")
file.remove("src/4_data_with_dd.RData.RData")


################### TO REMOVE THIS ABSOLUTELY ##################################
rm(list = ls())
################### TO REMOVE THAT ABSOLUTELY ##################################


source("config.R")
source("functions/functions.R")
source("data/list_Rmd_files.R")
source("data/get_data.R")
# make a pause here to answer the choice

source("config.R")
source("functions/functions.R")
source("munge/munge_data.R")
source("export/export_data.R")

file.remove("src/1_all_dom_data_backup.RData")
file.remove("src/2_all_final_data_backup.RData")
file.remove("src/3_all_data_new_id_backup.RData")
file.remove("src/4_data_with_dd.RData_backup.RData")


source("Rdata_control.R")
               

