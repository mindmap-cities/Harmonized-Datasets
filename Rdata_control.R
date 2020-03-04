choice <- menu(c("Yes", "No"),
               title = "Do you want to delete all Rdata except for the sourced raw data?")
ifelse(choice == 1, 
       file.remove("src/1_all_dom_data.RData")&
         file.remove("src/2_all_final_data.RData")&
         file.remove("src/3_all_data_new_id.RData")&
         file.remove("src/4_data_with_dd.RData.RData"),"ok!")