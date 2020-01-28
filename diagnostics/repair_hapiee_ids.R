# mho_HAPIEE_CZ_2$id<-substr(mho_HAPIEE_CZ_2$id, 1, nchar(as.character(mho_HAPIEE_CZ_2$id))-2)%>%as.factor()
# mho_HAPIEE_CZ_3$id<-substr(mho_HAPIEE_CZ_3$id, 1, nchar(as.character(mho_HAPIEE_CZ_3$id))-2)%>%as.factor()
# mho_HAPIEE_CZ_4$id<-substr(mho_HAPIEE_CZ_4$id, 1, nchar(as.character(mho_HAPIEE_CZ_4$id))-2)%>%as.factor()
# mho_HAPIEE_CZ_5$id<-substr(mho_HAPIEE_CZ_5$id, 1, nchar(as.character(mho_HAPIEE_CZ_5$id))-2)%>%as.factor()
# 
# mho_HAPIEE_RU_2$id<-substr(mho_HAPIEE_RU_2$id, 1, nchar(as.character(mho_HAPIEE_RU_2$id))-2)%>%as.factor()
# mho_HAPIEE_RU_3$id<-substr(mho_HAPIEE_RU_3$id, 1, nchar(as.character(mho_HAPIEE_RU_3$id))-2)%>%as.factor()
# 
# mho_HAPIEE_LT_1$id<-substr(mho_HAPIEE_LT_1$id, 1, nchar(as.character(mho_HAPIEE_LT_1$id))-2)%>%as.factor()
# mho_HAPIEE_LT_2$id<-substr(mho_HAPIEE_LT_2$id, 1, nchar(as.character(mho_HAPIEE_LT_2$id))-2)%>%as.factor()
# mho_HAPIEE_LT_3$id<-substr(mho_HAPIEE_LT_3$id, 1, nchar(as.character(mho_HAPIEE_LT_3$id))-2)%>%as.factor()

mho_HAPIEE_CZ_2$id<-mho_HAPIEE_CZ_2$id%>%as.character()%>%as.integer
mho_HAPIEE_CZ_3$id<-mho_HAPIEE_CZ_3$id%>%as.character()%>%as.integer
mho_HAPIEE_CZ_4$id<-mho_HAPIEE_CZ_4$id%>%as.character()%>%as.integer
mho_HAPIEE_CZ_5$id<-mho_HAPIEE_CZ_5$id%>%as.character()%>%as.integer


mho_HAPIEE_RU_2$id<-mho_HAPIEE_RU_2$id%>%as.character()%>%as.integer
mho_HAPIEE_RU_3$id<-mho_HAPIEE_RU_3$id%>%as.character()%>%as.integer

mho_HAPIEE_LT_1$id<-mho_HAPIEE_LT_1$id%>%as.character()%>%as.integer
mho_HAPIEE_LT_2$id<-mho_HAPIEE_LT_2$id%>%as.character()%>%as.integer
mho_HAPIEE_LT_3$id<-mho_HAPIEE_LT_3$id%>%as.character()%>%as.integer