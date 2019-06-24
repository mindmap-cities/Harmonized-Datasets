########################################################################################################################
################################################## DD ##################################################################
########################################################################################################################
options(
  opal.username='gfabre',
  opal.password='7Q9eg2kFgAzYTk8A',
  opal.url='https://obiba.erasmusmc.nl')

message("\n*** Successfully loaded .Rprofile with Opal login options ***\n")

library(zoo)
library(anchors)
library(pipeR)
library(dplyr)
library(data.table)
rm(list = ls())

dd_test=fread('HARMO_DS_HUNT_ND.csv', colClasses = 'characters',na.strings = '', fill = TRUE)
names(dd_test)="Text"

dd_test1=dd_test[substr(dd_test$Text,1,1) =="#"|substr(dd_test$Text,1,2) %in% c("##","**", "--")|substr(dd_test$Text,1,1) %in% c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9")]
dd_test1$Order=as.numeric(rownames(dd_test1))

dd_test1_vl=dd_test1[dd_test1$Order %in% grep("Variable label", dd_test1$Text)]#2
dd_test1_vl$Variable_label=substring(gsub("Variable label", "", dd_test1_vl$Text), 11, nchar(gsub("Variable label", "", dd_test1_vl$Text)))

dd_test1_vn=dd_test1[dd_test1$Order %in% grep("Variable name", dd_test1$Text)]#1
dd_test1_vn$Variable_name=substring(gsub("Variable name", "", dd_test1_vn$Text), 7, nchar(gsub("Variable name", "", dd_test1_vn$Text)))
names(dd_test1_vn)=c("Text_vn", "Order_vn", "Variable_name")
dd_test1_vn$Order=dd_test1_vn$Order_vn-1

dd_test1_vd=dd_test1[dd_test1$Order %in% grep("Variable description", dd_test1$Text)]#4
dd_test1_vd$Variable_description=substring(gsub("Variable description", "", dd_test1_vd$Text), 7, nchar(gsub("Variable description", "", dd_test1_vd$Text)))
names(dd_test1_vd)=c("Text_vd", "Order_vd", "Variable_description")
dd_test1_vd$Order=dd_test1_vd$Order_vd-2

# dd_test1_vt=dd_test1[dd_test1$Order %in% grep("Value type", dd_test1$Text)]#4
# dd_test1_vt$Value_type=substring(gsub("Value type", "", dd_test1_vt$Text), 7, nchar(gsub("Value type", "", dd_test1_vt$Text)))
# names(dd_test1_vt)=c("Text_vt", "Order_vt", "Value_type")
# dd_test1_vt$Order=dd_test1_vt$Order_vt-3

dd_test1_vt=dd_test1[dd_test1$Order %in% grep("Variable type", dd_test1$Text)]#4
dd_test1_vt$Value_type=substring(gsub("Variable type", "", dd_test1_vt$Text), 7, nchar(gsub("Variable type", "", dd_test1_vt$Text)))
names(dd_test1_vt)=c("Text_vt", "Order_vt", "Value_type")
dd_test1_vt$Order=dd_test1_vt$Order_vt-3


dd_test1_vu=dd_test1[dd_test1$Order %in% grep("Variable unit", dd_test1$Text)]#5
dd_test1_vu$Variable_unit=substring(gsub("Variable unit", "", dd_test1_vu$Text), 7, nchar(gsub("Variable unit", "", dd_test1_vu$Text)))
names(dd_test1_vu)=c("Text_vu", "Order_vu", "Variable_unit")
dd_test1_vu$Order=dd_test1_vu$Order_vu-4

dd_test1_hs=dd_test1[dd_test1$Order %in% grep("Harmonization status", dd_test1$Text)]#6
dd_test1_hs$Harmonization_status=substring(gsub("Harmonization status", "", dd_test1_hs$Text), 7, nchar(gsub("Harmonization status", "", dd_test1_hs$Text)))
names(dd_test1_hs)=c("Text_hs", "Order_hs", "Harmonization_status")
dd_test1_hs$Order=dd_test1_hs$Order_hs-10

dd_test1_hc=dd_test1[dd_test1$Order %in% grep("Harmonization comment", dd_test1$Text)]#7
dd_test1_hc$Harmonization_comment=substring(gsub("Harmonization comment", "", dd_test1_hc$Text), 7, nchar(gsub("Harmonization comment", "", dd_test1_hc$Text)))
names(dd_test1_hc)=c("Text_hc", "Order_hc", "Harmonization_coment")
dd_test1_hc$Order=dd_test1_hc$Order_hc-11


for_dd=list(dd_test1_vn,dd_test1_vl, dd_test1_vd, dd_test1_vt, dd_test1_vu)
dd_lsb=Reduce(function(x,y) merge(x,y, by="Order", all=TRUE), for_dd)
dd_lsb$Row=rownames(dd_lsb)
for_dd1=merge(dd_test1_hs,dd_test1_hc, by="Order", all=TRUE)
for_dd1$Row=rownames(for_dd1)
table(dd_lsb$Order %in% for_dd1$Order)

dd_lsb_final=merge(dd_lsb, for_dd1, by="Row", all=TRUE)
dd_lsb_final=dd_lsb_final[,c("Variable_name", "Variable_label", "Variable_description", "Value_type", "Variable_unit", "Harmonization_status", "Harmonization_coment")]
dd_lsb_final$Variable= gsub('.{1}$', '', dd_lsb_final$Variable_name)
dd_lsb_final$Wave=substr(dd_lsb_final$Variable_name, nchar(dd_lsb_final$Variable_name), nchar(dd_lsb_final$Variable_name))
dd_lsb_final=dd_lsb_final[,c( "Wave", "Variable", "Variable_name", "Variable_label","Variable_description", 
                              "Value_type", "Variable_unit", "Harmonization_status","Harmonization_coment")]


dd_test1_cat=dd_test1[!(dd_test1$Order %in% c(grep("Variable label", dd_test1$Text), grep("Variable description", dd_test1$Text), grep("Value type", dd_test1$Text),
                                              grep("Variable unit", dd_test1$Text), grep("Harmonization status", dd_test1$Text), grep("Harmonization comment", dd_test1$Text),
                                              grep("R script", dd_test1$Text), grep("Category Label", dd_test1$Text), grep("Category coding", dd_test1$Text),
                                              grep("subdomain", dd_test1$Text), grep("------", dd_test1$Text)))]

dd_test1_cat1=dd_test1_cat[c(30:2082),]#  **from the first variable name**
dd_test1_cat1$Order_1=rownames(dd_test1_cat1)
dd_test1_cat1$Variable_name=ifelse(dd_test1_cat1$Order_1 %in% grep("Variable name", dd_test1_cat1$Text), substring(gsub("Variable name", "", dd_test1_cat1$Text), 7, nchar(gsub("Variable name", "", dd_test1_cat1$Text))), NA)

dd_test1_cat12=dd_test1_cat1[,lapply(.SD, function(t) na.locf(t))]
dd_test1_cat12$Code=substr(dd_test1_cat12$Text,1,1)
dd_test1_cat12$Category_Label=substr(dd_test1_cat12$Text,4,nchar(dd_test1_cat12$Text))
dd_test1_cat12=dd_test1_cat12[!(dd_test1_cat12$Code=="*")]
dd_test1_cat12=dd_test1_cat12[!(dd_test1_cat12$Code=="#")]
dd_test1_cat12$Variable=dd_test1_cat12$Variable= gsub('.{1}$', '', dd_test1_cat12$Variable_name)
dd_test1_cat12$Wave=substr(dd_test1_cat12$Variable_name, nchar(dd_test1_cat12$Variable_name), nchar(dd_test1_cat12$Variable_name))
dd_test1_cat12=dd_test1_cat12[,c("Wave", "Variable", "Variable_name","Code", "Category_Label")]

fwrite(dd_lsb_final, file = 'PhysEnv/PHYSENV_DD_HUNT_Variables.csv',row.names = FALSE)
fwrite(dd_test1_cat12, file = 'PhysEnv/PHYSENV_DD_HUNT_Categories.csv',row.names = FALSE)

grep("Category coding", dd_test1$Text)
grep("R script", dd_test1$Text)
