message("[0 - init]: loading of libraries")

if(!require(utils)){
  install.packages("utils")
  library(utils)}else{library(utils)}

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)}else{library(tidyverse)}

if(!require(devtools)){
  install.packages("devtools")
  library(devtools)}else{library(devtools)}

if(!require(harmor)){
  install_local("functions/harmor", dependencies = TRUE, force=TRUE)
  library(harmor)}else{library(harmor)}

if(!require(opalr)){
  install.packages("opalr")
  library(opalr)}else{library(opalr)}

if(!require(magrittr)){
  install.packages("magrittr")
  library(magrittr)}else{library(magrittr)}

if(!require(data.table)){
  install.packages("data.table")
  library(data.table)}else{library(data.table)}

if(!require(openxlsx)){
	install.packages("openxlsx")
	library(openxlsx)}else{library(openxlsx)}

# library(stringr)
# library(zoo)
# library(anchors)
# library(pipeR)
# library(dplyr)
# library(data.table)
# library(lubridate)
# library(lubridate)
# library(scales)
# library(car)
# library(sjmisc)
# library(openxlsx)
# library(naniar)

filter <- dplyr::filter
select <- dplyr::select
recode <- dplyr::recode

message("[0 - end]: all libs loaded")

