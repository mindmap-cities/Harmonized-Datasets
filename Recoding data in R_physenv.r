#Load Opal R library
library(opalr) 

# Create an opal object with the login information
erasmus_opal <- opal.login()




### RECORD ###

# Assign RECORD data 2006 and 2012
opal.assign.table.tibble(erasmus_opal, 'RECORD_physenv_2006_table', 'RECORD.physenv_record_2006')
physenv_RECORD_2006 <- opal.execute(erasmus_opal,'RECORD_physenv_2006_table')

opal.assign.table.tibble(erasmus_opal, 'RECORD_physenv_2012_table', 'RECORD.physenv_record_2012')
physenv_RECORD_2012 <- opal.execute(erasmus_opal,'RECORD_physenv_2012_table')


# Explore data
colnames(physenv_RECORD_2006)
colnames(physenv_RECORD_2012)


# Recode all _06 into _0
names(physenv_RECORD_2006) <- gsub("_06", "_0", names(physenv_RECORD_2006))

# Recode all _2012 into _1
names(physenv_RECORD_2012) <- gsub("_2012", "_1", names(physenv_RECORD_2012))


# Rename data files
physenv_RECORD_0 <- data.frame(physenv_RECORD_2006)
physenv_RECORD_1 <- data.frame(physenv_RECORD_2012)


# ne_facil --> be_facil?

# Check changes in column names in new dataframe
colnames(physenv_RECORD_0)
colnames(physenv_RECORD_1)

# remove and rename variables 
physenv_RECORD_0 <- rename(physenv_RECORD_0, 'physenv_cn_be_facil_as_0' = physenv_cn_ne_facil_as_0)
physenv_RECORD_0 <- rename(physenv_RECORD_0, 'physenv_cn_be_facil_0' = physenv._cn_ne_facil_0) 
physenv_RECORD_0 <- rename(physenv_RECORD_0, 'physenv_cn_bf_ttbsgr800_0' = physenv._cn_bf_ttbsgr800_0) 
physenv_RECORD_0 <- rename(physenv_RECORD_0, 'physenv_cn_bf_lu100_agri_0' = physenv_cn_lu100_agri_0) 
physenv_RECORD_0 <- rename(physenv_RECORD_0, 'physenv_cn_bf_lu100_in_0' = physenv_cn_bf_lu100_infra_0) 
physenv_RECORD_0 <- rename(physenv_RECORD_0, 'physenv_cn_bf_lu100_on_0' = physenv._cn_bf_lu100_on_0) 
physenv_RECORD_0 <- subset(physenv_RECORD_0, select = -c(physenv_cn_bf_lu3000_facil_0)) 
physenv_RECORD_0 <- rename(physenv_RECORD_0, 'physenv_ua_bf_forest100_0' = physenv_ua_bf_forests100_0) 
physenv_RECORD_0 <- subset(physenv_RECORD_0, select = -c(physenv_ua_bf_lu3000_facil_0))
physenv_RECORD_0 <- rename(physenv_RECORD_0, 'physenv_ua_be_facil_as_0' = physenv_ua_ne_facil_as_0) 
physenv_RECORD_0 <- rename(physenv_RECORD_0, 'physenv_ua_be_facil_0' = physenv_ua_ne_facil_0) 
physenv_RECORD_1 <- rename(physenv_RECORD_1, 'physenv_ua_ne_water_as_1' = phenv_ua_ne_water_as_1) 
physenv_RECORD_1 <- rename(physenv_RECORD_1, 'physenv_ua_ne_water_1' = phenv_ua_ne_water_1) 
physenv_RECORD_1 <- subset(physenv_RECORD_1, select = -c(commune))
physenv_RECORD_1 <- subset(physenv_RECORD_1, select = -c(codepostal))
physenv_RECORD_1 <- subset(physenv_RECORD_1, select = -c(pays))
physenv_RECORD_1 <- subset(physenv_RECORD_1, select = -c(IN_PARIS))
physenv_RECORD_1 <- subset(physenv_RECORD_1, select = -c(area))

### RECORD FINISHED ###


### GLOBE ###

# Assign GLOBE data 1997, 2004, 2011, 2014
opal.assign.table.tibble(erasmus_opal, 'GLOBE_physenv_1997_table', 'GLOBE.1997_physenv_globe_total')
physenv_GLOBE_1997 <- opal.execute(erasmus_opal,'GLOBE_physenv_1997_table')

opal.assign.table.tibble(erasmus_opal, 'GLOBE_physenv_2004_table', 'GLOBE.2004_physenv_globe_total')
physenv_GLOBE_2004 <- opal.execute(erasmus_opal,'GLOBE_physenv_2004_table')

opal.assign.table.tibble(erasmus_opal, 'GLOBE_physenv_2011_table', 'GLOBE.2011_physenv_globe_total')
physenv_GLOBE_2011 <- opal.execute(erasmus_opal,'GLOBE_physenv_2011_table')

opal.assign.table.tibble(erasmus_opal, 'GLOBE_physenv_2014_table', 'GLOBE.2014_physenv_globe_total')
physenv_GLOBE_2014 <- opal.execute(erasmus_opal,'GLOBE_physenv_2014_table')


# Explore data
# colnames(physenv_GLOBE_1997)
# colnames(physenv_GLOBE_2004)
# colnames(physenv_GLOBE_2011)
# colnames(physenv_GLOBE_2014)


# Recode all _1997 into _1
names(physenv_GLOBE_1997) <- gsub("_97", "_1", names(physenv_GLOBE_1997))

# Recode all _2004 into _2
names(physenv_GLOBE_2004) <- gsub("_04", "_2", names(physenv_GLOBE_2004))

# Recode all _2011 into _3
names(physenv_GLOBE_2011) <- gsub("_11", "_3", names(physenv_GLOBE_2011))

# Recode all _2014 into _4
names(physenv_GLOBE_2014) <- gsub("_14", "_4", names(physenv_GLOBE_2014))


# Rename data files
physenv_GLOBE_1 <- data.frame(physenv_GLOBE_1997)
physenv_GLOBE_2 <- data.frame(physenv_GLOBE_2004)
physenv_GLOBE_3 <- data.frame(physenv_GLOBE_2011)
physenv_GLOBE_4 <- data.frame(physenv_GLOBE_2014)

# Missing variables describing the distance to the nearest area marked as 'facility'
# We will upload these at a later time (not a very important variable)
# physenv_ua_be_facil_XX
# physenv_ua_be_facil_as_XX
# physenv_cn_be_facil_XX
# physenv_cn_be_facil_as_XX


# Check changes in column names
# colnames(physenv_GLOBE_1)
# colnames(physenv_GLOBE_2)
# colnames(physenv_GLOBE_3)
# colnames(physenv_GLOBE_4)

# remove unwanted variables 
physenv_GLOBE_1 = subset(physenv_GLOBE_1, select = -c(physenv_cn_bf_facil300_1)) 



### GLOBE PENDING ###




### LASA ###

# Assign LASA1 data 2006 2012
opal.assign.table.tibble(erasmus_opal, 'LASA1_2006', 'LASA.190423_MINDMAP_-_LASA1_UA2006_&_CORINE2006')
physenv_LASA1_2006 <- opal.execute(erasmus_opal,'LASA1_2006')

opal.assign.table.tibble(erasmus_opal, 'LASA1_2012', 'LASA.190423_MINDMAP_-_LASA1_UA2012_&_CORINE2012')
physenv_LASA1_2012 <- opal.execute(erasmus_opal,'LASA1_2012')


# Assign LASA2 data 2006 2012
opal.assign.table.tibble(erasmus_opal, 'LASA2_2006', 'LASA.190423_MINDMAP_-_LASA2_UA2006_&_CORINE2006')
physenv_LASA2_2006 <- opal.execute(erasmus_opal,'LASA2_2006')

opal.assign.table.tibble(erasmus_opal, 'LASA2_2012', 'LASA.190423_MINDMAP_-_LASA2_UA2012_&_CORINE2012')
physenv_LASA2_2012 <- opal.execute(erasmus_opal,'LASA2_2012')


# Explore data
# colnames(physenv_LASA1_2006)
# colnames(physenv_LASA1_2012)
# colnames(physenv_LASA2_2006)
# colnames(physenv_LASA2_2012)


# Recode all _2006 into _4 for LASA1
names(physenv_LASA1_2006) <- gsub("_06", "_4", names(physenv_LASA1_2006))

# Recode all _2012 into _6 for LASA1
names(physenv_LASA1_2012) <- gsub("_12", "_6", names(physenv_LASA1_2012))

# Recode all _2006 into _1 for LASA2
names(physenv_LASA2_2006) <- gsub("_06", "_1", names(physenv_LASA2_2006))

# Recode all _2012 into _3 for LASA2
names(physenv_LASA2_2012) <- gsub("_12", "_3", names(physenv_LASA2_2012))


# Check changes in column names
# colnames(physenv_LASA1_2006)
# colnames(physenv_LASA1_2012)
# colnames(physenv_LASA2_2006)
# colnames(physenv_LASA2_2012)


# Rename data files
physenv_LASA1_4 <- data.frame(physenv_LASA1_2006)
physenv_LASA1_6 <- data.frame(physenv_LASA1_2012)
physenv_LASA2_1 <- data.frame(physenv_LASA2_2006)
physenv_LASA2_3 <- data.frame(physenv_LASA2_2012)


# Change fa_ to facil_ for all LASA1_4, LASA1_6, LASA2_1, LASA2_3
names(physenv_LASA1_4) <- gsub("fa_", "facil_", names(physenv_LASA1_4))
names(physenv_LASA1_6) <- gsub("fa_", "facil_", names(physenv_LASA1_6))
names(physenv_LASA2_1) <- gsub("fa_", "facil_", names(physenv_LASA2_1))
names(physenv_LASA2_3) <- gsub("fa_", "facil_", names(physenv_LASA2_3))

# Change _landuse to _lu for all LASA1_4, LASA1_6, LASA2_1, LASA2_3
names(physenv_LASA1_4) <- gsub("_landuse", "_lu", names(physenv_LASA1_4))
names(physenv_LASA1_6) <- gsub("_landuse", "_lu", names(physenv_LASA1_6))
names(physenv_LASA2_1) <- gsub("_landuse", "_lu", names(physenv_LASA2_1))
names(physenv_LASA2_3) <- gsub("_landuse", "_lu", names(physenv_LASA2_3))

# Change _facilities to _facil for all LASA1_4, LASA1_6, LASA2_1, LASA2_3
names(physenv_LASA1_4) <- gsub("_facilities", "_facil", names(physenv_LASA1_4))
names(physenv_LASA1_6) <- gsub("_facilities", "_facil", names(physenv_LASA1_6))
names(physenv_LASA2_1) <- gsub("_facilities", "_facil", names(physenv_LASA2_1))
names(physenv_LASA2_3) <- gsub("_facilities", "_facil", names(physenv_LASA2_3))

# Change _area_ to _as_ for all LASA1_4, LASA1_6, LASA2_1, LASA2_3
names(physenv_LASA1_4) <- gsub("_area_", "_as_", names(physenv_LASA1_4))
names(physenv_LASA1_6) <- gsub("_area_", "_as_", names(physenv_LASA1_6))
names(physenv_LASA2_1) <- gsub("_area_", "_as_", names(physenv_LASA2_1))
names(physenv_LASA2_3) <- gsub("_area_", "_as_", names(physenv_LASA2_3))

# Change phenv_ to physenv_ for all LASA1_4, LASA1_6, LASA2_1, LASA2_3
names(physenv_LASA1_4) <- gsub("phenv_", "physenv_", names(physenv_LASA1_4))
names(physenv_LASA1_6) <- gsub("phenv_", "physenv_", names(physenv_LASA1_6))
names(physenv_LASA2_1) <- gsub("phenv_", "physenv_", names(physenv_LASA2_1))
names(physenv_LASA2_3) <- gsub("phenv_", "physenv_", names(physenv_LASA2_3))


# Change _bs_ to _water_ for all LASA1_4, LASA1_6, LASA2_1, LASA2_3
names(physenv_LASA1_4) <- gsub("_bs_", "_water_", names(physenv_LASA1_4))
names(physenv_LASA1_6) <- gsub("_bs_", "_water_", names(physenv_LASA1_6))
names(physenv_LASA2_1) <- gsub("_bs_", "_water_", names(physenv_LASA2_1))
names(physenv_LASA2_3) <- gsub("_bs_", "_water_", names(physenv_LASA2_3))


# NOTE: The variables CN2006, UA2006, CN2012 and CN2012 are meta-data variables 
# indicating whether a person was included in the calculations of those particular dataset
# They may be dropped:
physenv_LASA1_4 = subset(physenv_LASA1_4, select = -c(CN2006, UA2006))
physenv_LASA1_6 = subset(physenv_LASA1_6, select = -c(CN2012, UA2012))                     
physenv_LASA2_1 = subset(physenv_LASA2_1, select = -c(CN2006, UA2006))   
physenv_LASA2_3 = subset(physenv_LASA2_3, select = -c(CN2012, UA2012))   


# Check changes in column names
colnames(physenv_LASA1_4)
colnames(physenv_LASA1_6)
colnames(physenv_LASA2_1)
colnames(physenv_LASA2_3)



# recategorization
physenv_LASA1_4 <- physenv_LASA1_4 %>% as_tibble
physenv_LASA1_6 <- physenv_LASA1_6 %>% as_tibble
physenv_LASA2_1 <- physenv_LASA2_1 %>% as_tibble
physenv_LASA2_3 <- physenv_LASA2_3 %>% as_tibble

# 
physenv_LASA1_4 %>%
  select(physenv_ua_urbcy_4) %>%
  unique %>% na.omit()

level_key <- c("Continuous urban fabric (S.L. : > 80%)" = 1, 
               "Discontinuous dense urban fabric (S.L. : 50% -  80%)" = 2, 
               "Discontinuous medium density urban fabric (S.L. : 30% - 50%)" = 3, 
               "Discontinuous low density urban fabric (S.L. : 10% - 30%)" = 4,
               "Discontinuous very low density urban fabric (S.L. : < 10%)" = 5)

physenv_LASA1_4 <- physenv_LASA1_4 %>% mutate(physenv_ua_urbcy_4 = recode(physenv_ua_urbcy_4, !!!level_key)) %>% as.data.frame()
physenv_LASA1_6 <- physenv_LASA1_6 %>% mutate(physenv_ua_urbcy_6 = recode(physenv_ua_urbcy_6, !!!level_key)) %>% as.data.frame()
physenv_LASA2_1 <- physenv_LASA2_1 %>% mutate(physenv_ua_urbcy_1 = recode(physenv_ua_urbcy_1, !!!level_key)) %>% as.data.frame()
physenv_LASA2_3 <- physenv_LASA2_3 %>% mutate(physenv_ua_urbcy_3 = recode(physenv_ua_urbcy_3, !!!level_key)) %>% as.data.frame()
rm(level_key)

### LASA FINISHED ###




### HUNT ###

## NOTES REGARDING HUNT ENVIRONMENTAL DATA ##
# Residential history data was available to all HUNT 3 (HUNT_2) participants
# These residential addresses were linked to environmental data at three time points: 2000, 2006, 2012
# The environmental data from 2006 is from the same time as the cohort measurement and interview data
# To match the other cohort studies, the environmental data from 2006 will be renamed to _2
# The 2000 and 2012 data will be kept on the server under their orinal names (_06, _12) for possible additional analyses
# All files will be corrected with regard to other naming inconsistencies so they match the DataSchema

## HUNT 2006 --> HUNT_2 ##
# Assign HUNT data 2006 
opal.assign.table.tibble(erasmus_opal, 'HUNT3_2006', 'HUNT.physenv_hunt_2006_PID108058')
physenv_HUNT3_2006 <- opal.execute(erasmus_opal,'HUNT3_2006')

# Explore data
# colnames(physenv_HUNT3_2006)

# Recode all _06 into _2 for HUNT3
names(physenv_HUNT3_2006) <- gsub("_06", "_2", names(physenv_HUNT3_2006))

# Explore data
# colnames(physenv_HUNT3_2006)

# Rename data files
physenv_HUNT_2 <- data.frame(physenv_HUNT3_2006)


## HUNT 3 (HUNT_2) environmental data 2000 and 2012 ##
# Assign HUNT data 2000 2000
opal.assign.table.tibble(erasmus_opal, 'HUNT3_2000', 'HUNT.physenv_hunt_2000_PID108058')
physenv_HUNT3_2000 <- opal.execute(erasmus_opal,'HUNT3_2000')

opal.assign.table.tibble(erasmus_opal, 'HUNT3_2012', 'HUNT.physenv_hunt_2012_PID108058')
physenv_HUNT3_2012 <- opal.execute(erasmus_opal,'HUNT3_2012')

# Explore data
# colnames(physenv_HUNT3_2000)
# colnames(physenv_HUNT3_2012)

## Correct all naming inconsistencies
# Change _hu_lu to _cn_bf_lu for all HUNT_2, HUNT3_2000 and HUNT3_2012
names(physenv_HUNT_2) <- gsub("_hu_lu", "_cn_bf_lu", names(physenv_HUNT_2))
names(physenv_HUNT3_2000) <- gsub("_hu_lu", "_cn_bf_lu", names(physenv_HUNT3_2000))
names(physenv_HUNT3_2012) <- gsub("_hu_lu", "_cn_bf_lu", names(physenv_HUNT3_2012))

# Change _hu_ to _cn_ for all HUNT_2, HUNT3_2000 and HUNT3_2012
names(physenv_HUNT_2) <- gsub("_hu_", "_cn_", names(physenv_HUNT_2))
names(physenv_HUNT3_2000) <- gsub("_hu_", "_cn_", names(physenv_HUNT3_2000))
names(physenv_HUNT3_2012) <- gsub("_hu_", "_cn_", names(physenv_HUNT3_2012))

# Change _bf_ne_ to _bf_ for all HUNT_2, HUNT3_2000 and HUNT3_2012
names(physenv_HUNT_2) <- gsub("_bf_ne_", "_bf_", names(physenv_HUNT_2))
names(physenv_HUNT3_2000) <- gsub("_bf_ne_", "_bf_", names(physenv_HUNT3_2000))
names(physenv_HUNT3_2012) <- gsub("_bf_ne_", "_bf_", names(physenv_HUNT3_2012))

# Change ttbsgr_AREA_C to physenv_cn_ne_ttbsgr_as_12 for physenv_HUNT3_2012
colnames(physenv_HUNT3_2012)[colnames(physenv_HUNT3_2012)=="ttbsgr_AREA_C"] <- "physenv_cn_ne_ttbsgr_as_12"

# Change _uf_ne_ to _bu_ for all HUNT_2, HUNT3_2000 and HUNT3_2012
names(physenv_HUNT_2) <- gsub("_uf_", "_bu_", names(physenv_HUNT_2))
names(physenv_HUNT3_2000) <- gsub("_uf_", "_bu_", names(physenv_HUNT3_2000))
names(physenv_HUNT3_2012) <- gsub("_uf_", "_bu_", names(physenv_HUNT3_2012))

# Change physenv_cn_bf_totalgreen400 to physenv_cn_bf_ttgr400_2 for physenv_HUNT_2
colnames(physenv_HUNT_2)[colnames(physenv_HUNT_2)=="physenv_cn_bf_totalgreen400"] <- "physenv_cn_bf_ttgr400_2"

# Change physenv_cn_bf_ttbs1600_XX to physenv_cn_bf_ttbs1600_12 for physenv_HUNT3_2012
colnames(physenv_HUNT3_2012)[colnames(physenv_HUNT3_2012)=="physenv_cn_bf_ttbs1600_XX"] <- "physenv_cn_bf_ttbs1600_12"


# Check changes in column names
# colnames(physenv_HUNT_2)
# colnames(physenv_HUNT3_2000)
# colnames(physenv_HUNT3_2012)

# drop 'PrimaryLast'
physenv_HUNT3_2012 = subset(physenv_HUNT3_2012, select = -c(PrimaryLast)) 

# remove unwanted variables 
physenv_HUNT_2 = subset(physenv_HUNT_2, select = -c(physenv_cn_bf_lu3000_facil_2)) 


### HUNT FINISHED ###




### HAPIEE ###

# Assign HAPIEE data 
opal.assign.table.tibble(erasmus_opal, 'HAPIEE_Corine_0', 'HAPIEE.corine_0_Final_inorder_withid_noadd')
HAPIEE_Corine_0 <- opal.execute(erasmus_opal,'HAPIEE_Corine_0')

opal.assign.table.tibble(erasmus_opal, 'HAPIEE_Corine_1', 'HAPIEE.corine_1_Final_inorder_withid_noadd')
HAPIEE_Corine_1 <- opal.execute(erasmus_opal,'HAPIEE_Corine_1')

opal.assign.table.tibble(erasmus_opal, 'HAPIEE_UA_0', 'HAPIEE.UrbanAtlas_1_Final_inorder_withid_noadd')
HAPIEE_UA_1 <- opal.execute(erasmus_opal,'HAPIEE_UA_0')


# Explore data
# colnames(HAPIEE_Corine_0)
# colnames(HAPIEE_Corine_1)
# colnames(HAPIEE_UA_1)


# Merge and rename data files
physenv_HAPIEE_CZ_0 <- data.frame(HAPIEE_Corine_0)
physenv_HAPIEE_CZ_1 <- merge(HAPIEE_Corine_1, HAPIEE_UA_1, by = 'id')


# check data
# colnames(physenv_HAPIEE_CZ_0)
# colnames(physenv_HAPIEE_CZ_1)


# Missing variables describing the distance to the nearest area marked as 'facility'
# We will upload these at a later time (not a very important variable)
# physenv_ua_be_facil_XX
# physenv_ua_be_facil_as_XX
# physenv_cn_be_facil_XX
# physenv_cn_be_facil_as_XX


### HAPIEE PENDING ###





# remove unwanted variables 


# physenv_cn_ne_facil_as_0 change inphysenv_cn_be_facil_as_0
# physenv._cn_ne_facil_0 change inphysenv_cn_be_facil_0
# physenv._cn_bf_ttbsgr800_0 --> Name should be physenv_cn_bf_ttbsgr800_0
# physenv_cn_lu100_agri_0 --> Name should be physenv_cn_bf_lu100_agri_0
# physenv_cn_bf_lu100_infra_0 --> Name should be physenv_cn_bf_lu100_in_0
# physenv._cn_bf_lu100_on_0 --> Name should be physenv_cn_bf_lu100_on_0
# physenv_cn_bf_lu3000_facil_0   --> remove variable 
# physenv_ua_bf_forests100_0 -->  Name should be physenv_ua_bf_forest100_0
# physenv_ua_bf_lu3000_facil_0  --> remove variable
# physenv_ua_ne_facil_as_0 --> should be physenv_ua_be_facil_as_0 
# physenv_ua_ne_facil_0 --> physenv_ua_be_facil_0
# phenv_ua_ne_water_as_1 --> name should be physenv_ua_ne_water_as_1
# phenv_ua_ne_water_1 --> name should be physenv_ua_ne_water_1
# commune --> these are locational indicators. I believe Erik has these in his file too. They can be removed here
# codepostal à see above
# pays  à see above                      
# IN_PARIS à see above
# area à see above

