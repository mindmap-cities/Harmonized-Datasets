message("[2 - init]: creation of file paths")

#################     OBJECTS    ###############################################
#################                ###############################################

#list of Rmd files with domains and studies that will be included in the current harmonization 

{
  path_list = c(
    
    ### SDC ### 
    #NOT READY YET "../sociodem_characteristics/SDC_DS_ROTTERDAM.Rmd",
    "../sociodem_characteristics/SDC_DS_CLSA_COP.Rmd",
    "../sociodem_characteristics/SDC_DS_CLSA_TRA.Rmd",
    "../sociodem_characteristics/SDC_DS_GLOBE.Rmd",
    "../sociodem_characteristics/SDC_DS_HAPIEE_CZ.Rmd",  
    "../sociodem_characteristics/SDC_DS_HAPIEE_LT.Rmd",
    "../sociodem_characteristics/SDC_DS_HAPIEE_RU.Rmd",
    "../sociodem_characteristics/SDC_DS_HUNT.Rmd",
    "../sociodem_characteristics/SDC_DS_LASA1.Rmd",
    "../sociodem_characteristics/SDC_DS_LASA2.Rmd", 
    "../sociodem_characteristics/SDC_DS_LUCAS.Rmd",
    "../sociodem_characteristics/SDC_DS_RECORD.Rmd",
    
    ### LSB ###
    #NOT READY YET "../lifestyle_behaviours/LSB_DS_ROTTERDAM.Rmd",
    "../lifestyle_behaviours/LSB_DS_CLSA_COP.Rmd",
    "../lifestyle_behaviours/LSB_DS_CLSA_TRA.Rmd",
    "../lifestyle_behaviours/LSB_DS_GLOBE.Rmd",
    "../lifestyle_behaviours/LSB_DS_HAPIEE_CZ.Rmd",
    "../lifestyle_behaviours/LSB_DS_HAPIEE_LT.Rmd",
    "../lifestyle_behaviours/LSB_DS_HAPIEE_RU.Rmd",
    "../lifestyle_behaviours/LSB_DS_HUNT.Rmd",
    "../lifestyle_behaviours/LSB_DS_LASA1.Rmd",
    "../lifestyle_behaviours/LSB_DS_LASA2.Rmd",
    "../lifestyle_behaviours/LSB_DS_LUCAS.Rmd",
    "../lifestyle_behaviours/LSB_DS_RECORD.Rmd",
    
    ### OTH ### 
    # NOT READY YET  "../other_outcomes/OTH_DS_ROTTERDAM.Rmd",  
    "../other_outcomes/OTH_DS_CLSA_COP.Rmd",  
    "../other_outcomes/OTH_DS_CLSA_TRA.Rmd",  
    "../other_outcomes/OTH_DS_GLOBE.Rmd",  
    "../other_outcomes/OTH_DS_HAPIEE_CZ.Rmd",        
    "../other_outcomes/OTH_DS_HAPIEE_LT.Rmd",       
    "../other_outcomes/OTH_DS_HAPIEE_RU.Rmd",      
    "../other_outcomes/OTH_DS_HUNT.Rmd",     
    "../other_outcomes/OTH_DS_LASA1.Rmd",    
    "../other_outcomes/OTH_DS_LASA2.Rmd",   
    "../other_outcomes/OTH_DS_LUCAS.Rmd",  
    "../other_outcomes/OTH_DS_RECORD.Rmd",
    
    ### BIO ### .
    # NOT READY YET 
    "../biomarkers_genetics/BIO_DS_CLSA_COP.Rmd",
    "../biomarkers_genetics/BIO_DS_CLSA_TRA.Rmd",
    "../biomarkers_genetics/BIO_DS_GLOBE.Rmd",
    "../biomarkers_genetics/BIO_DS_HAPIEE_CZ.Rmd",
    "../biomarkers_genetics/BIO_DS_HAPIEE_LT.Rmd",
    "../biomarkers_genetics/BIO_DS_HAPIEE_RU.Rmd",
    "../biomarkers_genetics/BIO_DS_HUNT.Rmd",
    "../biomarkers_genetics/BIO_DS_LASA1.Rmd",
    "../biomarkers_genetics/BIO_DS_LASA2.Rmd",
    "../biomarkers_genetics/BIO_DS_LUCAS.Rmd",
    "../biomarkers_genetics/BIO_DS_RECORD.Rmd",
    
    ### MHO ### 
    # NOT READY YET "../mental_health_outcomes/MHO_DS_ROTTERDAM.Rmd", 
    "../mental_health_outcomes/MHO_DS_CLSA_COP.Rmd", 
    "../mental_health_outcomes/MHO_DS_CLSA_TRA.Rmd", 
    "../mental_health_outcomes/MHO_DS_GLOBE.Rmd", 
    "../mental_health_outcomes/MHO_DS_HAPIEE_CZ.Rmd",
    "../mental_health_outcomes/MHO_DS_HAPIEE_LT.Rmd",
    "../mental_health_outcomes/MHO_DS_HAPIEE_RU.Rmd",
    "../mental_health_outcomes/MHO_DS_HUNT.Rmd",
    "../mental_health_outcomes/MHO_DS_LASA1.Rmd",
    "../mental_health_outcomes/MHO_DS_LASA2.Rmd",
    "../mental_health_outcomes/MHO_DS_LUCAS.Rmd",
    "../mental_health_outcomes/MHO_DS_RECORD.Rmd",
    
    ### SOC ### 
    #NOT READY YET "../social_factors/SOC_DS_ROTTERDAM.Rmd",
    "../social_factors/SOC_DS_CLSA_COP.Rmd",
    "../social_factors/SOC_DS_CLSA_TRA.Rmd",
    "../social_factors/SOC_DS_GLOBE.Rmd",
    "../social_factors/SOC_DS_HAPIEE_CZ.Rmd",
    "../social_factors/SOC_DS_HAPIEE_LT.Rmd",
    "../social_factors/SOC_DS_HAPIEE_RU.Rmd",
    "../social_factors/SOC_DS_HUNT.Rmd",
    "../social_factors/SOC_DS_LASA1.Rmd",
    "../social_factors/SOC_DS_LASA2.Rmd",
    "../social_factors/SOC_DS_LUCAS.Rmd",
    "../social_factors/SOC_DS_RECORD.Rmd",
    
    ### ENV ### 
    #NOT READY YET "../perceptions_urban_env/ENV_DS_ROTTERDAM.Rmd",
    "../perceptions_urban_env/ENV_DS_CLSA_COP.Rmd",
    "../perceptions_urban_env/ENV_DS_CLSA_TRA.Rmd",
    "../perceptions_urban_env/ENV_DS_GLOBE.Rmd",
    "../perceptions_urban_env/ENV_DS_HAPIEE_CZ.Rmd",
    "../perceptions_urban_env/ENV_DS_HAPIEE_LT.Rmd",
    "../perceptions_urban_env/ENV_DS_HAPIEE_RU.Rmd",
    "../perceptions_urban_env/ENV_DS_HUNT.Rmd",
    "../perceptions_urban_env/ENV_DS_LASA1.Rmd",
    "../perceptions_urban_env/ENV_DS_LASA2.Rmd",
    "../perceptions_urban_env/ENV_DS_LUCAS.Rmd",
    "../perceptions_urban_env/ENV_DS_RECORD.Rmd",
    
    ### SOCENV ###    
    #NOT READY YET"../social_environmental/SOCENV_DS_ROTTERDAM.Rmd",
    "../social_environmental/SOCENV_DS_CLSA_COP.Rmd",
    "../social_environmental/SOCENV_DS_CLSA_TRA.Rmd",
    "../social_environmental/SOCENV_DS_GLOBE.Rmd",
    "../social_environmental/SOCENV_DS_HAPIEE_CZ.Rmd",
    "../social_environmental/SOCENV_DS_HAPIEE_LT.Rmd",
    "../social_environmental/SOCENV_DS_HAPIEE_RU.Rmd",
    "../social_environmental/SOCENV_DS_HUNT.Rmd",
    "../social_environmental/SOCENV_DS_LASA1.Rmd",
    "../social_environmental/SOCENV_DS_LASA2.Rmd",
    "../social_environmental/SOCENV_DS_LUCAS.Rmd",
    "../social_environmental/SOCENV_DS_RECORD.Rmd"
    
    ### PHYSENV ###
    # special file to do so
  )
}

message("[2 - end]: all file paths created")