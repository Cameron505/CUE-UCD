
Processes_Data_1= function(FILEPATH){
  # import data file
 sample_data_1
  
  
  list(respiration_data = respiration_data)
}

process_respiration = function(respiration_data){
  respiration_processed = respiration_data %>%
    mutate(Datemdy = lubridate::mdy(Date),
           JD = strftime(Datemdy, format = "%j"),
           JD2 = (as.numeric(JD)-317),
           Res2=Res * 24) %>%
    group_by(Sample_ID,Inc_temp,pre_inc) %>%
    dplyr::summarise(val=cumtrapz(JD2,Res),Res, JD2)
  

}


## Vector length and angle calculations for N vs P demand
EnzymeVec= function(Data, EnzymeID, Enzyme_value, BG, LAP, NAG, PHOS){
  
  
  Data2<-Data%>%
    pivot_wider(names_from=EnzymeID,values_from= Enzyme_value)%>%
    Mutate(CN= BG/(BG+LAP+NAG),
           CP= BG/(BG+PHOS),
           NP_length= sqrt(CN^2+CP^2),
           NP_angle= atan2(CP,CN))
  list(Data2=Data2)
} #This function is designed if all enzyme values are in one column and values are in another

EnzymeVec2= function(Data){
  
  
  Data2<- data%>%
    mutate(CN= BG/(BG+LAP+NAG),
           CP= BG/(BG+PHOS),
           NP_length= sqrt(CP^2+CN^2),
           NP_angle= atan2(CP,CN),
           Limitation= case_when(
             NP_angle >= 67.5* pi/180 ~ "Severe P",
             NP_angle >= 45* pi/180 ~ "P",
             NP_angle >= 22.5* pi/180 ~ "N",
             NP_angle >= 0 ~ "Severe N"
           )) 
  
  list(Data2=Data2)
} #This function is designed if each enzyme has its own column. 


load.S4= function(Data){
  
  Data.R <- read.csv(Data)
  Data.L <- Data.R %>%
    mutate(fPOM_TC_agg = fPOM_TC * proportion.fPOM.by.mass,    # Converting from per OM fraction to per aggregate
           oPOM_TC_agg = oPOM_TC * proportion.oPOM.by.mass,
           MAOM_TC_agg = MAOM_TC * proportion.MAOM.by.mass,
           fPOM_TN_agg = fPOM_TN * proportion.fPOM.by.mass,
           oPOM_TN_agg = oPOM_TN * proportion.oPOM.by.mass,
           MAOM_TN_agg = MAOM_TN * proportion.MAOM.by.mass)
  
  list(Data.L=Data.L)
}


