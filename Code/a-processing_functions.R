
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

