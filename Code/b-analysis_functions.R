plot_infiltration = function(Processed_Data_1){
  
  infiltration_aov <- aov(Infiltration ~ Plot , data = sample_data_1)
  tukey_results <- TukeyHSD(infiltration_aov)
  tukey_df <- as.data.frame(tukey_results$`Plot`)
  
gg_infiltration<- sample_data_1 %>%
  ggplot(aes(x= as.factor(Plot),y=Infiltration))+
  geom_boxplot(show.legend = F, 
               outlier.colour = NULL,
               outlier.fill = NULL,
               #position = position_dodge(width = 0.6), 
               alpha = 0.2)+
  geom_point()+
  ylim(NA,280)+
  ylab("Infiltration")+
  xlab("Field ID")+
  scale_x_discrete(labels = c("8-6" = "Native",
                              "8-8" = "Organic",
                              "7-8" = "Conventional"))+
  geom_signif(comparisons = list(c("8-6","7-8"), c("8-8", "8-6"), c("8-8", "7-8")),
              annotations = c("Conventional vs Native","Native vs Organic","Conventional vs Organic"),
              map_signif_level = TRUE, textsize = 4, vjust = -0.5, step_increase = 0.1)+
  theme_CKM2()
  
  
  list(#"Respiration" = gg_res,
        gg_N_Legend=gg_N_Legend,
       "Average Respiration" = gg_Avgres,
       "Cumulative Respiration" = gg_cumres,
       "Average Cumulative Respiration" = gg_Avgcumres
       )
  
}

