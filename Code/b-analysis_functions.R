plot_infiltration = function(sample_data_1){
  
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
  ylim(NA,300)+
  ylab("Infiltration (s)")+
  xlab("Field ID")+
  scale_x_discrete(labels = c("8-6" = "Native",
                              "8-8" = "Organic",
                              "7-8" = "Conventional"))+
  #geom_signif(comparisons = list(c("8-6","7-8"), c("8-8", "8-6"), c("8-8", "7-8")),
   #           annotations = c("Conventional vs Native","Native vs Organic","Conventional vs Organic"),
    #          map_signif_level = TRUE, textsize = 4, vjust = -0.5, step_increase = 0.1, test="t.test")+
  stat_compare_means(comparisons = list(c("8-6", "7-8"), 
                                        c("8-8", "8-6"), 
                                        c("8-8", "7-8")),
                     label = "p.format",  # This argument is for specifying the label format
                     method = "t.test",   # This argument is for specifying the test
                     size = 3,            # Adjust text size as needed
                     geom = "text",       # This argument specifies to display p-values as text
                     position = position_identity() # This argument is for positioning the labels
                     ) + 
  theme_CKM2()
  
  
  list(gg_infiltration=gg_infiltration
       )
  
}

