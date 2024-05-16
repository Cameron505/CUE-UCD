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
  scale_x_discrete(labels = c("N8-6" = "Native",
                              "O8-8" = "Organic",
                              "C7-8" = "Conventional"))+
  stat_compare_means(comparisons = list(c("N8-6", "C7-8"), 
                                        c("O8-8", "N8-6"), 
                                        c("O8-8", "C7-8")),
                     label = "p.format",  # This argument is for specifying the label format
                     method = "t.test",   # This argument is for specifying the test
                     size = 3,            # Adjust text size as needed
                     geom = "text",       # This argument specifies to display p-values as text
                     position = position_identity() # This argument is for positioning the labels
                     ) + 
  theme_CKM2()
  
  

Bulk_aov <- aov(bulk_density_g.cm3 ~ Plot , data = sample_data_1)
tukey_results2 <- TukeyHSD(Bulk_aov)
tukey_df2 <- as.data.frame(tukey_results2$`Plot`)

gg_Bulk<- sample_data_1 %>%
  ggplot(aes(x= as.factor(Plot),y=bulk_density_g.cm3))+
  geom_boxplot(show.legend = F, 
               outlier.colour = NULL,
               outlier.fill = NULL,
               #position = position_dodge(width = 0.6), 
               alpha = 0.2)+
  geom_point()+
  #ylim(NA,1)+
  ylab("Bulk density g.cm3")+
  xlab("Field ID")+
  scale_x_discrete(labels = c("N8-6" = "Native",
                              "O8-8" = "Organic",
                              "C7-8" = "Conventional"))+
  #geom_signif(comparisons = list(c("N8-6","C7-8"), c("O8-8", "N8-6"), c("O8-8", "C7-8")),
  #           annotations = c("Conventional vs Native","Native vs Organic","Conventional vs Organic"),
  #          map_signif_level = TRUE, textsize = 4, vjust = -0.5, step_increase = 0.1, test="t.test")+
  stat_compare_means(comparisons = list(c("N8-6", "C7-8"), 
                                        c("O8-8", "N8-6"), 
                                        c("O8-8", "C7-8")),
                     label = "p.format",  # This argument is for specifying the label format
                     method = "t.test",   # This argument is for specifying the test
                     size = 3,            # Adjust text size as needed
                     geom = "text",       # This argument specifies to display p-values as text
                     position = position_identity() # This argument is for positioning the labels
  ) + 
  theme_CKM2()














  list(gg_infiltration=gg_infiltration,
       gg_Bulk=gg_Bulk
       )
  
}

plot_WHC = function(sample_data_2){
  
  gg_bulk<- sample_data_2 %>%
    ggplot(aes(x= as.factor(Site),y=WHC.when.collected, color=Aggregatefraction ))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 #position = position_dodge(width = 0.6), 
                 alpha = 0.2)+
    geom_point(position=position_dodge(width=0.75))+
    ylim(NA,110)+
    ylab("WHC % imedietly after fractionation")+
    xlab("Field ID")+
    scale_x_discrete(labels = c("N" = "Native",
                                "O" = "Organic",
                                "C" = "Conventional"))+
    scale_color_discrete(labels = c("A" = ">2000",
                                "B" = "<2000",
                                "C" = "<250",
                                "D" = "<53"))+
    theme_CKM2()
  
  
  list(gg_bulk=gg_bulk
  )
  
}

plot_WHC2 = function(sample_data_3){
  
  gg_bulk<- sample_data_3 %>%
    ggplot(aes(x= as.factor(Site),y=g.wet.per.g.dry..100..water.holding.capacity., color=Aggregatefraction ))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 #position = position_dodge(width = 0.6), 
                 alpha = 0.2)+
    geom_point(position=position_dodge(width=0.75))+
    #ylim(NA,110)+
    ylab("g water per g dry soil at 100% WHC")+
    xlab("Field ID")+
    scale_x_discrete(labels = c("O" = "Organic",
                                "C" = "Conventional"))+
    scale_color_discrete(labels = c("A" = ">2000",
                                    "B" = "<2000",
                                    "C" = "<250",
                                    "D" = "<53"))+
    theme_CKM2()
  
  
  list(gg_bulk=gg_bulk
  )
  
}

plot_aggregate = function(sample_data_1){
  
  SD1<- sample_data_1%>%
    pivot_longer()
  
  
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
    scale_x_discrete(labels = c("N8-6" = "Native",
                                "O8-8" = "Organic",
                                "C7-8" = "Conventional"))+
    stat_compare_means(comparisons = list(c("N8-6", "C7-8"), 
                                          c("O8-8", "N8-6"), 
                                          c("O8-8", "C7-8")),
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