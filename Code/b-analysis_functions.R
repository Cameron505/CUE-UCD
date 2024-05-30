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
    ggplot(aes(x= as.factor(Site),y=g.dry.per.g.wet..100..water.holding.capacity., color=Aggregatefraction ))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 #position = position_dodge(width = 0.6), 
                 alpha = 0.2)+
    geom_point(position=position_dodge(width=0.75))+
    #ylim(NA,110)+
    ylab("g dry per g wet soil at 100% WHC")+
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
    select(Proportion..2000,Proportion..250,Proportion..53.1,Proportion..53,Plot)%>%
    pivot_longer("Proportion..2000":"Proportion..53")
  
  avg_values <- SD1 %>%
    group_by(Plot, name) %>%
    summarize(avg_value = mean(value, na.rm = TRUE), .groups = 'drop')
  
  
  gg_agrregate_proportion<- avg_values %>%
    mutate(name= factor(name, levels=c("Proportion..2000","Proportion..250","Proportion..53.1","Proportion..53")))%>%
    ggplot(aes(x= as.factor(Plot),y=avg_value, fill=name))+
    geom_col()+
    ylab("Proportion")+
    xlab("Field ID")+
    scale_x_discrete(labels = c("N8-6" = "Native",
                                "O8-8" = "Organic",
                                "C7-8" = "Conventional"))+
    scale_fill_manual(values= cbPalette6,labels = c("Proportion..2000" = ">2 mm",
    "Proportion..250" = "2-0.25 mm",
    "Proportion..53.1" = "0.25-0.053 mm",
    "Proportion..53"= "< 0.053 mm"))+
    labs(fill = "Aggregate size") +
    theme_CKM2()+
    geom_text(data = avg_values%>%
                mutate(name= factor(name, levels=c("Proportion..2000","Proportion..250","Proportion..53.1","Proportion..53"))), aes(x = as.factor(Plot), y = avg_value, label = round(avg_value, 2)),
              position = position_stack(vjust = 0.8), color = "black", size = 8)
  
  
  
  
  
  list(gg_agrregate_proportion=gg_agrregate_proportion
  )
  
}