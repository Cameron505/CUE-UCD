plot_infiltration = function(sample_data_1){
  
  sample_data_1<-sample_data_1%>%
    filter(Plot != "")
  
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
  #          map_signif_level = TRUE, textsize = 4, vjust = 2, step_increase = 0.1, test="t.test")+
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



means<-sample_data_1%>%
  group_by(Plot)%>%
  summarise(mean1=mean(bulk_density_g.cm3, na.rm = TRUE), mean2=mean(Infiltration, na.rm = TRUE))









  list(gg_infiltration=gg_infiltration,
       gg_Bulk=gg_Bulk
       )
  
}

plot_WHC = function(sample_data_2){
  
  gg_bulk<- sample_data_2 %>%
    ggplot(aes(x= as.factor(Aggregate),y=WHC.when.collected, color=Aggregatefraction ))+
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
    ggplot(aes(x= as.factor(Aggregatefraction),y=g.dry.per.g.wet..100..water.holding.capacity., color=Aggregatefraction ))+
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
    filter(Plot != "")%>%
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
  
  
  
  gg_Mean_Weight_diameter<- sample_data_1 %>%
    ggplot(aes(x= as.factor(Plot),y=Mean.weight.Diameter..um.))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 #position = position_dodge(width = 0.6), 
                 alpha = 0.2)+
    ylab("Mean weight diameter (um)")+
    xlab("Field ID")+
    scale_x_discrete(labels = c("N8-6" = "Native",
                                "O8-8" = "Organic",
                                "C7-8" = "Conventional"))+
    theme_CKM2()+
    stat_compare_means(comparisons = list(c("N8-6", "C7-8"), 
                                          c("O8-8", "N8-6"), 
                                          c("O8-8", "C7-8")),
                       label = "p.format",  # This argument is for specifying the label format
                       method = "t.test",   # This argument is for specifying the test
                       size = 3,            # Adjust text size as needed
                       geom = "text",       # This argument specifies to display p-values as text
                       position = position_identity() # This argument is for positioning the labels
    ) 
  
  ggplot_build(gg_Mean_Weight_diameter)$data[[1]]
  
  means<-sample_data_1%>%
    group_by(Plot)%>%
    summarise(mean=mean(Mean.weight.Diameter..um.))
  
  
  list(gg_agrregate_proportion=gg_agrregate_proportion,
       gg_Mean_Weight_diameter=gg_Mean_Weight_diameter
  )
  
}

plot_respiration = function(sample_data_4){
  
  sample_data_4$Aggregate <- factor(sample_data_4$Aggregate, levels = c("A", "B", "C", "D", "Bulk"))
  
  
  response_variables <- c("CO2", "N2O", "CH4")
  
  # Function to perform ANOVA and posthoc test, and extract results
  analyze_and_extract <- function(data, variable) {
    formula_str <- paste0(variable, " ~ Site")
    anova_model <- aov(as.formula(formula_str), data = data)
    anova_summary <- summary(anova_model)
    anova_p_value <- anova_summary[[1]]$`Pr(>F)`[1]
    
    posthoc_results <- NULL
    if (anova_p_value < 0.05) {
      posthoc_results <- data %>%
        group_by(Aggregate) %>%
        tukey_hsd(as.formula(formula_str)) %>%
        mutate(variable = variable) # Add the variable name for later use
    } else {
      # If ANOVA is not significant, create an empty tibble
      posthoc_results <- tibble(
        Aggregate = unique(data$Aggregate),
        term = "Site",
        group1 = NA_character_,
        group2 = NA_character_,
        estimate = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        p.adj = anova_p_value,
        variable = variable
      )
    }
    return(posthoc_results)
  }
  
 
  all_posthoc_results <- map_dfr(response_variables, ~ analyze_and_extract(sample_data_4, .x))
  
  
  all_posthoc_results_adjusted <- all_posthoc_results %>%
    group_by(variable, Aggregate) %>%
    adjust_pvalue(method = "BH") %>%
    ungroup() %>%
    mutate(p.adj.signif = case_when(
      p.adj <= 0.001 ~ "***",
      p.adj <= 0.01 ~ "**",
      p.adj <= 0.05 ~ "*",
      TRUE ~ "ns"
    ))
  
  
  plot_labels <- all_posthoc_results_adjusted %>%
    filter(p.adj < 0.05) %>% # Only keep significant comparisons for labeling
    separate(group1, into = c("site1"), sep = "(?<=.)", remove = FALSE) %>% # Small hack to get site labels
    separate(group2, into = c("site2"), sep = "(?<=.)", remove = FALSE) %>% # Small hack to get site labels
    mutate(
      y_position = case_when(
        variable == "CO2" ~ max(sample_data_4$CO2, na.rm = TRUE) * 1.1, # Adjust these multipliers based on your data range
        variable == "N2O" ~ max(sample_data_4$N2O, na.rm = TRUE) * 1.1,
        variable == "CH4" ~ max(sample_data_4$CH4, na.rm = TRUE) * 1.1,
        TRUE ~ NA_real_ # Add more cases for other variables
      ),
      label = p.adj.signif
    )
  
  # Create the ggplot
  CO2_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = CO2, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab(expression(CO[2] ~ (μg ~ day^{-1} ~ g^{-1} ~ dry ~ aggregate)))+
    xlab("Field ID") +
    
    facet_wrap(~Addition) +
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    theme_CKM2() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "CO2"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)
  
  print(CO2_plot)
  
  N2O_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = N2O, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab(expression(N[2] * O ~ (μg ~ day^{-1} ~ g^{-1} ~ dry ~ aggregate)))+
    xlab("Field ID") +
    
    facet_wrap(~Addition) +
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    theme_CKM2() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "N2O"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)
  
  print(N2O_plot)
  
  
  CH4_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = CH4, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab(expression(CH[4] ~ (μg ~ day^{-1} ~ g^{-1} ~ dry ~ aggregate)))+
    xlab("Field ID") +
    
    facet_wrap(~Addition) +
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    theme_CKM2() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "CH4"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)
  
  print(CH4_plot)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  list(CO2_plot=CO2_plot,
       N2O_plot=N2O_plot,
       CH4_plot=CH4_plot
  )
  
}

plot_respiration2 = function(sample_data_4){
  
  sample_data_4$Aggregate <- factor(sample_data_4$Aggregate, levels = c("A", "B", "C", "D", "Bulk"))
  
  
  response_variables <- c("CO2", "N2O", "CH4")
  
  # Function to perform ANOVA and posthoc test, and extract results
  analyze_and_extract <- function(data, variable) {
    formula_str <- paste0(variable, " ~ Site")
    anova_model <- aov(as.formula(formula_str), data = data)
    anova_summary <- summary(anova_model)
    anova_p_value <- anova_summary[[1]]$`Pr(>F)`[1]
    
    posthoc_results <- NULL
    if (anova_p_value < 0.05) {
      posthoc_results <- data %>%
        group_by(Aggregate) %>%
        tukey_hsd(as.formula(formula_str)) %>%
        mutate(variable = variable) # Add the variable name for later use
    } else {
      # If ANOVA is not significant, create an empty tibble
      posthoc_results <- tibble(
        Aggregate = unique(data$Aggregate),
        term = "Site",
        group1 = NA_character_,
        group2 = NA_character_,
        estimate = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        p.adj = anova_p_value,
        variable = variable
      )
    }
    return(posthoc_results)
  }
  
  
  all_posthoc_results <- map_dfr(response_variables, ~ analyze_and_extract(sample_data_4, .x))
  
  
  all_posthoc_results_adjusted <- all_posthoc_results %>%
    group_by(variable, Aggregate) %>%
    adjust_pvalue(method = "BH") %>%
    ungroup() %>%
    mutate(p.adj.signif = case_when(
      p.adj <= 0.001 ~ "***",
      p.adj <= 0.01 ~ "**",
      p.adj <= 0.05 ~ "*",
      TRUE ~ "ns"
    ))
  
  
  plot_labels <- all_posthoc_results_adjusted %>%
    filter(p.adj < 0.05) %>% # Only keep significant comparisons for labeling
    separate(group1, into = c("site1"), sep = "(?<=.)", remove = FALSE) %>% # Small hack to get site labels
    separate(group2, into = c("site2"), sep = "(?<=.)", remove = FALSE) %>% # Small hack to get site labels
    mutate(
      y_position = case_when(
        variable == "CO2" ~ max(sample_data_4$CO2, na.rm = TRUE) * 1.1, # Adjust these multipliers based on your data range
        variable == "N2O" ~ max(sample_data_4$N2O, na.rm = TRUE) * 1.1,
        variable == "CH4" ~ max(sample_data_4$CH4, na.rm = TRUE) * 1.1,
        TRUE ~ NA_real_ # Add more cases for other variables
      ),
      label = p.adj.signif
    )
  
  
  
  stat_pvalue_CO2 <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(CO2 ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() %>% 
    mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))
  
 
  
  
  
  
  
  
  # Create the ggplot
  CO2_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = CO2, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab(expression(CO[2] ~ (μg ~ day^{-1} ~ g^{-1} ~ dry ~ aggregate)))+
    xlab("") +
    facet_wrap(~ Site,
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "CO2"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    ggpubr::stat_pvalue_manual(stat_pvalue_CO2, label = "p.signif")
  
  print(CO2_plot)
  
  
  stat_pvalue_N2O <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(N2O ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() %>% 
    mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))
  
 
  
  
  
  N2O_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = N2O, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab(expression(N[2]*O ~ (μg ~ day^{-1} ~ g^{-1} ~ dry ~ aggregate)))+
    xlab("") +
    
    facet_wrap(~ Site,
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "N2O"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    ggpubr::stat_pvalue_manual(stat_pvalue_N2O, label = "p.signif")
  
  print(N2O_plot)
  
  
  
  stat_pvalue_CH4 <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(CH4 ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() 
  
 
  
  
  
  
  CH4_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = CH4, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab("CH4 (μg per day)") +
    ylab(expression(CH[4] ~ (μg ~ day^{-1} ~ g^{-1} ~ dry ~ aggregate)))+
    xlab("") +
    
    facet_wrap(~ Site,
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "CH4"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    ggpubr::stat_pvalue_manual(stat_pvalue_CH4, label = "p.signif")
  
  print(CH4_plot)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  list(CO2_plot=CO2_plot,
       N2O_plot=N2O_plot,
       CH4_plot=CH4_plot
  )
  
}

plot_DNA_Yield = function(sample_data_4){
  
 
  DNA_Yield<-sample_data_4 %>%
    ggplot(aes(x= as.factor(Site),y=Extraction.number, color=Aggregate))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 #position = position_dodge(width = 0.6), 
                 alpha = 0.2)+
    geom_point(position = position_dodge(width=0.8))+
    #ylim(NA,300)+
    ylab("respiration (μg per day)")+
    xlab("Field ID")+
    scale_x_discrete(labels = c(
      "O" = "Organic",
      "C" = "Conventional"))+
    #facet_wrap(~Addition)+
    theme_CKM2()
  
  
  list( DNA_Yield=DNA_Yield
  )
  
}

plot_biomass = function(sample_data_4){
  
  sample_data_4$Aggregate <- factor(sample_data_4$Aggregate, levels = c("A", "B", "C", "D", "Bulk"))
  
  response_variables <- c("MBC", "MBN", "TOC", "TN")
  
  desired_levels <- c("A", "B", "C", "D", "Bulk")
  sample_data_4$Aggregate <- factor(sample_data_4$Aggregate, levels = desired_levels)
  aggregate_levels <- levels(sample_data_4$Aggregate)
  comparison_pairs <- utils::combn(aggregate_levels, 2, simplify = FALSE)
  
  # Function to perform ANOVA and posthoc test, and extract results
  analyze_and_extract <- function(data, variable) {
    formula_str <- paste0(variable, " ~ Site")
    anova_model <- aov(as.formula(formula_str), data = data)
    anova_summary <- summary(anova_model)
    anova_p_value <- anova_summary[[1]]$`Pr(>F)`[1]
    
    posthoc_results <- NULL
    if (anova_p_value < 0.05) {
      posthoc_results <- data %>%
        group_by(Aggregate) %>%
        tukey_hsd(as.formula(formula_str)) %>%
        mutate(variable = variable) # Add the variable name for later use
    } else {
      # If ANOVA is not significant, create an empty tibble
      posthoc_results <- tibble(
        Aggregate = unique(data$Aggregate),
        term = "Site",
        group1 = NA_character_,
        group2 = NA_character_,
        estimate = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        p.adj = anova_p_value,
        variable = variable
      )
    }
    return(posthoc_results)
  }
  
  
  all_posthoc_results <- map_dfr(response_variables, ~ analyze_and_extract(sample_data_4, .x))
  
  
  all_posthoc_results_adjusted <- all_posthoc_results %>%
    group_by(variable, Aggregate) %>%
    adjust_pvalue(method = "BH") %>%
    ungroup() %>%
    mutate(p.adj.signif = case_when(
      p.adj <= 0.001 ~ "***",
      p.adj <= 0.01 ~ "**",
      p.adj <= 0.05 ~ "*",
      TRUE ~ "ns"
    ))
  
  
  plot_labels <- all_posthoc_results_adjusted %>%
    filter(p.adj < 0.05) %>% # Only keep significant comparisons for labeling
    separate(group1, into = c("site1"), sep = "(?<=.)", remove = FALSE) %>% # Small hack to get site labels
    separate(group2, into = c("site2"), sep = "(?<=.)", remove = FALSE) %>% # Small hack to get site labels
    mutate(
      y_position = case_when(
        variable == "MBC" ~ max(sample_data_4$MBC, na.rm = TRUE) * 1.1, # Adjust these multipliers based on your data range
        variable == "MBN" ~ max(sample_data_4$MBN, na.rm = TRUE) * 1.1,
        variable == "TOC" ~ max(sample_data_4$TOC, na.rm = TRUE) * 1.1,
        variable == "TN" ~ max(sample_data_4$TN, na.rm = TRUE) * 1.1,
        TRUE ~ NA_real_ # Add more cases for other variables
      ),
      label = p.adj.signif
    )
  
  stat_pvalue_MBC <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(MBC ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() %>% 
    mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))
  
 
  
  
  
  
  
  
  # Create the ggplot
  MBC_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = MBC, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab(expression(MBC ~ (μg ~ C ~ g^{-1} ~ dry ~ aggregate)))+
    xlab("Field ID") +
    
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "MBC"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    facet_wrap(~ Site,
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_MBC, label = "p.signif")
  
  print(MBC_plot)
  
  
  
  
  stat_pvalue_MBN <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(MBN ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() %>% 
    mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))
  
  
  
  
  
  
  
  
  MBN_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = MBN, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab(expression(MBN ~ (μg ~ N ~ g^{-1} ~ dry ~ aggregate)))+
    xlab("Field ID") +
    
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "MBN"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    facet_wrap(~ Site, # Allow independent Y-axes
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_MBN, label = "p.signif")
  
  print(MBN_plot)
  
  
  
  
  
  stat_pvalue_NPOC <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(TOC ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() %>% 
    mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))
  
  
  
  
  
  
  NPOC_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = TOC, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab(expression(NPOC ~ (μg ~ C ~ g^{-1} ~ dry ~ aggregate)))+
    xlab("Field ID") +
    
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "TOC"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    facet_wrap(~ Site, # Allow independent Y-axes
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_NPOC, label = "p.signif")
  
  print(NPOC_plot)
  
  
  
  stat_pvalue_TN <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(TN ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position()
  
  
  
  
  
  
  
  
  
  TN_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = TN, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab(expression(TDN~ (μg ~ N ~ g^{-1} ~ dry ~ aggregate)))+
    xlab("Field ID") +
    
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "TN"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    facet_wrap(~ Site, # Allow independent Y-axes
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_TN, label = "p.signif")
  
  print(TN_plot)
  
  list(MBC_plot=MBC_plot,
       MBN_plot=MBN_plot,
       NPOC_plot=NPOC_plot,
       TN_plot=TN_plot,
       all_posthoc_results_adjusted=all_posthoc_results_adjusted
  )
  
}

plot_MAOM <- function(sample_data_4){
  
  # ---- Factor order & response set ----
  sample_data_4$Aggregate <- factor(sample_data_4$Aggregate,
                                    levels = c("A","B","C","D","Bulk"))
  response_variables <- c(
    "fPOM_TC","fPOM_13C.","oPOM_TC","oPOM_13C.","MAOM_TC","MAOM_13C.",
    "fPOM_TN","fPOM_15N.","oPOM_TN","oPOM_15N.","MAOM_TN","MAOM_15N."
  )
  
  desired_levels <- c("A","B","C","D","Bulk")
  sample_data_4$Aggregate <- factor(sample_data_4$Aggregate, levels = desired_levels)
  
  sample_data_4<-  sample_data_4%>%
    mutate(fPOM_TC = fPOM_TC * proportion.fPOM.by.mass,    # Converting from per OM fraction to per aggregate
           oPOM_TC = oPOM_TC * proportion.oPOM.by.mass,
           MAOM_TC = MAOM_TC * proportion.MAOM.by.mass,
           fPOM_TN = fPOM_TN * proportion.fPOM.by.mass,
           oPOM_TN= oPOM_TN * proportion.oPOM.by.mass,
           MAOM_TN = MAOM_TN * proportion.MAOM.by.mass)
  
  
  
  
  # ---- ANOVA + Tukey to build label table (kept from your pattern) ----
  analyze_and_extract <- function(data, variable) {
    formula_str <- paste0("`", variable, "` ~ Site")        # backticks for safety
    anova_model  <- aov(as.formula(formula_str), data = data)
    anova_summary <- summary(anova_model)
    anova_p_value <- anova_summary[[1]]$`Pr(>F)`[1]
    
    if (isTRUE(anova_p_value < 0.05)) {
      data %>%
        dplyr::group_by(Aggregate) %>%
        rstatix::tukey_hsd(as.formula(formula_str)) %>%
        dplyr::mutate(variable = variable) %>%
        dplyr::ungroup()
    } else {
      tibble::tibble(
        Aggregate = unique(data$Aggregate),
        term      = "Site",
        group1    = NA_character_,
        group2    = NA_character_,
        estimate  = NA_real_,
        conf.low  = NA_real_,
        conf.high = NA_real_,
        p.adj     = anova_p_value,
        variable  = variable
      )
    }
  }
  
  all_posthoc_results <- purrr::map_dfr(response_variables, ~ analyze_and_extract(sample_data_4, .x))
  
  all_posthoc_results_adjusted <- all_posthoc_results %>%
    dplyr::group_by(variable, Aggregate) %>%
    rstatix::adjust_pvalue(method = "BH") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(p.adj.signif = dplyr::case_when(
      p.adj <= 0.001 ~ "***",
      p.adj <= 0.01  ~ "**",
      p.adj <= 0.05  ~ "*",
      TRUE ~ "ns"
    ))
  
  # Build y-positions for labels per variable using max of each response
  get_y_max <- function(df, var) {
    suppressWarnings(max(df[[var]], na.rm = TRUE))
  }
  
  plot_labels <- all_posthoc_results_adjusted %>%
    dplyr::filter(p.adj < 0.05) %>%
    tidyr::separate(group1, into = c("site1"), sep = "(?<=.)", remove = FALSE) %>%
    tidyr::separate(group2, into = c("site2"), sep = "(?<=.)", remove = FALSE) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      y_position = get_y_max(sample_data_4, variable) * 1.1,
      label      = p.adj.signif
    ) %>%
    dplyr::ungroup()
  
  # ---- Y labels ----
  ylabs <- list(
    "fPOM_TC"   = "fPOM Total C (g kg^-1 aggregate)",
    "oPOM_TC"   = "oPOM Total C (g kg^-1 aggregate)",
    "MAOM_TC"   = "MAOM Total C (g kg^-1 aggregate)",
    "fPOM_TN"   = "fPOM Total N (g kg^-1 aggregate)",
    "oPOM_TN"   = "oPOM Total N (g kg^-1 aggregate)",
    "MAOM_TN"   = "MAOM Total N (g kg^-1 aggregate)",
    
    # 13C / 15N as expressions
    "fPOM_13C." = expression(fPOM~{}^{13}*C~"(atom %)"),
    "oPOM_13C." = expression(oPOM~{}^{13}*C~"(atom %)"),
    "MAOM_13C." = expression(MAOM~{}^{13}*C~"(atom %)"),
    "fPOM_15N." = expression(fPOM~{}^{15}*N~"(atom %)"),
    "oPOM_15N." = expression(oPOM~{}^{15}*N~"(atom %)"),
    "MAOM_15N." = expression(MAOM~{}^{15}*N~"(atom %)")
  )
  
  # ---- One function to make any of the 12 panels ----
  make_one_plot <- function(var){
    
    # Pick y label: expression or character
    this_lab <- ylabs[[var]]
    if (is.null(this_lab)) this_lab <- var
    
    # Wilcoxon per Site for stat_pvalue_manual()
    stat_df <- sample_data_4 %>%
      dplyr::group_by(Site) %>%
      rstatix::wilcox_test(as.formula(paste0("`", var, "` ~ Aggregate"))) %>%
      dplyr::filter(p < 0.05) %>%
      rstatix::add_significance("p") %>%
      rstatix::add_y_position() %>%
      dplyr::mutate(
        y.position = seq(min(y.position), max(y.position), length.out = dplyr::n())
      )
    
    # Core box+points
    p <- sample_data_4 %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = as.factor(Aggregate),
          y = .data[[var]],
          color = Aggregate
        )
      ) +
      ggplot2::geom_boxplot(
        show.legend    = FALSE,
        outlier.colour = NA,
        outlier.fill   = NA,
        alpha          = 0.2
      ) +
      ggplot2::geom_point(
        position = ggplot2::position_dodge(width = 0.8)
      ) +
      ggplot2::ylab(this_lab) +                      # <<<< key change here
      ggplot2::xlab("Aaggregate") +
      ggplot2::scale_color_manual(
        values = cbPalette6,
        labels = c(
          "A"   = ">2 mm",
          "B"   = "2–0.25 mm",
          "C"   = "0.25–0.053 mm",
          "D"   = "<0.053 mm",
          "Bulk"= "Bulk"
        )
      ) +
      ggplot2::scale_x_discrete(
        labels = c(
          "A"   = ">2 mm",
          "B"   = "2–0.25 mm",
          "C"   = "0.25–0.053 mm",
          "D"   = "<0.053 mm",
          "Bulk"= "Bulk"
        )
      ) +
      theme_CKMBP()
    
    # Optional Tukey asterisks
    if (nrow(plot_labels %>% dplyr::filter(variable == var)) > 0) {
      p <- p +
        ggplot2::geom_text(
          data = plot_labels %>% dplyr::filter(variable == var),
          ggplot2::aes(
            x     = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2)))/2,
            y     = y_position,
            label = label,
            group = Aggregate
          ),
          position = ggplot2::position_dodge(width = 3),
          vjust    = 2
        )
    }
    
    # Facet by Site with per-facet p-value stars
    p <- p +
      ggplot2::facet_wrap(
        ~ Site,
        labeller = ggplot2::labeller(Site = c("C"="Conventional","O"="Organic"))
      ) +
      ggpubr::stat_pvalue_manual(stat_df, label = "p.signif")
    
    p
  }
  
  
  # ---- Build all plots & return as a named list ----
  plots <- purrr::map(response_variables, make_one_plot)
  names(plots) <- response_variables
  plots
  
  list(plots=plots,
    all_posthoc_results_adjusted=all_posthoc_results_adjusted)
}

plot_TC_TN<- function(sample_data_4){
  
  # ---- Factor order & response set ----
  sample_data_4$Aggregate <- factor(sample_data_4$Aggregate,
                                    levels = c("A","B","C","D","Bulk"))
  response_variables <- c(
    "TC","TN.1","C.","N."
  )
  
  desired_levels <- c("A","B","C","D","Bulk")
  sample_data_4$Aggregate <- factor(sample_data_4$Aggregate, levels = desired_levels)
  
  # ---- ANOVA + Tukey to build label table (kept from your pattern) ----
  analyze_and_extract <- function(data, variable) {
    formula_str <- paste0("`", variable, "` ~ Site")        # backticks for safety
    anova_model  <- aov(as.formula(formula_str), data = data)
    anova_summary <- summary(anova_model)
    anova_p_value <- anova_summary[[1]]$`Pr(>F)`[1]
    
    if (isTRUE(anova_p_value < 0.05)) {
      data %>%
        dplyr::group_by(Aggregate) %>%
        rstatix::tukey_hsd(as.formula(formula_str)) %>%
        dplyr::mutate(variable = variable) %>%
        dplyr::ungroup()
    } else {
      tibble::tibble(
        Aggregate = unique(data$Aggregate),
        term      = "Site",
        group1    = NA_character_,
        group2    = NA_character_,
        estimate  = NA_real_,
        conf.low  = NA_real_,
        conf.high = NA_real_,
        p.adj     = anova_p_value,
        variable  = variable
      )
    }
  }
  
  all_posthoc_results <- purrr::map_dfr(response_variables, ~ analyze_and_extract(sample_data_4, .x))
  
  all_posthoc_results_adjusted <- all_posthoc_results %>%
    dplyr::group_by(variable, Aggregate) %>%
    rstatix::adjust_pvalue(method = "BH") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(p.adj.signif = dplyr::case_when(
      p.adj <= 0.001 ~ "***",
      p.adj <= 0.01  ~ "**",
      p.adj <= 0.05  ~ "*",
      TRUE ~ "ns"
    ))
  
  # Build y-positions for labels per variable using max of each response
  get_y_max <- function(df, var) {
    suppressWarnings(max(df[[var]], na.rm = TRUE))
  }
  
  plot_labels <- all_posthoc_results_adjusted %>%
    dplyr::filter(p.adj < 0.05) %>%
    tidyr::separate(group1, into = c("site1"), sep = "(?<=.)", remove = FALSE) %>%
    tidyr::separate(group2, into = c("site2"), sep = "(?<=.)", remove = FALSE) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      y_position = get_y_max(sample_data_4, variable) * 1.1,
      label      = p.adj.signif
    ) %>%
    dplyr::ungroup()
  
  # ---- Y labels ----
  ylabs <- list(
    "TC"   = "Soil Total C (g kg^-1 aggregate)",
    "TN.1"   = "Soil Total N (g kg^-1 aggregate)",
    # 13C / 15N as expressions
    "C." = expression(Soil~{}^{13}*C~"(atom %)"),
    "N." = expression(Soil~{}^{15}*N~"(atom %)"),
   
  )
  
  # ---- One function to make any of the 12 panels ----
  make_one_plot <- function(var){
    
    # Pick y label: expression or character
    this_lab <- ylabs[[var]]
    if (is.null(this_lab)) this_lab <- var
    
    # Wilcoxon per Site for stat_pvalue_manual()
    stat_df <- sample_data_4 %>%
      dplyr::group_by(Site) %>%
      rstatix::wilcox_test(as.formula(paste0("`", var, "` ~ Aggregate"))) %>%
      dplyr::filter(p < 0.05) %>%
      rstatix::add_significance("p") %>%
      rstatix::add_y_position() %>%
      dplyr::mutate(
        y.position = seq(min(y.position), max(y.position), length.out = dplyr::n())
      )
    
    # Core box+points
    p <- sample_data_4 %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = as.factor(Aggregate),
          y = .data[[var]],
          color = Aggregate
        )
      ) +
      ggplot2::geom_boxplot(
        show.legend    = FALSE,
        outlier.colour = NA,
        outlier.fill   = NA,
        alpha          = 0.2
      ) +
      ggplot2::geom_point(
        position = ggplot2::position_dodge(width = 0.8)
      ) +
      ggplot2::ylab(this_lab) +                      # <<<< key change here
      ggplot2::xlab("Aaggregate") +
      ggplot2::scale_color_manual(
        values = cbPalette6,
        labels = c(
          "A"   = ">2 mm",
          "B"   = "2–0.25 mm",
          "C"   = "0.25–0.053 mm",
          "D"   = "<0.053 mm",
          "Bulk"= "Bulk"
        )
      ) +
      ggplot2::scale_x_discrete(
        labels = c(
          "A"   = ">2 mm",
          "B"   = "2–0.25 mm",
          "C"   = "0.25–0.053 mm",
          "D"   = "<0.053 mm",
          "Bulk"= "Bulk"
        )
      ) +
      theme_CKMBP()
    
    # Optional Tukey asterisks
    if (nrow(plot_labels %>% dplyr::filter(variable == var)) > 0) {
      p <- p +
        ggplot2::geom_text(
          data = plot_labels %>% dplyr::filter(variable == var),
          ggplot2::aes(
            x     = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2)))/2,
            y     = y_position,
            label = label,
            group = Aggregate
          ),
          position = ggplot2::position_dodge(width = 3),
          vjust    = 2
        )
    }
    
    # Facet by Site with per-facet p-value stars
    p <- p +
      ggplot2::facet_wrap(
        ~ Site,
        labeller = ggplot2::labeller(Site = c("C"="Conventional","O"="Organic"))
      ) +
      ggpubr::stat_pvalue_manual(stat_df, label = "p.signif")
    
    p
  }
  
  
  # ---- Build all plots & return as a named list ----
  plots <- purrr::map(response_variables, make_one_plot)
  names(plots) <- response_variables
  plots
  
  list(plots=plots,
       all_posthoc_results_adjusted=all_posthoc_results_adjusted)
}

plot_MAOM_PROP = function(sample_data_4){
  
  
  sample_data_4$Aggregate <- factor(sample_data_4$Aggregate, levels = c("A", "B", "C", "D", "Bulk"))
  
  response_variables <- c("fPOM_TC","fPOM_13C.","fPOM_TN","fPOM_15N.","oPOM_TC","oPOM_13C.","oPOM_TN","oPOM_15N.","MAOM_TC","MAOM_13C.","MAOM_TN",
                          "MAOM_15N.", "fPOM..g.","oPOM.g.","MAOM","proportion.fPOM.by.mass","proportion.oPOM.by.mass",
                          "proportion.MAOM.by.mass")
  
  desired_levels <- c("A", "B", "C", "D", "Bulk")
  sample_data_4$Aggregate <- factor(sample_data_4$Aggregate, levels = desired_levels)
  aggregate_levels <- levels(sample_data_4$Aggregate)
  comparison_pairs <- utils::combn(aggregate_levels, 2, simplify = FALSE)
  
  # Function to perform ANOVA and posthoc test, and extract results
  analyze_and_extract <- function(data, variable) {
    formula_str <- paste0(variable, " ~ Site")
    anova_model <- aov(as.formula(formula_str), data = data)
    anova_summary <- summary(anova_model)
    anova_p_value <- anova_summary[[1]]$`Pr(>F)`[1]
    
    posthoc_results <- NULL
    if (anova_p_value < 0.05) {
      posthoc_results <- data %>%
        group_by(Aggregate) %>%
        tukey_hsd(as.formula(formula_str)) %>%
        mutate(variable = variable) # Add the variable name for later use
    } else {
      # If ANOVA is not significant, create an empty tibble
      posthoc_results <- tibble(
        Aggregate = unique(data$Aggregate),
        term = "Site",
        group1 = NA_character_,
        group2 = NA_character_,
        estimate = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        p.adj = anova_p_value,
        variable = variable
      )
    }
    return(posthoc_results)
  }
  
  
  all_posthoc_results <- map_dfr(response_variables, ~ analyze_and_extract(sample_data_4, .x))
  
  
  all_posthoc_results_adjusted <- all_posthoc_results %>%
    group_by(variable, Aggregate) %>%
    adjust_pvalue(method = "BH") %>%
    ungroup() %>%
    mutate(p.adj.signif = case_when(
      p.adj <= 0.001 ~ "***",
      p.adj <= 0.01 ~ "**",
      p.adj <= 0.05 ~ "*",
      TRUE ~ "ns"
    ))
  
  
  
  
  
  
  SD1<- sample_data_4%>%
    filter(Addition=="16O")%>%
    select(proportion.fPOM.by.mass,proportion.oPOM.by.mass,proportion.MAOM.by.mass,Site,Aggregate)%>%
    pivot_longer("proportion.fPOM.by.mass":"proportion.MAOM.by.mass")
  
  avg_values <- SD1 %>%
    filter(Aggregate != "")%>%
    group_by(Site,Aggregate, name) %>%
    summarize(avg_value = mean(value, na.rm = TRUE), .groups = 'drop')
  
  
  avg_values2 <- avg_values %>%
    mutate(
      name = factor(name, levels = c("proportion.fPOM.by.mass",
                                     "proportion.oPOM.by.mass",
                                     "proportion.MAOM.by.mass")),
      Aggregate = factor(Aggregate, levels = c("A","B","C","D","Bulk")),
      Site = factor(Site, levels = c("O","C"))
    )
  
  gg_MAOM_proportion <- ggplot(avg_values2, aes(x = Aggregate, y = avg_value, fill = name)) +
    geom_col() +
    labs(y = "Proportion OM of aggregate", x = "Aggregate", fill = "OM fraction") +
    scale_fill_manual(values = cbPalette3,
                      labels = c("proportion.fPOM.by.mass" = "fPOM",
                                 "proportion.oPOM.by.mass" = "oPOM",
                                 "proportion.MAOM.by.mass" = "MAOM")) +
    facet_wrap(~Site, labeller = labeller(Site = c(O = "Organic", C = "Conventional"))) +
    theme_CKM2() +
    geom_text(
      data = avg_values2 %>% filter(avg_value != 0),
      aes(label = round(avg_value, 2)),
      position = position_stack(vjust = 0.8),
      color = "black", size = 4
    )
 
  
  list(gg_MAOM_proportion=gg_MAOM_proportion,
       all_posthoc_results_adjusted=all_posthoc_results_adjusted
      
  )
  
}

plot_Enzyme = function(sample_data_4){
  
  sample_data_4$Aggregate <- factor(sample_data_4$Aggregate, levels = c("A", "B", "C", "D", "Bulk"))
  desired_levels <- c("A", "B", "C", "D", "Bulk")
  sample_data_4$Aggregate <- factor(sample_data_4$Aggregate, levels = desired_levels)
  aggregate_levels <- levels(sample_data_4$Aggregate)
  comparison_pairs <- utils::combn(aggregate_levels, 2, simplify = FALSE)
  
  sample_data_4$NDEMAND<- sample_data_4$BG/(sample_data_4$BG+sample_data_4$LAP+sample_data_4$NAG)
  sample_data_4$PDEMAND<- sample_data_4$BG/(sample_data_4$BG+sample_data_4$PHOS)
  response_variables <- c("BG", "CBH", "LAP", "NAG", "PHOS")
  # Function to perform ANOVA and posthoc test, and extract results
  analyze_and_extract <- function(data, variable) {
    formula_str <- paste0(variable, " ~ Site")
    anova_model <- aov(as.formula(formula_str), data = data)
    anova_summary <- summary(anova_model)
    anova_p_value <- anova_summary[[1]]$`Pr(>F)`[1]
    
    posthoc_results <- NULL
    if (anova_p_value < 0.05) {
      posthoc_results <- data %>%
        group_by(Aggregate) %>%
        tukey_hsd(as.formula(formula_str)) %>%
        mutate(variable = variable) # Add the variable name for later use
    } else {
      # If ANOVA is not significant, create an empty tibble
      posthoc_results <- tibble(
        Aggregate = unique(data$Aggregate),
        term = "Site",
        group1 = NA_character_,
        group2 = NA_character_,
        estimate = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        p.adj = anova_p_value,
        variable = variable
      )
    }
    return(posthoc_results)
  }
  
  
  all_posthoc_results <- map_dfr(response_variables, ~ analyze_and_extract(sample_data_4, .x))
  
  
  all_posthoc_results_adjusted <- all_posthoc_results %>%
    group_by(variable, Aggregate) %>%
    adjust_pvalue(method = "BH") %>%
    ungroup() %>%
    mutate(p.adj.signif = case_when(
      p.adj <= 0.001 ~ "***",
      p.adj <= 0.01 ~ "**",
      p.adj <= 0.05 ~ "*",
      TRUE ~ "ns"
    ))
  
  
  plot_labels <- all_posthoc_results_adjusted %>%
    filter(p.adj < 0.05) %>% # Only keep significant comparisons for labeling
    separate(group1, into = c("site1"), sep = "(?<=.)", remove = FALSE) %>% # Small hack to get site labels
    separate(group2, into = c("site2"), sep = "(?<=.)", remove = FALSE) %>% # Small hack to get site labels
    mutate(
      y_position = case_when(
        variable == "BG" ~ max(sample_data_4$BG, na.rm = TRUE) * 1.1, # Adjust these multipliers based on your data range
        variable == "CBH" ~ max(sample_data_4$CBH, na.rm = TRUE) * 1.1,
        variable == "LAP" ~ max(sample_data_4$LAP, na.rm = TRUE) * 1.1,
        variable == "NAG" ~ max(sample_data_4$NAG, na.rm = TRUE) * 1.1,
        variable == "PHOS" ~ max(sample_data_4$PHOS, na.rm = TRUE) * 1.1,
        TRUE ~ NA_real_ # Add more cases for other variables
      ),
      label = p.adj.signif
    )
  
  stat_pvalue_BG <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(BG ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() %>% 
    mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))
  
  
  
  
  BG_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = BG, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab(expression(BG(nmol ~ g^{-1} ~ aggregate ~ hr^{-1})))+
    xlab("Field ID") +
    
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "BG"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    facet_wrap(~ Site, # Allow independent Y-axes
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_BG, label = "p.signif")
  
  
  stat_pvalue_CBH <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(CBH ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() %>% 
    mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))
  
  
  
  
  
  
  
  CBH_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = CBH, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab(expression(CBH(nmol ~ g^{-1} ~ aggregate ~ hr^{-1})))+
    xlab("Field ID") +
    
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "CBH"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    facet_wrap(~ Site, # Allow independent Y-axes
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_CBH, label = "p.signif")
  
  
  
  stat_pvalue_LAP <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(LAP ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() %>% 
    mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))
  
  
 
  
  
  
  
  
  LAP_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = LAP, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab(expression(LAP(nmol ~ g^{-1} ~ aggregate ~ hr^{-1})))+
    xlab("Field ID") +
    
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "LAP"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    facet_wrap(~ Site, # Allow independent Y-axes
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_LAP, label = "p.signif")
  
  
  
  stat_pvalue_NAG <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(NAG ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() %>% 
    mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))
  
  
  
  
  
  
  
  NAG_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = NAG, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab(expression(NAG(nmol ~ g^{-1} ~ aggregate ~ hr^{-1})))+
    xlab("Field ID") +
    
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "NAG"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    facet_wrap(~ Site, # Allow independent Y-axes
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_NAG, label = "p.signif")
  
  
  stat_pvalue_PHOS <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(PHOS ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() 
  
  
  
  
  
  PHOS_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = PHOS, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab(expression(PHOS(nmol ~ g^{-1} ~ aggregate ~ hr^{-1})))+
    xlab("Field ID") +
    
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "PHOS"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    facet_wrap(~ Site, # Allow independent Y-axes
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_PHOS, label = "p.signif")
  
  
  PHOSL<-sample_data_4 %>%
    ggplot(aes(x= as.factor(Aggregate),y=PHOS, color=Aggregate))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 #position = position_dodge(width = 0.6), 
                 alpha = 0.2)+
    geom_point(position = position_dodge(width=0.8))+
    #ylim(NA,300)+
    ylab(expression(PHOS(nmol ~ g^{-1} ~ aggregate ~ hr^{-1})))+
    xlab("")+
    scale_x_discrete(labels = c(
      "O" = "Organic",
      "C" = "Conventional"))+
    #facet_wrap(~Addition)+
    scale_color_manual(values= cbPalette6,labels = c("A" = ">2 mm",
                                                     "B" = "2-0.25 mm",
                                                     "C" = "0.25-0.053 mm",
                                                     "D"= "< 0.053 mm",
                                                     "Bulk"="Bulk"))+
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP()
    
  
  legend<-get_legend(PHOSL)
  
  gg_enzyme <- plot_grid(BG_plot,CBH_plot,LAP_plot,NAG_plot,PHOS_plot,legend,
                     nrow = 2, align = "v",
                     label_size = 14, vjust= -0.5)
  
  
  Nextract<-sample_data_4 %>%
    ggplot(aes(x= as.factor(Aggregate),y=Extraction.number))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 #position = position_dodge(width = 0.6), 
                 alpha = 0.2)+
    geom_point(position = position_dodge(width=0.8))+
    #ylim(NA,300)+
    ylab("# of extractions")+
    xlab("")+
    scale_x_discrete(labels = c(
      "O" = "Organic",
      "C" = "Conventional"))+
    #facet_wrap(~Addition)+
    scale_color_manual(values= cbPalette6,labels = c("A" = ">2 mm",
                                                     "B" = "2-0.25 mm",
                                                     "C" = "0.25-0.053 mm",
                                                     "D"= "< 0.053 mm",
                                                     "Bulk"="Bulk"))+
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKM2()
  
  
  
  
 
 
  
  list(BG_plot=BG_plot,
       CBH_plot=CBH_plot,
       LAP_plot=LAP_plot,
       NAG_plot=NAG_plot,
       PHOS_plot=PHOS_plot,
       all_posthoc_results_adjusted=all_posthoc_results_adjusted
  )
  
}

plot_Enzyme_ve = function(sample_data_4){
  
  sample_data_4$Aggregate <- factor(sample_data_4$Aggregate, levels = c("A", "B", "C", "D", "Bulk"))
  desired_levels <- c("A", "B", "C", "D", "Bulk")
  sample_data_4$Aggregate <- factor(sample_data_4$Aggregate, levels = desired_levels)
  aggregate_levels <- levels(sample_data_4$Aggregate)
  comparison_pairs <- utils::combn(aggregate_levels, 2, simplify = FALSE)
  
  sample_data_4$NDEMAND<- sample_data_4$BG/(sample_data_4$BG+sample_data_4$LAP+sample_data_4$NAG)
  sample_data_4$PDEMAND<- sample_data_4$BG/(sample_data_4$BG+sample_data_4$PHOS)
  sample_data_4<-sample_data_4%>%
    mutate(CN= BG/(BG+LAP+NAG),
           CP= BG/(BG+PHOS),
           NP_length= sqrt(CN^2+CP^2),
           NP_angle= atan2(CP,CN))

  response_variables <- c("BG", "CBH", "LAP", "NAG", "PHOS", "CN","CP","NP_length","NP_angle")
  # Function to perform ANOVA and posthoc test, and extract results
  analyze_and_extract <- function(data, variable) {
    formula_str <- paste0(variable, " ~ Site")
    anova_model <- aov(as.formula(formula_str), data = data)
    anova_summary <- summary(anova_model)
    anova_p_value <- anova_summary[[1]]$`Pr(>F)`[1]
    
    posthoc_results <- NULL
    if (anova_p_value < 0.05) {
      posthoc_results <- data %>%
        group_by(Aggregate) %>%
        tukey_hsd(as.formula(formula_str)) %>%
        mutate(variable = variable) # Add the variable name for later use
    } else {
      # If ANOVA is not significant, create an empty tibble
      posthoc_results <- tibble(
        Aggregate = unique(data$Aggregate),
        term = "Site",
        group1 = NA_character_,
        group2 = NA_character_,
        estimate = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        p.adj = anova_p_value,
        variable = variable
      )
    }
    return(posthoc_results)
  }
  
  
  all_posthoc_results <- map_dfr(response_variables, ~ analyze_and_extract(sample_data_4, .x))
  
  
  all_posthoc_results_adjusted <- all_posthoc_results %>%
    group_by(variable, Aggregate) %>%
    adjust_pvalue(method = "BH") %>%
    ungroup() %>%
    mutate(p.adj.signif = case_when(
      p.adj <= 0.001 ~ "***",
      p.adj <= 0.01 ~ "**",
      p.adj <= 0.05 ~ "*",
      TRUE ~ "ns"
    ))
  
  
  plot_labels <- all_posthoc_results_adjusted %>%
    filter(p.adj < 0.05) %>% # Only keep significant comparisons for labeling
    separate(group1, into = c("site1"), sep = "(?<=.)", remove = FALSE) %>% # Small hack to get site labels
    separate(group2, into = c("site2"), sep = "(?<=.)", remove = FALSE) %>% # Small hack to get site labels
    mutate(
      y_position = case_when(
        variable == "BG" ~ max(sample_data_4$BG, na.rm = TRUE) * 1.1, # Adjust these multipliers based on your data range
        variable == "CBH" ~ max(sample_data_4$CBH, na.rm = TRUE) * 1.1,
        variable == "LAP" ~ max(sample_data_4$LAP, na.rm = TRUE) * 1.1,
        variable == "NAG" ~ max(sample_data_4$NAG, na.rm = TRUE) * 1.1,
        variable == "PHOS" ~ max(sample_data_4$PHOS, na.rm = TRUE) * 1.1,
        TRUE ~ NA_real_ # Add more cases for other variables
      ),
      label = p.adj.signif
    )
  
  stat_pvalue_BG <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(BG ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() %>% 
    mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))
  
  
  
  
  BG_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = BG, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab(expression(BG(nmol ~ g^{-1} ~ aggregate ~ hr^{-1})))+
    xlab("Field ID") +
    
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "BG"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    facet_wrap(~ Site, # Allow independent Y-axes
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_BG, label = "p.signif")
  
  
  stat_pvalue_CBH <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(CBH ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() %>% 
    mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))
  
  
  
  
  
  
  
  CBH_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = CBH, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab(expression(CBH(nmol ~ g^{-1} ~ aggregate ~ hr^{-1})))+
    xlab("Field ID") +
    
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "CBH"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    facet_wrap(~ Site, # Allow independent Y-axes
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_CBH, label = "p.signif")
  
  
  
  stat_pvalue_LAP <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(LAP ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() %>% 
    mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))
  
  
  
  
  
  
  
  
  LAP_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = LAP, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab(expression(LAP(nmol ~ g^{-1} ~ aggregate ~ hr^{-1})))+
    xlab("Field ID") +
    
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "LAP"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    facet_wrap(~ Site, # Allow independent Y-axes
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_LAP, label = "p.signif")
  
  
  
  stat_pvalue_NAG <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(NAG ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() %>% 
    mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))
  
  
  
  
  
  
  
  NAG_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = NAG, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab(expression(NAG(nmol ~ g^{-1} ~ aggregate ~ hr^{-1})))+
    xlab("Field ID") +
    
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "NAG"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    facet_wrap(~ Site, # Allow independent Y-axes
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_NAG, label = "p.signif")
  
  
  stat_pvalue_PHOS <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(PHOS ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() 
  
  
  
  
  
  PHOS_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = PHOS, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab(expression(PHOS(nmol ~ g^{-1} ~ aggregate ~ hr^{-1})))+
    xlab("Field ID") +
    
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "PHOS"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    facet_wrap(~ Site, # Allow independent Y-axes
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_PHOS, label = "p.signif")
  
  
  PHOSL<-sample_data_4 %>%
    ggplot(aes(x= as.factor(Aggregate),y=PHOS, color=Aggregate))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 #position = position_dodge(width = 0.6), 
                 alpha = 0.2)+
    geom_point(position = position_dodge(width=0.8))+
    #ylim(NA,300)+
    ylab(expression(PHOS(nmol ~ g^{-1} ~ aggregate ~ hr^{-1})))+
    xlab("")+
    scale_x_discrete(labels = c(
      "O" = "Organic",
      "C" = "Conventional"))+
    #facet_wrap(~Addition)+
    scale_color_manual(values= cbPalette6,labels = c("A" = ">2 mm",
                                                     "B" = "2-0.25 mm",
                                                     "C" = "0.25-0.053 mm",
                                                     "D"= "< 0.053 mm",
                                                     "Bulk"="Bulk"))+
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP()
  
  
  legend<-get_legend(PHOSL)
  
  gg_enzyme <- plot_grid(BG_plot,CBH_plot,LAP_plot,NAG_plot,PHOS_plot,legend,
                         nrow = 2, align = "v",
                         label_size = 14, vjust= -0.5)
  
  
  Nextract<-sample_data_4 %>%
    ggplot(aes(x= as.factor(Aggregate),y=Extraction.number))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 #position = position_dodge(width = 0.6), 
                 alpha = 0.2)+
    geom_point(position = position_dodge(width=0.8))+
    #ylim(NA,300)+
    ylab("# of extractions")+
    xlab("")+
    scale_x_discrete(labels = c(
      "O" = "Organic",
      "C" = "Conventional"))+
    #facet_wrap(~Addition)+
    scale_color_manual(values= cbPalette6,labels = c("A" = ">2 mm",
                                                     "B" = "2-0.25 mm",
                                                     "C" = "0.25-0.053 mm",
                                                     "D"= "< 0.053 mm",
                                                     "Bulk"="Bulk"))+
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKM2()
  
  
  
  
  
  
  
  list(BG_plot=BG_plot,
       CBH_plot=CBH_plot,
       LAP_plot=LAP_plot,
       NAG_plot=NAG_plot,
       PHOS_plot=PHOS_plot,
       all_posthoc_results_adjusted=all_posthoc_results_adjusted
  )
  
}

plot_FDA = function(sample_data_4){
  
  sample_data_4$Aggregate <- factor(sample_data_4$Aggregate, levels = c("A", "B", "C", "D", "Bulk"))
  response_variables <- c("FDA")
  desired_levels <- c("A", "B", "C", "D", "Bulk")
  sample_data_4$Aggregate <- factor(sample_data_4$Aggregate, levels = desired_levels)
  aggregate_levels <- levels(sample_data_4$Aggregate)
  comparison_pairs <- utils::combn(aggregate_levels, 2, simplify = FALSE)
  # Function to perform ANOVA and posthoc test, and extract results
  analyze_and_extract <- function(data, variable) {
    formula_str <- paste0(variable, " ~ Site")
    anova_model <- aov(as.formula(formula_str), data = data)
    anova_summary <- summary(anova_model)
    anova_p_value <- anova_summary[[1]]$`Pr(>F)`[1]
    
    posthoc_results <- NULL
    if (anova_p_value < 0.05) {
      posthoc_results <- data %>%
        group_by(Aggregate) %>%
        tukey_hsd(as.formula(formula_str)) %>%
        mutate(variable = variable) # Add the variable name for later use
    } else {
      # If ANOVA is not significant, create an empty tibble
      posthoc_results <- tibble(
        Aggregate = unique(data$Aggregate),
        term = "Site",
        group1 = NA_character_,
        group2 = NA_character_,
        estimate = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        p.adj = anova_p_value,
        variable = variable
      )
    }
    return(posthoc_results)
  }
  
  
  all_posthoc_results <- map_dfr(response_variables, ~ analyze_and_extract(sample_data_4, .x))
  
  
  all_posthoc_results_adjusted <- all_posthoc_results %>%
    group_by(variable, Aggregate) %>%
    adjust_pvalue(method = "BH") %>%
    ungroup() %>%
    mutate(p.adj.signif = case_when(
      p.adj <= 0.001 ~ "***",
      p.adj <= 0.01 ~ "**",
      p.adj <= 0.05 ~ "*",
      TRUE ~ "ns"
    ))
  
  
  plot_labels <- all_posthoc_results_adjusted %>%
    filter(p.adj < 0.05) %>% # Only keep significant comparisons for labeling
    separate(group1, into = c("site1"), sep = "(?<=.)", remove = FALSE) %>% # Small hack to get site labels
    separate(group2, into = c("site2"), sep = "(?<=.)", remove = FALSE) %>% # Small hack to get site labels
    mutate(
      y_position = case_when(
        variable == "FDA" ~ max(sample_data_4$FDA, na.rm = TRUE) * 1.1, # Adjust these multipliers based on your data range
        TRUE ~ NA_real_ # Add more cases for other variables
      ),
      label = p.adj.signif
    )
  
  
  stat_pvalue_FDA <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(FDA ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() 
  
  
  
  
  
  
  
  
  
  
  
  
  FDA_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = FDA, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab(expression(FDA ~ (μg ~ hr^{-1} ~ g^{-1} ~ dry ~ aggregate)))+
    xlab("Field ID") +
    
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "FDA"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    facet_wrap(~ Site, # Allow independent Y-axes
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_FDA, label = "p.signif")
  
  
  
  
  list(FDA_plot=FDA_plot,
       all_posthoc_results_adjusted=all_posthoc_results_adjusted
  )
  
}

plot_POXC = function(sample_data_4){
  
  sample_data_4$Aggregate <- factor(sample_data_4$Aggregate, levels = c("A", "B", "C", "D", "Bulk"))
  response_variables <- c("POXC")
  desired_levels <- c("A", "B", "C", "D", "Bulk")
  sample_data_4$Aggregate <- factor(sample_data_4$Aggregate, levels = desired_levels)
  aggregate_levels <- levels(sample_data_4$Aggregate)
  comparison_pairs <- utils::combn(aggregate_levels, 2, simplify = FALSE)
  # Function to perform ANOVA and posthoc test, and extract results
  analyze_and_extract <- function(data, variable) {
    formula_str <- paste0(variable, " ~ Site")
    anova_model <- aov(as.formula(formula_str), data = data)
    anova_summary <- summary(anova_model)
    anova_p_value <- anova_summary[[1]]$`Pr(>F)`[1]
    
    posthoc_results <- NULL
    if (anova_p_value < 0.05) {
      posthoc_results <- data %>%
        group_by(Aggregate) %>%
        tukey_hsd(as.formula(formula_str)) %>%
        mutate(variable = variable) # Add the variable name for later use
    } else {
      # If ANOVA is not significant, create an empty tibble
      posthoc_results <- tibble(
        Aggregate = unique(data$Aggregate),
        term = "Site",
        group1 = NA_character_,
        group2 = NA_character_,
        estimate = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        p.adj = anova_p_value,
        variable = variable
      )
    }
    return(posthoc_results)
  }
  
  
  all_posthoc_results <- map_dfr(response_variables, ~ analyze_and_extract(sample_data_4, .x))
  
  
  all_posthoc_results_adjusted <- all_posthoc_results %>%
    group_by(variable, Aggregate) %>%
    adjust_pvalue(method = "BH") %>%
    ungroup() %>%
    mutate(p.adj.signif = case_when(
      p.adj <= 0.001 ~ "***",
      p.adj <= 0.01 ~ "**",
      p.adj <= 0.05 ~ "*",
      TRUE ~ "ns"
    ))
  
  
  plot_labels <- all_posthoc_results_adjusted %>%
    filter(p.adj < 0.05) %>% # Only keep significant comparisons for labeling
    separate(group1, into = c("site1"), sep = "(?<=.)", remove = FALSE) %>% # Small hack to get site labels
    separate(group2, into = c("site2"), sep = "(?<=.)", remove = FALSE) %>% # Small hack to get site labels
    mutate(
      y_position = case_when(
        variable == "POXC" ~ max(sample_data_4$POXC, na.rm = TRUE) * 1.1, # Adjust these multipliers based on your data range
        TRUE ~ NA_real_ # Add more cases for other variables
      ),
      label = p.adj.signif
    )
  
  sample_data_4$Site<-as.factor(sample_data_4$Site)
  sample_data_4$Aggregate<-as.factor(sample_data_4$Aggregate)
  
  
  
  
  stat_pvalue_POXC <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(POXC ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() 
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  POXC_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = POXC, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab(expression(POXC ~ (μg ~  g^{-1} ~ dry ~ aggregate)))+
    xlab("") +
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "POXC"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    
    facet_wrap(~ Site, # Allow independent Y-axes
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_POXC, label = "p.signif")
    
    
  
  
  list(POXC_plot=POXC_plot,
       all_posthoc_results_adjusted=all_posthoc_results_adjusted
  )
  
}

plot_soil = function(sample_data_4,sample_data_11){
  
  # Combine data frames so that 11 has the sample data
  sample_data_4<- sample_data_4 %>%
    left_join(sample_data_11, by= "ID")
  
  ## Graphing "NO3", "NH4", "OlsenP", "X_Ca","X_Mg", "X_Na", "pH", "CEC", "LOI", "X_K"
  sample_data_4$Aggregate <- factor(sample_data_4$Aggregate, levels = c("A", "B", "C", "D", "Bulk"))
  
  response_variables <- c("NO3", "NH4", "OlsenP", "X_Ca","X_Mg", "X_Na", "pH", "CEC", "LOI","X_K")
  
  desired_levels <- c("A", "B", "C", "D", "Bulk")
  sample_data_4$Aggregate <- factor(sample_data_4$Aggregate, levels = desired_levels)
  aggregate_levels <- levels(sample_data_4$Aggregate)
  comparison_pairs <- utils::combn(aggregate_levels, 2, simplify = FALSE)
  
  # Function to perform ANOVA and posthoc test, and extract results
  analyze_and_extract <- function(data, variable) {
    formula_str <- paste0(variable, " ~ Site")
    anova_model <- aov(as.formula(formula_str), data = data)
    anova_summary <- summary(anova_model)
    anova_p_value <- anova_summary[[1]]$`Pr(>F)`[1]
    
    posthoc_results <- NULL
    if (anova_p_value < 0.05) {
      posthoc_results <- data %>%
        group_by(Aggregate) %>%
        tukey_hsd(as.formula(formula_str)) %>%
        mutate(variable = variable) # Add the variable name for later use
    } else {
      # If ANOVA is not significant, create an empty tibble
      posthoc_results <- tibble(
        Aggregate = unique(data$Aggregate),
        term = "Site",
        group1 = NA_character_,
        group2 = NA_character_,
        estimate = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        p.adj = anova_p_value,
        variable = variable
      )
    }
    return(posthoc_results)
  }
  
  
  all_posthoc_results <- map_dfr(response_variables, ~ analyze_and_extract(sample_data_4, .x))
  
  
  all_posthoc_results_adjusted <- all_posthoc_results %>%
    group_by(variable, Aggregate) %>%
    adjust_pvalue(method = "BH") %>%
    ungroup() %>%
    mutate(p.adj.signif = case_when(
      p.adj <= 0.001 ~ "***",
      p.adj <= 0.01 ~ "**",
      p.adj <= 0.05 ~ "*",
      TRUE ~ "ns"
    ))
  
  
  plot_labels <- all_posthoc_results_adjusted %>%
    filter(p.adj < 0.05) %>% # Only keep significant comparisons for labeling
    separate(group1, into = c("site1"), sep = "(?<=.)", remove = FALSE) %>% # Small hack to get site labels
    separate(group2, into = c("site2"), sep = "(?<=.)", remove = FALSE) %>% # Small hack to get site labels
    mutate(
      y_position = case_when(
        variable == "NO3" ~ max(sample_data_4$NO3, na.rm = TRUE) * 1.1, # Adjust these multipliers based on your data range
        variable == "NH4" ~ max(sample_data_4$NH4, na.rm = TRUE) * 1.1,
        variable == "OlsenP" ~ max(sample_data_4$OlsenP, na.rm = TRUE) * 1.1,
        variable == "X_Ca" ~ max(sample_data_4$X_Ca, na.rm = TRUE) * 1.1,
        variable == "X_Mg" ~ max(sample_data_4$X_Mg, na.rm = TRUE) * 1.1,
        variable == "X_Na" ~ max(sample_data_4$X_Na, na.rm = TRUE) * 1.1,
        variable == "pH" ~ max(sample_data_4$pH, na.rm = TRUE) * 1.1,
        variable == "CEC" ~ max(sample_data_4$CEC, na.rm = TRUE) * 1.1,
        variable == "LOI" ~ max(sample_data_4$LOI, na.rm = TRUE) * 1.1,
        variable == "X_K" ~ max(sample_data_4$X_K, na.rm = TRUE) * 1.1,
        TRUE ~ NA_real_ # Add more cases for other variables
      ),
      label = p.adj.signif
    )
  
  
  
  
  
  
  
  ### NO3
  
  stat_pvalue_NO3 <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(NO3 ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() %>% 
    mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))
  
  # Create the ggplot
  NO3_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = NO3, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab("NO3 (ppm)") +
    xlab("Field ID") +
    
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "NO3"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    facet_wrap(~ Site,
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_NO3, label = "p.signif")
  
  print(NO3_plot)
  
  
  
  ### NH4 
  stat_pvalue_NH4 <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(NH4 ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() %>% 
    mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))
  
  NH4_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = NH4, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab("NH4 (ppm)") +
    xlab("Field ID") +
    
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "NH4"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    facet_wrap(~ Site, # Allow independent Y-axes
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_NH4, label = "p.signif")
  
  print(NH4_plot)
  
  
  
  
  ### OlsonP
  
  stat_pvalue_OlsenP <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(OlsenP ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() %>% 
    mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))
  

  OlsenP_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = OlsenP, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab("OlsenP (ppm)") +
    xlab("Field ID") +
    
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "OlsenP"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    facet_wrap(~ Site, # Allow independent Y-axes
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_OlsenP, label = "p.signif")
  
  print(OlsenP_plot)
  
  
  ### X_Ca
  
  stat_pvalue_X_Ca <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(X_Ca ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position()
  
  X_Ca_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = X_Ca, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab("X-Ca (meq/100g)") +
    xlab("Field ID") +
    
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "X_Ca"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    facet_wrap(~ Site, # Allow independent Y-axes
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_X_Ca, label = "p.signif")
  
  print(X_Ca_plot)
  
  
  
  stat_pvalue_X_Mg <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(X_Mg ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() %>% 
    mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))
  
  
  
  
  
  
  
  
  # Create the ggplot
  X_Mg_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = X_Mg, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab("X_Mg (meq/100g)") +
    xlab("Field ID") +
    
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "X_Mg"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    facet_wrap(~ Site,
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_X_Mg, label = "p.signif")
  
  print(X_Mg_plot)
  
  
  stat_pvalue_X_Na <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(X_Na ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() %>% 
    mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))
  
  
  
  
  
  
  
  
  # Create the ggplot
  X_Na_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = X_Na, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab("X_Na (ppm)") +
    xlab("Field ID") +
    
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "X_Na"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    facet_wrap(~ Site,
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_X_Na, label = "p.signif")
  
  print(X_Na_plot)
  
  
  stat_pvalue_pH <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(pH ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() %>% 
    mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))
  
  
  
  
  
  
  
  
  # Create the ggplot
  pH_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = pH, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab("pH") +
    xlab("Field ID") +
    
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "pH"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    facet_wrap(~ Site,
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_pH, label = "p.signif")
  
  print(pH_plot)
  
  stat_pvalue_CEC <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(CEC ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() %>% 
    mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))
  
  
  
  
  
  
  
  
  # Create the ggplot
  CEC_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = CEC, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab("CEC (meq/100g)") +
    xlab("Field ID") +
    
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "CEC"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    facet_wrap(~ Site,
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_CEC, label = "p.signif")
  
  print(CEC_plot)
  
  
  
  
  stat_pvalue_LOI <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(LOI ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() %>% 
    mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))
  
  
  
  
  
  
  
  
  # Create the ggplot
  LOI_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = LOI, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab("LOI (%)") +
    xlab("Field ID") +
    
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "LOI"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    facet_wrap(~ Site,
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_LOI, label = "p.signif")
  
  print(LOI_plot)
  
  stat_pvalue_X_K <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(X_K ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() %>% 
    mutate(y.position = seq(min(y.position), max(y.position),length.out = n()))
  # Create the ggplot
  X_K_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = X_K, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab("X_K (ppm)") +
    xlab("Field ID") +
    
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "X_K"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    facet_wrap(~ Site,
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_X_K, label = "p.signif")
  
  print(X_K_plot)
  
  
  list(NO3_plot = NO3_plot,
       NH4_plot = NH4_plot,
       OlsenP_plot = OlsenP_plot,
       X_Ca_plot = X_Ca_plot,
       X_Mg_plot = X_Mg_plot,
       X_Na_plot = X_Na_plot,
       pH_plot = pH_plot,
       CEC_plot = CEC_plot,
       LOI_plot = LOI_plot,
       X_K_plot = X_K_plot,
       all_posthoc_results_adjusted=all_posthoc_results_adjusted
       
  )
  
}

plot_CUE = function(sample_data_4){
  
  
  sample_data_4 <- sample_data_4 %>%
    filter(CUE >= 0, CUE <= 1)
  
  
  sample_data_4$Aggregate <- factor(sample_data_4$Aggregate, levels = c("A", "B", "C", "D", "Bulk"))
  response_variables <- c("CUE")
  desired_levels <- c("A", "B", "C", "D", "Bulk")
  sample_data_4$Aggregate <- factor(sample_data_4$Aggregate, levels = desired_levels)
  aggregate_levels <- levels(sample_data_4$Aggregate)
  comparison_pairs <- utils::combn(aggregate_levels, 2, simplify = FALSE)
  # Function to perform ANOVA and posthoc test, and extract results
  analyze_and_extract <- function(data, variable) {
    formula_str <- paste0(variable, " ~ Site")
    anova_model <- aov(as.formula(formula_str), data = data)
    anova_summary <- summary(anova_model)
    anova_p_value <- anova_summary[[1]]$`Pr(>F)`[1]
    
    posthoc_results <- NULL
    if (anova_p_value < 0.05) {
      posthoc_results <- data %>%
        group_by(Aggregate) %>%
        tukey_hsd(as.formula(formula_str)) %>%
        mutate(variable = variable) # Add the variable name for later use
    } else {
      # If ANOVA is not significant, create an empty tibble
      posthoc_results <- tibble(
        Aggregate = unique(data$Aggregate),
        term = "Site",
        group1 = NA_character_,
        group2 = NA_character_,
        estimate = NA_real_,
        conf.low = NA_real_,
        conf.high = NA_real_,
        p.adj = anova_p_value,
        variable = variable
      )
    }
    return(posthoc_results)
  }
  
  
  all_posthoc_results <- map_dfr(response_variables, ~ analyze_and_extract(sample_data_4, .x))
  
  
  all_posthoc_results_adjusted <- all_posthoc_results %>%
    group_by(variable, Aggregate) %>%
    adjust_pvalue(method = "BH") %>%
    ungroup() %>%
    mutate(p.adj.signif = case_when(
      p.adj <= 0.001 ~ "***",
      p.adj <= 0.01 ~ "**",
      p.adj <= 0.05 ~ "*",
      TRUE ~ "ns"
    ))
  
  
  plot_labels <- all_posthoc_results_adjusted %>%
    filter(p.adj < 0.05) %>% # Only keep significant comparisons for labeling
    separate(group1, into = c("site1"), sep = "(?<=.)", remove = FALSE) %>% # Small hack to get site labels
    separate(group2, into = c("site2"), sep = "(?<=.)", remove = FALSE) %>% # Small hack to get site labels
    mutate(
      y_position = case_when(
        variable == "CUE" ~ max(sample_data_4$CUE, na.rm = TRUE) * 1.1, # Adjust these multipliers based on your data range
        TRUE ~ NA_real_ # Add more cases for other variables
      ),
      label = p.adj.signif
    )
  
  sample_data_4$Site<-as.factor(sample_data_4$Site)
  sample_data_4$Aggregate<-as.factor(sample_data_4$Aggregate)
  
  
  
  
  stat_pvalue_CUE <- sample_data_4 %>% 
    group_by(Site)%>%
    rstatix::wilcox_test(CUE ~ Aggregate) %>%
    filter(p < 0.05) %>% 
    rstatix::add_significance("p") %>% 
    rstatix::add_y_position() 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  
  
  CUE_plot <- sample_data_4 %>%
    ggplot(aes(x = as.factor(Aggregate), y = CUE, color = Aggregate)) +
    geom_boxplot(show.legend = FALSE,
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 alpha = 0.2) +
    geom_point(position = position_dodge(width = 0.8)) +
    ylab("CUE") +
    xlab("") +
    scale_color_manual(values = cbPalette6,
                       labels = c("A" = ">2 mm",
                                  "B" = "2-0.25 mm",
                                  "C" = "0.25-0.053 mm",
                                  "D" = "< 0.053 mm",
                                  "Bulk" = "Bulk")) +
    scale_x_discrete(labels = c("A" = ">2 mm",
                                "B" = "2-0.25 mm",
                                "C" = "0.25-0.053 mm",
                                "D" = "< 0.053 mm",
                                "Bulk" = "Bulk")) +
    theme_CKMBP() +
    # Add significance labels
    geom_text(data = plot_labels %>% filter(variable == "CUE"),
              aes(x = (as.numeric(as.factor(site1)) + as.numeric(as.factor(site2))) / 2,
                  y = y_position,
                  label = label,
                  group = Aggregate),
              position = position_dodge(width = 3),
              vjust = 2)+
    
    facet_wrap(~ Site, # Allow independent Y-axes
               labeller = labeller(Site = c("C" = "Conventional", "O" = "Organic"))) +
    ggpubr::stat_pvalue_manual(stat_pvalue_CUE, label = "p.signif")
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  list(CUE_plot=CUE_plot,
       all_posthoc_results_adjusted=all_posthoc_results_adjusted
  )
  
}

plot_gene = function(sample_data_5,sample_data_4,sample_data_6){
  
  ko_cnt=sample_data_5
  basic_meta=sample_data_6
  soil_meta=sample_data_4
  
  
  #plant_meta=read_delim('data/meta/Soilhealth_wgs_soil_meta.csv', delim = ',')
  meta_data=inner_join(basic_meta, soil_meta)
  #reorder
  ko_cnt=ko_cnt[c('KO',meta_data$ID)]
  
  
  #ko anno
  ko_anno=read.delim('Data/names',sep='\t',comment='#', quote = "", col.names = c('gene',"Name"))%>%
    mutate(gene=str_c('ko:',gene))
  
  links=list.files('Data/ko', 'ko_', full.names = T)
  links=links[!grepl('genes.list', links)]
  for (l in links){
    db_name=as.name(str_match(l, '.*/ko_(.*).list')[2])#grep link name )
    db=read.delim(l ,sep='\t',comment='#', col.names = c('gene',db_name))%>%    group_by(gene) %>%
      summarize(!! db_name := toString(!!db_name))
    ko_anno=left_join(ko_anno, db)
  }
  ko_anno=mutate(ko_anno, gene=str_replace(gene, 'ko:', ''))
  
  # Pathways
  paths_df=read.delim('Data/links/links/pathway_ko.list', sep='\t', col.names = c('Path','Gene'))%>%
    filter(!grepl("map",Path))%>%
    mutate(Path=str_sub(Path, start=6), Gene=str_sub(Gene, start=4))
  
  paths_names=read.delim('Data/pathway.list',sep='\t',comment='#', col.names = c('Path',"Name"))%>%
    mutate(Path=str_c('ko', str_pad(Path, 5, side = 'left',pad='0')))
  
  ### Norm and ord
  
  mean(apply(ko_cnt[-1], 2, sum, na.rm=T)) # median ko count in samples
  median(apply(ko_cnt[-1], 2, median, na.rm=T)) #median of median ko count in samples
  
  # Distribution
  ko_cnt%>%
    gather(key='set', value='cnt', 2:73)%>%
    ggplot(aes(x=set, y=cnt))+
    geom_violin(scale = 'width', draw_quantiles = c(0.05,0.25,0.5,0.75,0.95))+
    scale_y_log10()
  
  #Normilize
  ko_low=t(column_to_rownames(ko_cnt,var="KO"))
  d=apply(ko_low, 2, sum)
  q=quantile(d, probs=0.10, na.rm = TRUE)
  ko_low=ko_low[,d>q]
  ko_norm_wis = decostand(ko_low, 'max')
  #ko_norm_wis = decostand(ko_norm_wis, 'tot')
  ko_norm_wis = decostand(ko_norm_wis, 'log')
  ko_norm_log = decostand(ko_low, 'log')
  
  # Distribution
  as.data.frame(t(ko_norm_wis))%>%
    gather(key='set', value='cnt')%>%
    ggplot(aes(x=set, y=cnt))+
    geom_violin(scale = 'width', draw_quantiles = c(0.05,0.25,0.5,0.75,0.95))#+ scale_y_log10()
  
 # decorana(ko_norm_wis) #First Axis length <3, data is considered homogonous, linear methods(pca,rda) OK
  
  # Check how many axis  for nmds
  #goeveg::dimcheckMDS(ko_norm_wis, distance = "bray", k = 6, trymax = 20, autotransform = F, na.rm = TRUE )
  
  
  nmds=metaMDS(ko_norm_wis, "bray", too=0, wa=FALSE, k=3, auto=FALSE, maxit=1000, maxtry =1000, parallel=8, na.rm = TRUE)
  points=as_tibble(nmds$points)
  points$ID=rownames(nmds$points)
  points=inner_join(points, meta_data, by=c('ID'='ID'))
  #env_vec=envfit(nmds, soil_meta)
  #env_vec_arw=rownames_to_column(as.data.frame(env_vec$vectors$arrows)[env_vec$vectors$pvals <= 0.05,], 'fac')
  
  ggplot(points,aes(x=MDS1,y=MDS2, color=Site, shape=Aggregate))+
    geom_point(size = 4, stroke=1)+
    #geom_segment(data=env_vec_arw, aes(x=0, xend= max(abs(points$MDS1))*NMDS1,y=0,yend= max(abs(points$MDS2))*NMDS2), arrow = arrow(length = unit(0.5, "cm")), colour="grey", inherit.aes = F)+
    #geom_text(data=env_vec_arw, aes(x=max(abs(points$MDS1))*NMDS1, y=max(abs(points$MDS2))*NMDS2, label=fac),size=5, inherit.aes = F)+
    # coord_fixed()+
    labs(title = 'KEGG Gene Bray-NMDS', shape='Aggregate', color='Site', fill="Replicate", caption = sprintf('Stress: %.5f, kdim: %i', nmds$stress, nmds$ndim))
  #ggsave('data/results/plots/NMDS/ko-nmds.png')
  #env_vec
  
  # QC checks
  stressplot(nmds)
  goodness(nmds)
  #GGally::ggpairs(points, aes(color= land, alpha=.3) ,1:4, upper=list(continuous='blank'), legend = 1,progress=F)+ theme(legend.position = "bottom")
  
  #PERMANOVA
  ko_norm_wis_imputed <- ko_norm_wis
  ko_norm_wis_imputed[is.na(ko_norm_wis_imputed)] <- 0
  ko_bray <- vegdist(ko_norm_wis_imputed, 'bray')
  
  
  
  #ko_bray=vegdist(ko_norm_wis, 'bray', na.rm = TRUE)
  adonis(ko_bray ~ Site+Aggregate , data = meta_data, parallel=8, permutations=10000)
  
  ps.disper <- betadisper(ko_bray, meta_data$Site)
  plot(ps.disper)
  permutest(ps.disper, pairwise = TRUE, permutations = how(nperm = 10000))
  
  #PCA
  pca <- rda(ko_norm_wis_imputed)
  
  p=prcomp(ko_norm_wis_imputed)
  biplot(pca)
  
  
  edge_data=column_to_rownames(ko_cnt,'KO')
  edge_data=edge_data[,meta_data$Site== c('O',"C")]
  edge_meta=meta_data[meta_data$Site== c('O',"C"),]
  
  #  edge_data=column_to_rownames(ko_cnt,'Gene_ko')
  edge_data[is.na(edge_data)] <- 0
  
  lib_sizes <- edge_data %>%
    summarise_if(is.numeric, sum)
  
  lib_sizes<-unlist(lib_sizes)
  #  edge_meta=meta_data # set diploid as control
  #edge_anno=edge_anno[rownames(edge_data),]
  
  edge_meta=mutate(edge_meta, group=as.factor(paste(Site, Aggregate, sep = '.')))
  edge_meta$Aggregate<- factor(edge_meta$Aggregate, levels= c(  "Bulk","A", "B" ,"C","D"))
  design=model.matrix(~Site+Aggregate, data=edge_meta) # look at land effect hile controlling for replicate
  
  dge=DGEList(edge_data, group = edge_meta$group, lib.size = lib_sizes)#, genes = edge_anno)
  #run edger
  keep <- filterByExpr(dge, design, min.total.count=100, min.count=5)
  dge <- dge[keep, , keep.lib.sizes=FALSE]
  
  dge <- calcNormFactors(dge, method="RLE")
  dge <- estimateGLMCommonDisp(dge, design)
  dge <- estimateGLMTagwiseDisp(dge, design)
  dge = estimateDisp(dge,design)
  
  edgeR::plotBCV(dge)
  cons=list(Site=c(0,1,0,0,0,0), A=c(0,0,1,0,0,0),B=c(0,0,0,1,0,0),C=c(0,0,0,0,1,0),D=c(0,0,0,0,0,1)) # Define contrasts
  
  fits=list()
  edger_res=data.frame(ID=character())#edge_anno
  for (cs in 1:length(cons)){
    lfit <- glmFit(dge, design) # fit
    lrt <- glmLRT(lfit, contrast = cons[[cs]]) # test
    #lfit=glmQLFit(dge,design, robust = T)
    #lrt=glmQLFTest(lfit,contrast = cons[[cs]])
    fits[[names(cons[cs])]]=lrt # save test
    lrt$table$FDR=p.adjust(lrt$table$PValue, method = 'BH')
    colnames(lrt$table)=paste(names(cons[cs]), colnames(lrt$table), sep = "_") # append test sufix
    edger_res=full_join(edger_res, rownames_to_column(lrt$table, 'ID')) # join
  }
  
  edger_res=full_join(edger_res, ko_anno, by=c('ID'='gene'))
  
  
  #write.csv(edger_sig, 'data/edger_results.csv')
  
  ## Kegg vol-ma
  plot_data=edger_res
  sig_limit=0.05
  fc_limit=1
  cpm_limit=0
  Title=', KO terms'
  plots=list()
  for (con in names(cons)){
    FDR=as.name(paste(con, 'FDR', sep = '_'))
    logFC=as.name(paste(con, 'logFC', sep = '_'))
    logCPM=as.name(paste(con, 'logCPM', sep = '_'))
    
    cnt=plot_data %>% summarise(up = sum((!!logFC > fc_limit & !!FDR <= sig_limit & !!logCPM >= cpm_limit), na.rm=T), down = sum((!!logFC < -fc_limit & !!FDR <= sig_limit & !!logCPM >= cpm_limit), na.rm=T), n=n())
    
    plots[[paste(con, 'vol', sep = '_')]]=
      arrange(plot_data, desc(!!FDR))%>%
      mutate(., sig = !!FDR <= sig_limit & abs(!!logFC) > fc_limit & !!logCPM >= cpm_limit)%>%
      ggplot(., aes(y=-log10(!!FDR), x=!!logFC, color=sig))+
      geom_point(alpha=0.5)+
      scale_color_manual(values = c('black','red'))+
      labs(title = paste(as.character(con), Title, 'Volcano plot'), caption = sprintf('p <= %f, abs(FC) > %f, CPM > %f', sig_limit, fc_limit, cpm_limit))
    
    plots[[paste(con, 'ma', sep = '_')]]=
      mutate(arrange(plot_data, desc(!!FDR)), sig = !!FDR <= sig_limit & abs(!!logFC) > fc_limit & !!logCPM >= cpm_limit)%>%
      ggplot(., aes(x=!!logCPM, y=!!logFC, color=sig))+
      geom_point(alpha=0.5)+
      scale_color_manual(values = c('black','red'))+
      labs(title = paste(as.character(con), Title, 'MA plot'), caption = sprintf('Up: %d, Down: %d, Total: %d', cnt[[1,1]], cnt[[1,2]], cnt[[1,3]]))
  }
  
  Plot1_v<- ggpubr::ggarrange(plotlist = plots, common.legend = T) #%>%
  # ggexport(filename = "Graphs/ko_vol-ma.png", width = 600, height = 400)
  ggsave("Graphs//Gene_volcano_NIFA2024.png",Plot1_v)
  
  ## Kegg cluster
  library(clusterProfiler)
  library(enrichplot)
  #dges_sig=filter(edger_res, HvC_FDR <= 0.05 & abs(HvC_logFC)> 0.5 & HvC_logCPM > 0)
  #dges=dges_sig$HvC_logFC
  gene_anno=list()
  for (con in names(cons)){
    FDR=as.name(paste(con, 'FDR', sep = '_')) #define FDR col
    logFC=paste(con, 'logFC', sep = '_') #define Logfc column
    
    dges_sig=filter(edger_res, !!FDR <= 0.05)
    dges=dges_sig[[logFC]]
    names(dges)=as.character(dges_sig$ID)
    dges=sort(dges,decreasing = T)
    
    geom_path=clusterProfiler::enricher(names(dges), TERM2GENE = paths_df, TERM2NAME = paths_names,   minGSSize = 1, pvalueCutoff = 1)
    #geom_mod=clusterProfiler::enricher(names(dges), TERM2GENE = mod_df, TERM2NAME = mod_names,   minGSSize = 1, pvalueCutoff = 1)
    
    dges=edger_res[[logFC]]
    names(dges)=as.character(edger_res$ID)
    dges=dges[!is.na(edger_res$ID)]
    dges=sort(dges,decreasing = T)
    
    gsea_path=clusterProfiler::GSEA(dges, TERM2GENE = paths_df, TERM2NAME = paths_names,   minGSSize = 1, pvalueCutoff = 1)
    #gsea_mod=clusterProfiler::GSEA(dges, TERM2GENE = mod_df, TERM2NAME = mod_names, minGSSize=1,pvalueCutoff = 1 )
    
    gene_anno[[con]]=list(geom_path=geom_path, gsea_path=gsea_path)
    #write.csv(gsea_mod, 'data/results/kegg_mod_gsea.csv' )
    #write.csv(gsea_path, 'data/results/kegg_path_gsea.csv' )
    #write.csv(geom_mod, 'data/results/kegg_mod_geo.csv' )
    #write.csv(geom_path, 'data/results/kegg_path_geo.csv' )
  }
  #Plot results
  
  plot_data=gene_anno
  org='ko'
  show_Category=20
  pvalue_cutoff=0.05
  
  plots=list()
  for (i in names(plot_data)){ # for treat
    plots[[i]]=list()
    for (j in names(plot_data[[i]])){ # for method
      plots[[i]][[j]]=list()
      cur=plot_data[[i]][[j]] # method data
      if (j=='genelist') next() # skip if genelist data
      
      fc=dges#plot_data[[i]][['genelist']] # get fold change
      name=paste(i, j) # make title name
      if (is.null(cur)) next()
      
      cur@result=cur@result[cur@result$p.adjust <= pvalue_cutoff,]# filter pvalues
      if (nrow(cur@result)==0) next() # skip if no values
      #if (class(cur) == 'enrichResult') cur@gene=cur@gene[cur@result$pvalue <= pvalue_cutoff] # Readjust genes
      #cur=setReadable(cur, ko, 'ENTREZID')
      
      plots[[i]][[j]][['dot']]=dotplot(cur, showCategory=show_Category) + ggtitle(name) + scale_fill_gradient( low = "#0072B2",  high = "#D55E00", space = "Lab",  na.value = "grey50", guide = "colourbar", aesthetics = "color")
      plots[[i]][[j]][['cnet']]=cnetplot(cur, categorySize="pvalue", showCategory=show_Category, foldChange=fc)+ ggtitle(name)+ scale_fill_gradient2( low = "#0072B2", mid = "white",  high = "#D55E00", midpoint = 0, space = "Lab",  na.value = "grey50", guide = "colourbar", aesthetics = "fill")
      plots[[i]][[j]][['heat']]=heatplot(cur, foldChange=fc, showCategory = show_Category)+ ggtitle(name)+ scale_fill_gradient2( low = "#0072B2", mid = "white",  high = "#D55E00", midpoint = 0, space = "Lab",  na.value = "grey50", guide = "colourbar", aesthetics = "fill")
      #plots[[i]][[j]][['emap']]=emapplot(cur,showCategory=show_Category, color='pvalue')+ ggtitle(name)+ scale_fill_gradient( low = "#0072B2",  high = "#D55E00",  space = "Lab",  na.value = "grey50", guide = "colourbar", aesthetics = "color")
      if (class(cur) == 'enrichResult'){
        plots[[i]][[j]][['upset']]=upsetplot(cur,n=show_Category)
        plots[[i]][[j]][['bar']]=barplot(cur, showCategory=show_Category)
      }
      if (class(cur)=='gseaResult') {
        plots[[i]][[j]][['ridge']]=ridgeplot(cur, showCategory = show_Category, fill = 'pvalue' )
        plots[[i]][[j]][['cnet']] + scale_fill_gradient2( low = "#0072B2", mid = "white",  high = "#D55E00", midpoint = 0, space = "Lab",  na.value = "grey50", guide = "colourbar", aesthetics = "color")
      }
    }
  }
  
 # write_rds(plots, 'data/results/plots/kegg_gsea-geo.RDS')
  

  #PDF print?
  # Save all plots into one multi-page PDF
  pdf("Graphs//enrichment_plots_Gene.pdf", width = 11, height = 8)
  
  # Loop through all the plots and print them to the PDF file
  for (i in names(plots)) {
    for (j in names(plots[[i]])) {
      # Skip empty lists (like for 'genelist')
      if (length(plots[[i]][[j]]) == 0) next()
      
      for (k in names(plots[[i]][[j]])) {
        # The print() command is essential to render the plot
        print(plots[[i]][[j]][[k]])
      }
    }
  }
  
  # Finalize and close the PDF file
  dev.off()
  
  
  Plot2<-print(plots)
  
  
  ##Heatmap
  
  library(ggdendro)
  sig_data=edger_res%>%
    filter(if_any(ends_with("FDR"), ~ .x<=0.05)) #%>%
  #filter(if_any(ends_with("logFC"), ~ .x>=2))
  
  plot_data=data.frame(Name=character(), test=character(), FDR=double(), logCPM=double(), FDR=double())
  for (con in names(cons)){
    FDR=as.name(paste(con, 'FDR', sep = '_'))
    logFC=as.name(paste(con, 'logFC', sep = '_'))
    logCPM=as.name(paste(con, 'logCPM', sep = '_'))
    
    plot_data=rbind(plot_data, transmute(sig_data, ID = ID, Name=Name, test=con, logFC=!!logFC, logCPM=!!logCPM, FDR=!!FDR))
  }
  
  dendo=column_to_rownames(sig_data, 'ID')%>%select(ends_with('logFC'))%>%vegan::vegdist(., 'euclidean')%>%hclust(.)%>%as.dendrogram(.)
  dendo_plot=dendo%>%ggdendrogram(., rotate = T) + theme(axis.text.y = element_text(size = 6))
  plot_data$ID <- factor(x = plot_data$ID, levels = plot_data$ID[order.dendrogram(dendo)], ordered = TRUE)
  
  
  heat= ggplot(plot_data, aes(y=ID, x=factor(test, levels=names(cons)), fill=logFC))+
    #scale_y_discrete(labels=plot_data$Name)+
    geom_tile()+
    xlab('Comparison')
    scale_fill_gradient2()
  
  heat
  
  heat2<-ggarrange(heat, dendo_plot, ncol = 2, align = 'hv', common.legend = T, label.y = c(1,0))
  
  ggsave("Graphs//Gene_Heat_NIFA2024.png",heat)
  
  list(Plot1_v=Plot1_v,
       Plot2=Plot2,
       heat=heat,
       heat2=heat2,
       edger_res=edger_res,
       ko_anno=ko_anno,
       paths_df=paths_df,
       paths_names=paths_names,
       ko_norm_wis = ko_norm_wis,         # samples x KO (normalized/logged)
       ko_norm_log = ko_norm_log          # samples x KO (logged only)
       
  )
  
}

plot_metab = function(sample_data_7,sample_data_4,sample_data_6){
  
  ko_cnt=sample_data_7[, -c(2:8)]
  ko_cnt=ko_cnt[,-c(92:100)]
  ko_cnt=ko_cnt%>%
    `colnames<-`(c("KO", colnames(.)[-1]))
  
  basic_meta=sample_data_6
  
  soil_meta=sample_data_4
  
  IDs<- sample_data_7%>%
    select(c("BinBase.name", "KEGG"))
  
  IDs=IDs%>%
    `colnames<-`(c("ID", colnames(.)[-1]))
  
  #plant_meta=read_delim('data/meta/Soilhealth_wgs_soil_meta.csv', delim = ',')
  meta_data=inner_join(basic_meta, soil_meta)
  #reorder
  ko_cnt=ko_cnt[c('KO',meta_data$ID)]
  
  
  #ko anno
  ko_anno=read.delim('Data/names',sep='\t',comment='#', quote = "", col.names = c('gene',"Name"))%>%
    mutate(gene = substr(gene, 2, nchar(gene)))%>%
    mutate(gene=str_c('map',gene))
  
  IDs_aggregated <- IDs %>%
    group_by(ID) %>%
    summarise(
      Annotation_Combined = paste(KEGG, collapse = "; ") # Combines into a single string
      # If you wanted a list of annotations in a single cell (more complex for general use):
      # Annotation_List = list(Annotation)
    ) %>%
    ungroup()
  
  
  
  
  
  # links=list.files('Data/ko', 'ko_', full.names = T)
  #  links=links[!grepl('genes.list', links)]
  # for (l in links){
  #  db_name=as.name(str_match(l, '.*/ko_(.*).list')[2])#grep link name )
  # db=read.delim(l ,sep='\t',comment='#', col.names = c('gene',db_name))%>%    group_by(gene) %>%
  #  summarize(!! db_name := toString(!!db_name))
  #ko_anno=left_join(ko_anno, db)
  #}
  #ko_anno=mutate(ko_anno, gene=str_replace(gene, 'ko:', ''))
  
  # Pathways
  paths_df=read.delim('Data/links/links/pathway_compound.list', sep='\t', col.names = c('Path','Gene'))%>%
    mutate(Path=str_sub(Path, start=6), Gene=str_sub(Gene, start=5))
  
  paths_names=read.delim('Data/pathway.list',sep='\t',comment='#', col.names = c('Path',"Name"))%>%
    mutate(Path=str_c('map', str_pad(Path, 5, side = 'left',pad='0')))
  
  ### Norm and ord
  
  mean(apply(ko_cnt[-1], 2, sum, na.rm=T)) # median ko count in samples
  median(apply(ko_cnt[-1], 2, median, na.rm=T)) #median of median ko count in samples
  
  # Distribution
  ko_cnt%>%
    gather(key='set', value='cnt', 2:73)%>%
    ggplot(aes(x=set, y=cnt))+
    geom_violin(scale = 'width', draw_quantiles = c(0.05,0.25,0.5,0.75,0.95))+
    scale_y_log10()
  
  #Normilize
  ko_low=t(column_to_rownames(ko_cnt,var="KO"))
  d=apply(ko_low, 2, sum)
  q=quantile(d, probs=0.10, na.rm = TRUE)
  ko_low=ko_low[,d>q]
  ko_norm_wis = decostand(ko_low, 'max')
  #ko_norm_wis = decostand(ko_norm_wis, 'tot')
  ko_norm_wis = decostand(ko_norm_wis, 'log')
  ko_norm_log = decostand(ko_low, 'log')
  
  # Distribution
  as.data.frame(t(ko_norm_wis))%>%
    gather(key='set', value='cnt')%>%
    ggplot(aes(x=set, y=cnt))+
    geom_violin(scale = 'width', draw_quantiles = c(0.05,0.25,0.5,0.75,0.95))#+ scale_y_log10()
  
  # decorana(ko_norm_wis) #First Axis length <3, data is considered homogonous, linear methods(pca,rda) OK
  
  # Check how many axis  for nmds
  #goeveg::dimcheckMDS(ko_norm_wis, distance = "bray", k = 6, trymax = 20, autotransform = F, na.rm = TRUE )
  
  
  nmds=metaMDS(ko_norm_wis, "bray", too=0, wa=FALSE, k=3, auto=FALSE, maxit=1000, maxtry =1000, parallel=8, na.rm = TRUE)
  points=as_tibble(nmds$points)
  points$ID=rownames(nmds$points)
  points=inner_join(points, meta_data, by=c('ID'='ID'))
  #env_vec=envfit(nmds, soil_meta)
  #env_vec_arw=rownames_to_column(as.data.frame(env_vec$vectors$arrows)[env_vec$vectors$pvals <= 0.05,], 'fac')
  
  ggplot(points,aes(x=MDS1,y=MDS2, color=Site, shape=Aggregate))+
    geom_point(size = 4, stroke=1)+
    #geom_segment(data=env_vec_arw, aes(x=0, xend= max(abs(points$MDS1))*NMDS1,y=0,yend= max(abs(points$MDS2))*NMDS2), arrow = arrow(length = unit(0.5, "cm")), colour="grey", inherit.aes = F)+
    #geom_text(data=env_vec_arw, aes(x=max(abs(points$MDS1))*NMDS1, y=max(abs(points$MDS2))*NMDS2, label=fac),size=5, inherit.aes = F)+
    # coord_fixed()+
    labs(title = 'KEGG Gene Bray-NMDS', shape='Aggregate', color='Site', fill="Replicate", caption = sprintf('Stress: %.5f, kdim: %i', nmds$stress, nmds$ndim))
  #ggsave('data/results/plots/NMDS/ko-nmds.png')
  #env_vec
  
  # QC checks
  stressplot(nmds)
  goodness(nmds)
  #GGally::ggpairs(points, aes(color= land, alpha=.3) ,1:4, upper=list(continuous='blank'), legend = 1,progress=F)+ theme(legend.position = "bottom")
  
  #PERMANOVA
  ko_norm_wis_imputed <- ko_norm_wis
  ko_norm_wis_imputed[is.na(ko_norm_wis_imputed)] <- 0
  ko_bray <- vegdist(ko_norm_wis_imputed, 'bray')
  
  
  
  #ko_bray=vegdist(ko_norm_wis, 'bray', na.rm = TRUE)
  adonis(ko_bray ~ Site+Aggregate , data = meta_data, parallel=8, permutations=10000)
  
  ps.disper <- betadisper(ko_bray, meta_data$Site)
  plot(ps.disper)
  permutest(ps.disper, pairwise = TRUE, permutations = how(nperm = 10000))
  
  #PCA
  pca <- rda(ko_norm_wis_imputed)
  
  p=prcomp(ko_norm_wis_imputed)
  biplot(pca)
  
  
  edge_data=column_to_rownames(ko_cnt,'KO')
  edge_data=edge_data[,meta_data$Site== c('O',"C")]
  edge_meta=meta_data[meta_data$Site== c('O',"C"),]
  
  #  edge_data=column_to_rownames(ko_cnt,'Gene_ko')
  edge_data[is.na(edge_data)] <- 0
  
  lib_sizes <- edge_data %>%
    summarise_if(is.numeric, sum)
  
  lib_sizes<-unlist(lib_sizes)
  #  edge_meta=meta_data # set diploid as control
  #edge_anno=edge_anno[rownames(edge_data),]
  
  edge_meta=mutate(edge_meta, group=as.factor(paste(Site, Aggregate, sep = '.')))
  edge_meta$Aggregate<- factor(edge_meta$Aggregate, levels= c(  "Bulk","A", "B" ,"C","D"))
  design=model.matrix(~Site+Aggregate, data=edge_meta) # look at land effect hile controlling for replicate
  
  dge=DGEList(edge_data, group = edge_meta$group, lib.size = lib_sizes)#, genes = edge_anno)
  #run edger
  keep <- filterByExpr(dge, design, min.total.count=100, min.count=5)
  dge <- dge[keep, , keep.lib.sizes=FALSE]
  
  dge <- calcNormFactors(dge, method="RLE")
  dge <- estimateGLMCommonDisp(dge, design)
  dge <- estimateGLMTagwiseDisp(dge, design)
  dge = estimateDisp(dge,design)
  
  edgeR::plotBCV(dge)
  cons=list(Site=c(0,1,0,0,0,0), A=c(0,0,1,0,0,0),B=c(0,0,0,1,0,0),C=c(0,0,0,0,1,0),D=c(0,0,0,0,0,1)) # Define contrasts
  
  fits=list()
  edger_res=data.frame(ID=character())#edge_anno
  for (cs in 1:length(cons)){
    lfit <- glmFit(dge, design) # fit
    lrt <- glmLRT(lfit, contrast = cons[[cs]]) # test
    #lfit=glmQLFit(dge,design, robust = T)
    #lrt=glmQLFTest(lfit,contrast = cons[[cs]])
    fits[[names(cons[cs])]]=lrt # save test
    lrt$table$FDR=p.adjust(lrt$table$PValue, method = 'BH')
    colnames(lrt$table)=paste(names(cons[cs]), colnames(lrt$table), sep = "_") # append test sufix
    edger_res=full_join(edger_res, rownames_to_column(lrt$table, 'ID')) # join
  }
  
  p<- paths_df %>%
    left_join(ko_anno, by=c("Path"="gene"))%>%
    group_by(Gene) %>%
    summarise(
      Path= paste(Path, collapse = "; "), Name= paste(Name, collapse = "; ") 
    ) %>%
    ungroup()
  
  edger_res<- edger_res %>%
    left_join(IDs, by="ID")%>%
    left_join(p, by=c("KEGG"='Gene'))
  
  
  
  
  
  #edger_res=full_join(edger_res, ko_anno, by=c('Path'='gene'))
  
  
  #write.csv(edger_sig, 'data/edger_results.csv')
  
  ## Kegg vol-ma
  plot_data=edger_res
  sig_limit=0.05
  fc_limit=1
  cpm_limit=0
  Title=', KO terms'
  plots=list()
  for (con in names(cons)){
    FDR=as.name(paste(con, 'FDR', sep = '_'))
    logFC=as.name(paste(con, 'logFC', sep = '_'))
    logCPM=as.name(paste(con, 'logCPM', sep = '_'))
    
    cnt=plot_data %>% summarise(up = sum((!!logFC > fc_limit & !!FDR <= sig_limit & !!logCPM >= cpm_limit), na.rm=T), down = sum((!!logFC < -fc_limit & !!FDR <= sig_limit & !!logCPM >= cpm_limit), na.rm=T), n=n())
    
    plots[[paste(con, 'vol', sep = '_')]]=
      arrange(plot_data, desc(!!FDR))%>%
      mutate(., sig = !!FDR <= sig_limit & abs(!!logFC) > fc_limit & !!logCPM >= cpm_limit)%>%
      ggplot(., aes(y=-log10(!!FDR), x=!!logFC, color=sig))+
      geom_point(alpha=0.5)+
      scale_color_manual(values = c('black','red'))+
      labs(title = paste(as.character(con), Title, 'Volcano plot'), caption = sprintf('p <= %f, abs(FC) > %f, CPM > %f', sig_limit, fc_limit, cpm_limit))
    
    plots[[paste(con, 'ma', sep = '_')]]=
      mutate(arrange(plot_data, desc(!!FDR)), sig = !!FDR <= sig_limit & abs(!!logFC) > fc_limit & !!logCPM >= cpm_limit)%>%
      ggplot(., aes(x=!!logCPM, y=!!logFC, color=sig))+
      geom_point(alpha=0.5)+
      scale_color_manual(values = c('black','red'))+
      labs(title = paste(as.character(con), Title, 'MA plot'), caption = sprintf('Up: %d, Down: %d, Total: %d', cnt[[1,1]], cnt[[1,2]], cnt[[1,3]]))
  }
  
  Plot1_v<- ggpubr::ggarrange(plotlist = plots, common.legend = T) #%>%
  # ggexport(filename = "Graphs/ko_vol-ma.png", width = 600, height = 400)
  ggsave("Graphs//Metab_volcano_NIFA2024.png",Plot1_v)
  
  ## Kegg cluster
  library(clusterProfiler)
  library(enrichplot)
  #dges_sig=filter(edger_res, HvC_FDR <= 0.05 & abs(HvC_logFC)> 0.5 & HvC_logCPM > 0)
  #dges=dges_sig$HvC_logFC
  gene_anno=list()
  for (con in names(cons)){
    FDR=as.name(paste(con, 'FDR', sep = '_')) #define FDR col
    logFC=paste(con, 'logFC', sep = '_') #define Logfc column
    
    dges_sig=filter(edger_res, !!FDR <= 0.05)
    dges=dges_sig[[logFC]]
    names(dges)=as.character(dges_sig$KEGG)
    dges=sort(dges,decreasing = T)
    
    geom_path=clusterProfiler::enricher(names(dges), TERM2GENE = paths_df, TERM2NAME = paths_names,   minGSSize = 1, pvalueCutoff = 1)
    #geom_mod=clusterProfiler::enricher(names(dges), TERM2GENE = mod_df, TERM2NAME = mod_names,   minGSSize = 1, pvalueCutoff = 1)
    
    dges=edger_res[[logFC]]
    names(dges)=as.character(edger_res$KEGG)
    dges=dges[!is.na(edger_res$KEGG)]
    dges=sort(dges,decreasing = T)
    
    gsea_path=clusterProfiler::GSEA(dges, TERM2GENE = paths_df, TERM2NAME = paths_names,   minGSSize = 1, pvalueCutoff = 1)
    #gsea_mod=clusterProfiler::GSEA(dges, TERM2GENE = mod_df, TERM2NAME = mod_names, minGSSize=1,pvalueCutoff = 1 )
    
    gene_anno[[con]]=list(geom_path=geom_path, gsea_path=gsea_path)
    #write.csv(gsea_mod, 'data/results/kegg_mod_gsea.csv' )
    #write.csv(gsea_path, 'data/results/kegg_path_gsea.csv' )
    #write.csv(geom_mod, 'data/results/kegg_mod_geo.csv' )
    #write.csv(geom_path, 'data/results/kegg_path_geo.csv' )
  }
  #Plot results
  
  plot_data=gene_anno
  org='ko'
  show_Category=20
  pvalue_cutoff=0.05
  
  plots=list()
  for (i in names(plot_data)){ # for treat
    plots[[i]]=list()
    for (j in names(plot_data[[i]])){ # for method
      plots[[i]][[j]]=list()
      cur=plot_data[[i]][[j]] # method data
      if (j=='genelist') next() # skip if genelist data
      
      fc=dges#plot_data[[i]][['genelist']] # get fold change
      name=paste(i, j) # make title name
      if (is.null(cur)) next()
      
      cur@result=cur@result[cur@result$p.adjust <= pvalue_cutoff,]# filter pvalues
      if (nrow(cur@result)==0) next() # skip if no values
      #if (class(cur) == 'enrichResult') cur@gene=cur@gene[cur@result$pvalue <= pvalue_cutoff] # Readjust genes
      #cur=setReadable(cur, ko, 'ENTREZID')
      
      plots[[i]][[j]][['dot']]=dotplot(cur, showCategory=show_Category) + ggtitle(name) + scale_fill_gradient( low = "#C51B7D",  high = "#F0E442", space = "Lab",  na.value = "grey50", guide = "colourbar", aesthetics = "color")
      plots[[i]][[j]][['cnet']]=cnetplot(cur, categorySize="pvalue", showCategory=show_Category, foldChange=fc)+ ggtitle(name)+ scale_fill_gradient2( low = "#C51B7D", mid = "white",  high = "#F0E442", midpoint = 0, space = "Lab",  na.value = "grey50", guide = "colourbar", aesthetics = "fill")
      plots[[i]][[j]][['heat']]=heatplot(cur, foldChange=fc, showCategory = show_Category)+ ggtitle(name)+ scale_fill_gradient2( low = "#C51B7D", mid = "white",  high = "#F0E442", midpoint = 0, space = "Lab",  na.value = "grey50", guide = "colourbar", aesthetics = "fill")
      #plots[[i]][[j]][['emap']]=emapplot(cur,showCategory=show_Category, color='pvalue')+ ggtitle(name)+ scale_fill_gradient( low = "#C51B7D",  high = "#F0E442",  space = "Lab",  na.value = "grey50", guide = "colourbar", aesthetics = "color")
      if (class(cur) == 'enrichResult'){
        plots[[i]][[j]][['upset']]=upsetplot(cur,n=show_Category)
        plots[[i]][[j]][['bar']]=barplot(cur, showCategory=show_Category)
      }
      if (class(cur)=='gseaResult') {
        plots[[i]][[j]][['ridge']]=ridgeplot(cur, showCategory = show_Category, fill = 'pvalue' )
        plots[[i]][[j]][['cnet']] + scale_fill_gradient2( low = "#C51B7D", mid = "white",  high = "#F0E442", midpoint = 0, space = "Lab",  na.value = "grey50", guide = "colourbar", aesthetics = "color")
      }
    }
  }
  
  #write_rds(plots, 'data/results/plots/kegg_gsea-Metab.RDS')
  
  
  #PDF print?
  # Save all plots into one multi-page PDF
  pdf("Graphs//enrichment_plots_Metab.pdf", width = 11, height = 8)
  
  # Loop through all the plots and print them to the PDF file
  for (i in names(plots)) {
    for (j in names(plots[[i]])) {
      # Skip empty lists (like for 'genelist')
      if (length(plots[[i]][[j]]) == 0) next()
      
      for (k in names(plots[[i]][[j]])) {
        # The print() command is essential to render the plot
        print(plots[[i]][[j]][[k]])
      }
    }
  }
  
  # Finalize and close the PDF file
  dev.off()
  
  
  Plot2<-print(plots)
  
  
  ##Heatmap
  
  library(ggdendro)
  sig_data=edger_res%>%
    filter(if_any(ends_with("FDR"), ~ .x<=0.05)) #%>%
  #filter(if_any(ends_with("logFC"), ~ .x>=2))
  
  plot_data=data.frame(Name=character(), test=character(), FDR=double(), logCPM=double(), FDR=double())
  for (con in names(cons)){
    FDR=as.name(paste(con, 'FDR', sep = '_'))
    logFC=as.name(paste(con, 'logFC', sep = '_'))
    logCPM=as.name(paste(con, 'logCPM', sep = '_'))
    
    plot_data=rbind(plot_data, transmute(sig_data, ID = ID, Name=Name, test=con, logFC=!!logFC, logCPM=!!logCPM, FDR=!!FDR))
  }
  
  dendo=column_to_rownames(sig_data, 'ID')%>%select(ends_with('logFC'))%>%vegan::vegdist(., 'euclidean')%>%hclust(.)%>%as.dendrogram(.)
  dendo_plot=dendo%>%ggdendrogram(., rotate = T) + theme(axis.text.y = element_text(size = 6))
  plot_data$ID <- factor(x = plot_data$ID, levels = plot_data$ID[order.dendrogram(dendo)], ordered = TRUE)
  
  
  heat= ggplot(plot_data, aes(y=ID, x=factor(test, levels=names(cons)), fill=logFC))+
    #scale_y_discrete(labels=plot_data$Name)+
    geom_tile()+
    xlab('Comparison')
  scale_fill_gradient2()
  
  heat
  
  heat2<-ggarrange(heat, dendo_plot, ncol = 2, align = 'hv', common.legend = T, label.y = c(1,0))
  
  ggsave("Graphs//Metab_Heat_NIFA2024.png",heat)
  
  list(Plot1_v=Plot1_v,
       Plot2=Plot2,
       heat=heat,
       heat2=heat2,
       edger_res=edger_res,
       ko_anno=ko_anno,
       paths_df=paths_df,
       paths_names=paths_names,
       ko_norm_wis = ko_norm_wis,         # samples x metabolite (normalized/logged)
       ko_norm_log = ko_norm_log,         # samples x metabolite (logged only)
       ids_table   = IDs                  # metabolite IDs table for mapping
       
  )
  
}

plot_KEGG = function(gg_gene, gg_metab){
  
  edger_res<-gg_gene$edger_res
  ko_anno<-gg_gene$ko_anno
  paths_df<-gg_gene$paths_df
  paths_names<-gg_gene$paths_names
  
  edger_res2<-gg_metab$edger_res
  ko_anno2<-gg_metab$ko_anno
  paths_df2<-gg_metab$paths_df
  paths_names2<-gg_metab$paths_names

  
  
  
  
  
  
  
  
  
  
  
  
  ##Plotting KEGG pathways A
  

  ko_sig=edger_res%>%
    mutate(ID=str_c('ko:', ID))%>%
    filter(A_FDR < 0.05 & abs(A_logFC) >.5)%>%
    left_join(.,ko_anno, by=c('ID'='gene'))
  
  pv_data=as.numeric(ko_sig$A_logFC)
  names(pv_data)=ko_sig$ID%>%
    str_replace(., 'ko:', '')
  
  pathways=paths_df%>%
    left_join(., paths_names)%>%
    mutate(Gene=str_replace(.$Gene, 'ko:', ''))%>%
    filter(., Gene %in% names(pv_data))%>%
    mutate(Path=str_replace(.$Path, 'path:', ''))%>%
    group_by(Path, Name)%>%
    summarise(cnt=n())%>%
    arrange(desc(cnt))
  
  ko_sig2=edger_res2%>%
    mutate(ID=str_c('ko:', ID))%>%
    filter(A_FDR < 0.05 & abs(A_logFC) >.5)%>%
    left_join(.,ko_anno2, by=c('ID'='gene'))
  
  pv_data2=as.numeric(ko_sig2$A_logFC)
  names(pv_data2)=ko_sig2$KEGG
  
  pathways2=paths_df2%>%
    left_join(., paths_names2)%>%
    mutate(Path = str_replace(Path, 'map', ''))%>%
    group_by(Path, Name)%>%
    summarise(cnt=n())%>%
    arrange(desc(cnt))
  
  combined_pathways_df <- bind_rows(pathways, pathways2)
  
  paths_df2<- paths_df2%>%
    rename(Metabolite= Gene)
  
  
  
  
  
  
  
  
  kegg_maps_dir <- "C://Users//cmcm//OneDrive - University of California, Davis//Documents//Github//CUE-UCD//Graphs//KEGG//A"
  
  withr::with_dir(kegg_maps_dir, {
    for (pw in 1:nrow(combined_pathways_df)) {
      
      current_pathway_id <- combined_pathways_df[[pw, 1]]
      
      # 1. Filter for GENES in the current pathway
      genes_in_pathway <- mutate(paths_df, 
                                 Path = str_remove(Path, "ko|map"),
                                 Gene = str_remove(Gene, "ko:")) %>%
        dplyr::filter(Path == current_pathway_id)
      
      # 2. Filter for METABOLITES in the current pathway
      # Assumes paths_df2 has columns 'Path' and 'Metabolite'
      # Assumes metabolite IDs are like "cpd:C00001", remove "cpd:"
      
      metabolites_in_pathway <- mutate(paths_df2, 
                                       Path = str_remove(Path, "ko|map"),
                                       Metabolite = str_remove(Metabolite, "cpd:")) %>%
        dplyr::filter(Path == current_pathway_id)
      
      # 3. Check how many of your genes/metabolites have DATA
      genes_with_data <- names(pv_data) %in% genes_in_pathway$Gene
      metabolites_with_data <- names(pv_data2) %in% metabolites_in_pathway$Metabolite
      
      num_genes <- sum(genes_with_data)
      num_metabolites <- sum(metabolites_with_data)
      
      print(paste("Pathway:", current_pathway_id, "| Genes with data:", num_genes, "| Metabolites with data:", num_metabolites))
      
      # 4. New condition: Skip only if there is NO data for EITHER genes or metabolites
      if (num_genes == 0 && num_metabolites == 0) {
        print("--> Skipping: No data found for this pathway.")
        next 
      }
      
      # Generate the pathway map
      pathview(gene.data = pv_data,
               cpd.data = pv_data2,
               pathway.id = current_pathway_id,
               species = "ko",
               out.suffix = paste0(str_remove_all(combined_pathways_df[pw, 2], '/'), ".A.ko.data"),
               kegg.native = TRUE,
               same.layer = FALSE,
               low = list(gene = "#0072B2", cpd = "#C51B7D"),
               mid = list(gene = "gray", cpd = "white"),
               high = list(gene = "#D55E00", cpd = "#F0E442"),
               bins = list(gene = 25, cpd = 25),
               kegg.dir = kegg_maps_dir)
    }
  })
  
  
  
  
  ##Plotting KEGG pathways C
  
  
  
  
  ko_sig=edger_res%>%
    mutate(ID=str_c('ko:', ID))%>%
    filter(C_FDR < 0.05 & abs(C_logFC) >.5)%>%
    left_join(.,ko_anno, by=c('ID'='gene'))
  
  pv_data=as.numeric(ko_sig$C_logFC)
  names(pv_data)=ko_sig$ID%>%
    str_replace(., 'ko:', '')
  
  pathways=paths_df%>%
    left_join(., paths_names)%>%
    mutate(Gene=str_replace(.$Gene, 'ko:', ''))%>%
    filter(., Gene %in% names(pv_data))%>%
    mutate(Path=str_replace(.$Path, 'path:', ''))%>%
    group_by(Path, Name)%>%
    summarise(cnt=n())%>%
    arrange(desc(cnt))
  
  
  
  ko_sig2=edger_res2%>%
    mutate(ID=str_c('ko:', ID))%>%
    filter(C_FDR < 0.05 & abs(C_logFC) >.5)%>%
    left_join(.,ko_anno2, by=c('ID'='gene'))
  
  pv_data2=as.numeric(ko_sig2$C_logFC)
  names(pv_data2)=ko_sig2$KEGG
  
  pathways2=paths_df2%>%
    left_join(., paths_names2)%>%
    mutate(Path = str_replace(Path, 'map', ''))%>%
    group_by(Path, Name)%>%
    summarise(cnt=n())%>%
    arrange(desc(cnt))
  
  
  
  
  combined_pathways_df <- bind_rows(pathways, pathways2)
  
  
  
  
  
  
  
  
  
  
  kegg_maps_dir <- "C://Users//cmcm//OneDrive - University of California, Davis//Documents//Github//CUE-UCD//Graphs//KEGG//C"
  
  withr::with_dir(kegg_maps_dir, {
    for (pw in 1:nrow(combined_pathways_df)) {
      
      current_pathway_id <- combined_pathways_df[[pw, 1]]
      
      # 1. Filter for GENES in the current pathway
      genes_in_pathway <- mutate(paths_df, 
                                 Path = str_remove(Path, "ko|map"),
                                 Gene = str_remove(Gene, "ko:")) %>%
        dplyr::filter(Path == current_pathway_id)
      
      # 2. Filter for METABOLITES in the current pathway
      # Assumes paths_df2 has columns 'Path' and 'Metabolite'
      # Assumes metabolite IDs are like "cpd:C00001", remove "cpd:"
      
      metabolites_in_pathway <- mutate(paths_df2, 
                                       Path = str_remove(Path, "ko|map"),
                                       Metabolite = str_remove(Metabolite, "cpd:")) %>%
        dplyr::filter(Path == current_pathway_id)
      
      # 3. Check how many of your genes/metabolites have DATA
      genes_with_data <- names(pv_data) %in% genes_in_pathway$Gene
      metabolites_with_data <- names(pv_data2) %in% metabolites_in_pathway$Metabolite
      
      num_genes <- sum(genes_with_data)
      num_metabolites <- sum(metabolites_with_data)
      
      print(paste("Pathway:", current_pathway_id, "| Genes with data:", num_genes, "| Metabolites with data:", num_metabolites))
      
      # 4. New condition: Skip only if there is NO data for EITHER genes or metabolites
      if (num_genes == 0 && num_metabolites == 0) {
        print("--> Skipping: No data found for this pathway.")
        next 
      }
      
      # Generate the pathway map
      pathview(gene.data = pv_data,
               cpd.data = pv_data2,
               pathway.id = current_pathway_id,
               species = "ko",
               out.suffix = paste0(str_remove_all(combined_pathways_df[pw, 2], '/'), ".C.ko.data"),
               kegg.native = TRUE,
               same.layer = FALSE,
               low = list(gene = "#0072B2", cpd = "#C51B7D"),
               mid = list(gene = "gray", cpd = "white"),
               high = list(gene = "#D55E00", cpd = "#F0E442"),
               bins = list(gene = 25, cpd = 25),
               kegg.dir = kegg_maps_dir)
    }
  })
  
  
  
  
  ##Plotting KEGG pathways B
  
  
  
  
  ko_sig=edger_res%>%
    mutate(ID=str_c('ko:', ID))%>%
    filter(B_FDR < 0.05 & abs(B_logFC) >.5)%>%
    left_join(.,ko_anno, by=c('ID'='gene'))
  
  pv_data=as.numeric(ko_sig$B_logFC)
  names(pv_data)=ko_sig$ID%>%
    str_replace(., 'ko:', '')
  
  pathways=paths_df%>%
    left_join(., paths_names)%>%
    mutate(Gene=str_replace(.$Gene, 'ko:', ''))%>%
    filter(., Gene %in% names(pv_data))%>%
    mutate(Path=str_replace(.$Path, 'path:', ''))%>%
    group_by(Path, Name)%>%
    summarise(cnt=n())%>%
    arrange(desc(cnt))
  
  
  ko_sig2=edger_res2%>%
    mutate(ID=str_c('ko:', ID))%>%
    filter(B_FDR < 0.05 & abs(B_logFC) >.5)%>%
    left_join(.,ko_anno2, by=c('ID'='gene'))
  
  pv_data2=as.numeric(ko_sig2$B_logFC)
  names(pv_data2)=ko_sig2$KEGG
  
  pathways2=paths_df2%>%
    left_join(., paths_names2)%>%
    mutate(Path = str_replace(Path, 'map', ''))%>%
    group_by(Path, Name)%>%
    summarise(cnt=n())%>%
    arrange(desc(cnt))
  
  
  
  
  combined_pathways_df <- bind_rows(pathways, pathways2)
  
  
  
  
  
  
  
  
  
  
  
  kegg_maps_dir <- "C://Users//cmcm//OneDrive - University of California, Davis//Documents//Github//CUE-UCD//Graphs//KEGG//B"
  
  withr::with_dir(kegg_maps_dir, {
    for (pw in 1:nrow(combined_pathways_df)) {
      
      current_pathway_id <- combined_pathways_df[[pw, 1]]
      
      # 1. Filter for GENES in the current pathway
      genes_in_pathway <- mutate(paths_df, 
                                 Path = str_remove(Path, "ko|map"),
                                 Gene = str_remove(Gene, "ko:")) %>%
        dplyr::filter(Path == current_pathway_id)
      
      # 2. Filter for METABOLITES in the current pathway
      # Assumes paths_df2 has columns 'Path' and 'Metabolite'
      # Assumes metabolite IDs are like "cpd:C00001", remove "cpd:"
      
      metabolites_in_pathway <- mutate(paths_df2, 
                                       Path = str_remove(Path, "ko|map"),
                                       Metabolite = str_remove(Metabolite, "cpd:")) %>%
        dplyr::filter(Path == current_pathway_id)
      
      # 3. Check how many of your genes/metabolites have DATA
      genes_with_data <- names(pv_data) %in% genes_in_pathway$Gene
      metabolites_with_data <- names(pv_data2) %in% metabolites_in_pathway$Metabolite
      
      num_genes <- sum(genes_with_data)
      num_metabolites <- sum(metabolites_with_data)
      
      print(paste("Pathway:", current_pathway_id, "| Genes with data:", num_genes, "| Metabolites with data:", num_metabolites))
      
      # 4. New condition: Skip only if there is NO data for EITHER genes or metabolites
      if (num_genes == 0 && num_metabolites == 0) {
        print("--> Skipping: No data found for this pathway.")
        next 
      }
      
      # Generate the pathway map
      pathview(gene.data = pv_data,
               cpd.data = pv_data2,
               pathway.id = current_pathway_id,
               species = "ko",
               out.suffix = paste0(str_remove_all(combined_pathways_df[pw, 2], '/'), ".B.ko.data"),
               kegg.native = TRUE,
               same.layer = FALSE,
               low = list(gene = "#0072B2", cpd = "#C51B7D"),
               mid = list(gene = "gray", cpd = "white"),
               high = list(gene = "#D55E00", cpd = "#F0E442"),
               bins = list(gene = 25, cpd = 25),
               kegg.dir = kegg_maps_dir)
    }
  })
  
  
  
  ##Plotting KEGG pathways Site
  
  
  
  
  ko_sig=edger_res%>%
    mutate(ID=str_c('ko:', ID))%>%
    filter(Site_FDR < 0.05 & abs(Site_logFC) >.5)%>%
    left_join(.,ko_anno, by=c('ID'='gene'))
  
  pv_data=as.numeric(ko_sig$Site_logFC)
  names(pv_data)=ko_sig$ID%>%
    str_replace(., 'ko:', '')
  
  pathways=paths_df%>%
    left_join(., paths_names)%>%
    mutate(Gene=str_replace(.$Gene, 'ko:', ''))%>%
    filter(., Gene %in% names(pv_data))%>%
    mutate(Path=str_replace(.$Path, 'path:', ''))%>%
    group_by(Path, Name)%>%
    summarise(cnt=n())%>%
    arrange(desc(cnt))
  
  ko_sig2=edger_res2%>%
    mutate(ID=str_c('ko:', ID))%>%
    filter(Site_FDR < 0.05 & abs(Site_logFC) >.5)%>%
    left_join(.,ko_anno2, by=c('ID'='gene'))
  
  pv_data2=as.numeric(ko_sig2$Site_logFC)
  names(pv_data2)=ko_sig2$KEGG
  
  pathways2=paths_df2%>%
    left_join(., paths_names2)%>%
    mutate(Path = str_replace(Path, 'map', ''))%>%
    group_by(Path, Name)%>%
    summarise(cnt=n())%>%
    arrange(desc(cnt))
  
  
  
  
  combined_pathways_df <- bind_rows(pathways, pathways2)
  
  
  
  
  
  
  
  
  
  
  
  
  kegg_maps_dir <- "C://Users//cmcm//OneDrive - University of California, Davis//Documents//Github//CUE-UCD//Graphs//KEGG//Site"
  
  withr::with_dir(kegg_maps_dir, {
    for (pw in 1:nrow(combined_pathways_df)) {
      
      current_pathway_id <- combined_pathways_df[[pw, 1]]
      
      # 1. Filter for GENES in the current pathway
      genes_in_pathway <- mutate(paths_df, 
                                 Path = str_remove(Path, "ko|map"),
                                 Gene = str_remove(Gene, "ko:")) %>%
        dplyr::filter(Path == current_pathway_id)
      
      # 2. Filter for METABOLITES in the current pathway
      # Assumes paths_df2 has columns 'Path' and 'Metabolite'
      # Assumes metabolite IDs are like "cpd:C00001", remove "cpd:"
      
      metabolites_in_pathway <- mutate(paths_df2, 
                                       Path = str_remove(Path, "ko|map"),
                                       Metabolite = str_remove(Metabolite, "cpd:")) %>%
        dplyr::filter(Path == current_pathway_id)
      
      # 3. Check how many of your genes/metabolites have DATA
      genes_with_data <- names(pv_data) %in% genes_in_pathway$Gene
      metabolites_with_data <- names(pv_data2) %in% metabolites_in_pathway$Metabolite
      
      num_genes <- sum(genes_with_data)
      num_metabolites <- sum(metabolites_with_data)
      
      print(paste("Pathway:", current_pathway_id, "| Genes with data:", num_genes, "| Metabolites with data:", num_metabolites))
      
      # 4. New condition: Skip only if there is NO data for EITHER genes or metabolites
      if (num_genes == 0 && num_metabolites == 0) {
        print("--> Skipping: No data found for this pathway.")
        next 
      }
      
      # Generate the pathway map
      pathview(gene.data = pv_data,
               cpd.data = pv_data2,
               pathway.id = current_pathway_id,
               species = "ko",
               out.suffix = paste0(str_remove_all(combined_pathways_df[pw, 2], '/'), ".Site.ko.data"),
               kegg.native = TRUE,
               same.layer = FALSE,
               low = list(gene = "#0072B2", cpd = "#C51B7D"),
               mid = list(gene = "gray", cpd = "white"),
               high = list(gene = "#D55E00", cpd = "#F0E442"),
               bins = list(gene = 25, cpd = 25),
               kegg.dir = kegg_maps_dir)
    }
  })
  
  
  
  
  
  ##Plotting KEGG pathways D
  
  
  
  
  ko_sig=edger_res%>%
    mutate(ID=str_c('ko:', ID))%>%
    filter(D_FDR < 0.05 & abs(D_logFC) >.5)%>%
    left_join(.,ko_anno, by=c('ID'='gene'))
  
  pv_data=as.numeric(ko_sig$D_logFC)
  names(pv_data)=ko_sig$ID%>%
    str_replace(., 'ko:', '')
  
  pathways=paths_df%>%
    left_join(., paths_names)%>%
    mutate(Gene=str_replace(.$Gene, 'ko:', ''))%>%
    filter(., Gene %in% names(pv_data))%>%
    mutate(Path=str_replace(.$Path, 'path:', ''))%>%
    group_by(Path, Name)%>%
    summarise(cnt=n())%>%
    arrange(desc(cnt))
  
  ko_sig2=edger_res2%>%
    mutate(ID=str_c('ko:', ID))%>%
    filter(D_FDR < 0.05 & abs(D_logFC) >.5)%>%
    left_join(.,ko_anno2, by=c('ID'='gene'))
  
  pv_data2=as.numeric(ko_sig2$D_logFC)
  names(pv_data2)=ko_sig2$KEGG
  
  pathways2=paths_df2%>%
    left_join(., paths_names2)%>%
    mutate(Path = str_replace(Path, 'map', ''))%>%
    group_by(Path, Name)%>%
    summarise(cnt=n())%>%
    arrange(desc(cnt))
  
  
  
  
  combined_pathways_df <- bind_rows(pathways, pathways2)
  
  
  
  
  
  
  
  kegg_maps_dir <- "C://Users//cmcm//OneDrive - University of California, Davis//Documents//Github//CUE-UCD//Graphs//KEGG//D"
  
  withr::with_dir(kegg_maps_dir, {
    for (pw in 1:nrow(combined_pathways_df)) {
      
      current_pathway_id <- combined_pathways_df[[pw, 1]]
      
      # 1. Filter for GENES in the current pathway
      genes_in_pathway <- mutate(paths_df, 
                                 Path = str_remove(Path, "ko|map"),
                                 Gene = str_remove(Gene, "ko:")) %>%
        dplyr::filter(Path == current_pathway_id)
      
      # 2. Filter for METABOLITES in the current pathway
      # Assumes paths_df2 has columns 'Path' and 'Metabolite'
      # Assumes metabolite IDs are like "cpd:C00001", remove "cpd:"
      
      metabolites_in_pathway <- mutate(paths_df2, 
                                       Path = str_remove(Path, "ko|map"),
                                       Metabolite = str_remove(Metabolite, "cpd:")) %>%
        dplyr::filter(Path == current_pathway_id)
      
      # 3. Check how many of your genes/metabolites have DATA
      genes_with_data <- names(pv_data) %in% genes_in_pathway$Gene
      metabolites_with_data <- names(pv_data2) %in% metabolites_in_pathway$Metabolite
      
      num_genes <- sum(genes_with_data)
      num_metabolites <- sum(metabolites_with_data)
      
      print(paste("Pathway:", current_pathway_id, "| Genes with data:", num_genes, "| Metabolites with data:", num_metabolites))
      
      # 4. New condition: Skip only if there is NO data for EITHER genes or metabolites
      if (num_genes == 0 && num_metabolites == 0) {
        print("--> Skipping: No data found for this pathway.")
        next 
      }
      
      # Generate the pathway map
      pathview(gene.data = pv_data,
               cpd.data = pv_data2,
               pathway.id = current_pathway_id,
               species = "ko",
               out.suffix = paste0(str_remove_all(combined_pathways_df[pw, 2], '/'), ".D.ko.data"),
               kegg.native = TRUE,
               same.layer = FALSE,
               low = list(gene = "#0072B2", cpd = "#C51B7D"),
               mid = list(gene = "gray", cpd = "white"),
               high = list(gene = "#D55E00", cpd = "#F0E442"),
               bins = list(gene = 25, cpd = 25),
               kegg.dir = kegg_maps_dir)
    }
  })
  
  
  
  list(
       
  )
  
}

plot_network = function(gg_gene, gg_metab, sample_data_4,sample_data_11){
  
  
  
  `%||%` <- function(a,b) if (is.null(a)) b else a
  
  .var_ok <- function(x) is.numeric(x) && is.finite(stats::var(x, na.rm = TRUE)) && stats::sd(x, na.rm = TRUE) > 0
  .factor_ok <- function(x) (is.character(x) || is.factor(x)) && length(unique(stats::na.omit(x))) >= 2
  
  # --- PATCH 1: configurable min_n inside prepare ---
  .prepare_triplet_data <- function(df, X, M, Y, covars = NULL, min_n = 6) {
    `%||%` <- function(a,b) if (is.null(a)) b else a
    cols_xmy <- c(X, M, Y)
    if (!all(cols_xmy %in% names(df))) return(list(d = NULL, reason = "missing_X_M_or_Y"))
    
    covars_use <- intersect(covars %||% character(0), names(df))
    d0 <- df[, c(cols_xmy, covars_use), drop = FALSE]
    
    d  <- tidyr::drop_na(d0)
    if (nrow(d) < min_n) return(list(d = NULL, reason = "n_too_small"))
    
    if (length(covars_use)) {
      keep_cov <- vapply(d[covars_use], function(v) {
        if (is.numeric(v)) (is.finite(stats::var(v, na.rm = TRUE)) && stats::sd(v, na.rm = TRUE) > 0)
        else ((is.character(v) || is.factor(v)) && length(unique(stats::na.omit(v))) >= 2)
      }, logical(1))
      covars_use <- covars_use[keep_cov]
      d <- d[, c(cols_xmy, covars_use), drop = FALSE]
    }
    
    .var_ok <- function(x) is.numeric(x) && is.finite(stats::var(x, na.rm = TRUE)) && stats::sd(x, na.rm = TRUE) > 0
    if (!(.var_ok(d[[X]]) && .var_ok(d[[M]]) && .var_ok(d[[Y]]))) {
      return(list(d = NULL, reason = "zero_variance_X_M_or_Y"))
    }
    list(d = d, covars_use = covars_use, reason = NULL)
  }
  
  # --- PATCH 2: robust formulas + forward min_n ---
  # safe formula builder (handles odd column names)
  .mk_formula <- function(lhs, rhs_vars) {
    rhs <- paste(paste0("`", rhs_vars, "`"), collapse = " + ")
    stats::as.formula(paste0("`", lhs, "` ~ ", rhs))
  }
  
  # fallback: nonparametric bootstrap for linear mediation
  .bootstrap_linear_mediation <- function(d, X, M, Y, covars = character(0), B = 1000, seed = 1L) {
    set.seed(seed)
    # Point estimates from full sample
    fM <- .mk_formula(M, c(X, covars))
    fY <- .mk_formula(Y, c(X, M, covars))
    fYt<- .mk_formula(Y, c(X, covars))
    
    mfit <- stats::lm(fM, d)
    yfit <- stats::lm(fY, d)
    tfit <- stats::lm(fYt, d)
    
    a   <- unname(coef(mfit)[X])
    b   <- unname(coef(yfit)[M])
    cp  <- unname(coef(yfit)[X])
    ctot<- unname(coef(tfit)[X])
    
    acme_hat <- a * b
    ade_hat  <- cp
    tot_hat  <- ctot
    prop_hat <- if (is.finite(tot_hat) && tot_hat != 0) acme_hat / tot_hat else NA_real_
    
    # Bootstrap draws
    n <- nrow(d)
    acme_b <- ade_b <- tot_b <- numeric(B)
    for (bidx in seq_len(B)) {
      idx <- sample.int(n, n, replace = TRUE)
      db  <- d[idx, , drop = FALSE]
      # guard against degenerate resamples
      ok <- try({
        mfit_b <- stats::lm(fM, db)
        yfit_b <- stats::lm(fY, db)
        tfit_b <- stats::lm(fYt, db)
        a_b    <- unname(coef(mfit_b)[X])
        b_b    <- unname(coef(yfit_b)[M])
        cp_b   <- unname(coef(yfit_b)[X])
        ct_b   <- unname(coef(tfit_b)[X])
        acme_b[bidx] <- a_b * b_b
        ade_b[bidx]  <- cp_b
        tot_b[bidx]  <- ct_b
        TRUE
      }, silent = TRUE)
      if (inherits(ok, "try-error")) {
        acme_b[bidx] <- NA_real_; ade_b[bidx] <- NA_real_; tot_b[bidx] <- NA_real_
      }
    }
    # CIs and p-values (percentile CI; two-sided p from bootstrap null mass)
    q <- function(x) quantile(x, probs = c(0.025, 0.975), na.rm = TRUE, names = FALSE)
    ci_acme <- q(acme_b); ci_ade <- q(ade_b); ci_tot <- q(tot_b)
    
    pfrom <- function(x, hat) {
      # two-sided: 2 * min( P(X>=hat), P(X<=hat) )
      x <- x[is.finite(x)]
      if (!length(x) || !is.finite(hat)) return(NA_real_)
      p_hi <- mean(x >= hat); p_lo <- mean(x <= hat)
      2 * min(p_hi, p_lo)
    }
    
    list(
      acme = acme_hat, acme_ci = ci_acme, acme_p = pfrom(acme_b, acme_hat),
      ade  = ade_hat,  ade_ci  = ci_ade,  ade_p  = pfrom(ade_b,  ade_hat),
      tot  = tot_hat,  tot_ci  = ci_tot,  tot_p  = pfrom(tot_b,  tot_hat),
      prop = prop_hat
    )
  }
  
  # ---- DROP-IN: robust mediation_test with fallback ----
  mediation_test <- function(df, X, M, Y, covars = NULL, boot = 2000, robust = TRUE, min_n = 6) {
    `%||%` <- function(a,b) if (is.null(a)) b else a
    
    # Prep data (listwise complete on X, M, Y + pruned covars)
    prep <- .prepare_triplet_data(df, X, M, Y, covars, min_n = min_n)
    if (is.null(prep$d)) {
      return(tibble::tibble(
        X=X, M=M, Y=Y, n=NA_integer_,
        acme=NA_real_, acme_ci_low=NA_real_, acme_ci_high=NA_real_, acme_p=NA_real_,
        ade =NA_real_, ade_ci_low =NA_real_, ade_ci_high =NA_real_, ade_p =NA_real_,
        tot =NA_real_, tot_ci_low=NA_real_, tot_ci_high=NA_real_, tot_p=NA_real_,
        prop_med=NA_real_, reason = prep$reason
      ))
    }
    d <- prep$d
    covars_use <- prep$covars_use %||% character(0)
    
    # Try mediation::mediate first (fast, standard)
    fM <- .mk_formula(M, c(X, covars_use))
    fY <- .mk_formula(Y, c(X, M, covars_use))
    
    try_med <- try({
      mfit <- stats::lm(formula = fM, data = d)
      yfit <- stats::lm(formula = fY, data = d)
      med  <- mediation::mediate(mfit, yfit, treat = X, mediator = M,
                                 robustSE = robust, boot = TRUE, sims = boot)
      tibble::tibble(
        X=X, M=M, Y=Y, n=nrow(d),
        acme = med$d0,  acme_ci_low=med$d0.ci[1], acme_ci_high=med$d0.ci[2], acme_p = med$d0.p,
        ade  = med$z0,  ade_ci_low =med$z0.ci[1], ade_ci_high =med$z0.ci[2], ade_p  = med$z0.p,
        tot  = med$tau.coef, tot_ci_low=med$tau.ci[1], tot_ci_high=med$tau.ci[2], tot_p=med$tau.p,
        prop_med = med$n0, reason = NA_character_
      )
    }, silent = TRUE)
    
    if (!inherits(try_med, "try-error")) {
      return(try_med)
    }
    
    # Fallback: bootstrap linear mediation (always returns numbers if models fit)
    fb <- try(.bootstrap_linear_mediation(d, X, M, Y, covars_use, B = max(500, min(2000, boot))), silent = TRUE)
    if (inherits(fb, "try-error")) {
      # even fallback failed: return informative reason
      return(tibble::tibble(
        X=X, M=M, Y=Y, n=nrow(d),
        acme=NA_real_, acme_ci_low=NA_real_, acme_ci_high=NA_real_, acme_p=NA_real_,
        ade =NA_real_, ade_ci_low =NA_real_, ade_ci_high =NA_real_, ade_p =NA_real_,
        tot =NA_real_, tot_ci_low=NA_real_, tot_ci_high=NA_real_, tot_p=NA_real_,
        prop_med=NA_real_,
        reason = paste0("mediate_error_and_fallback_failed: ", conditionMessage(attr(try_med, "condition")))
      ))
    }
    
    tibble::tibble(
      X=X, M=M, Y=Y, n=nrow(d),
      acme = fb$acme,  acme_ci_low = fb$acme_ci[1], acme_ci_high = fb$acme_ci[2], acme_p = fb$acme_p,
      ade  = fb$ade,   ade_ci_low  = fb$ade_ci[1],  ade_ci_high  = fb$ade_ci[2],  ade_p  = fb$ade_p,
      tot  = fb$tot,   tot_ci_low  = fb$tot_ci[1],  tot_ci_high  = fb$tot_ci[2],  tot_p  = fb$tot_p,
      prop_med = fb$prop,
      reason = "fallback_bootstrap"
    )
  }
  
  # --- PATCH 3: screen now passes min_n through; supports "median_xmy" ---
  run_mediation_screen <- function(df, trips, covars = NULL, boot = 2000,
                                   min_n = 6,
                                   impute = c("none","median_xm","median_xmy")) {
    `%||%` <- function(a,b) if (is.null(a)) b else a
    impute <- match.arg(impute)
    if (is.null(trips) || !nrow(trips)) return(tibble::tibble())
    
    df_imp <- df
    if (impute %in% c("median_xm","median_xmy")) {
      xm_vars <- unique(c(trips$X, trips$M)); xm_vars <- xm_vars[xm_vars %in% names(df_imp)]
      for (v in xm_vars) {
        medv <- stats::median(df_imp[[v]], na.rm = TRUE)
        if (is.finite(medv)) df_imp[[v]][is.na(df_imp[[v]])] <- medv
      }
    }
    if (impute == "median_xmy") {
      y_vars <- unique(trips$Y); y_vars <- y_vars[y_vars %in% names(df_imp)]
      for (v in y_vars) {
        medv <- stats::median(df_imp[[v]], na.rm = TRUE)
        if (is.finite(medv)) df_imp[[v]][is.na(df_imp[[v]])] <- medv
      }
    }
    
    exists_mask <- mapply(function(X,M,Y) all(c(X,M,Y) %in% names(df_imp)),
                          trips$X, trips$M, trips$Y)
    trips2 <- trips[exists_mask, , drop = FALSE]
    if (!nrow(trips2)) return(tibble::tibble())
    
    # fast prefilter by achievable n with current covariates
    n_after <- purrr::pmap_int(
      list(trips2$X, trips2$M, trips2$Y),
      \(X,M,Y) {
        prep <- .prepare_triplet_data(df_imp, X, M, Y, covars, min_n = min_n)
        if (is.null(prep$d)) 0L else nrow(prep$d)
      }
    )
    trips2 <- trips2[n_after >= min_n, , drop = FALSE]
    if (!nrow(trips2)) return(tibble::tibble())
    
    res <- purrr::pmap_dfr(
      list(trips2$X, trips2$M, trips2$Y),
      \(X,M,Y) mediation_test(df_imp, X, M, Y, covars = covars, boot = boot, min_n = min_n)
    )
    
    if (!nrow(res)) return(res)
    
    res %>%
      dplyr::mutate(
        acme_fdr = ifelse(is.finite(acme_p), p.adjust(acme_p, method = "BH"), NA_real_),
        ade_fdr  = ifelse(is.finite(ade_p),  p.adjust(ade_p,  method = "BH"), NA_real_),
        tot_fdr  = ifelse(is.finite(tot_p),  p.adjust(tot_p,  method = "BH"), NA_real_)
      ) %>%
      dplyr::arrange(dplyr::desc(!is.na(acme_p)), acme_p)
  }
  
  
  # =========================
  # Integrated network + mediation 
  # =========================
  sample_data_4<- sample_data_4 %>%
    left_join(sample_data_11, by= "ID") %>%
    mutate(
      X_K = as.numeric(X_K),
      X_Na = as.numeric(X_Na)
    )
  suppressPackageStartupMessages({
    library(tidyverse); library(janitor); library(ppcor); library(Hmisc)
    library(igraph); library(tidygraph); library(ggraph); library(scales)
    library(mediation); library(sandwich); library(broom)
  })
  
  `%||%` <- function(a,b) if (is.null(a)) b else a
  
  # ---------- Helpers ----------
  clean_meta <- function(df){
    df %>%
      janitor::clean_names() %>%
      mutate(across(where(is.character), ~na_if(.x, "")))
  }
  
  assert_cols <- function(df, cols, label=""){
    miss <- setdiff(cols, colnames(df))
    if (length(miss)) stop(sprintf("Missing %s columns: %s", label, paste(miss, collapse=", ")))
  }
  
  top_var <- function(mat, n = 150){
    if (is.null(mat) || ncol(mat) == 0) return(mat)
    v <- apply(mat, 2, function(x) var(x, na.rm = TRUE))
    keep <- names(sort(v, decreasing = TRUE))[seq_len(min(n, length(v)))]
    mat[, keep, drop = FALSE]
  }
  
  sanitize_feature_matrix <- function(X, prefix){
    # Ensure data.frame with rownames = IDs; enforce valid, unique, prefixed colnames
    X <- as.matrix(X)
    if (is.null(colnames(X))) colnames(X) <- paste0(prefix, "_", seq_len(ncol(X)))
    bad <- is.na(colnames(X)) | trimws(colnames(X)) == ""
    if (any(bad)) colnames(X)[bad] <- paste0(prefix, "_", which(bad))
    colnames(X) <- make.names(colnames(X), unique = TRUE)
    needs_prefix <- !startsWith(colnames(X), paste0(prefix, "_"))
    colnames(X)[needs_prefix] <- paste0(prefix, "_", colnames(X)[needs_prefix])
    as.data.frame(X, check.names = FALSE)
  }
  
  # ---------- ALL-VARS adaptor ----------
  # Pulls:
  # • Enzymes:    from sample_data_4 (case-insensitive match to enzyme_guess)
  # • Gases:      from sample_data_4 (case-insensitive match to gas_guess)
  # • Soil vars:  ALL OTHER numeric columns in sample_data_4
  # • Covariates: all non-numeric columns (unless you pass a subset)
  # • Genes/metabs: normalized matrices from gg_gene/gg_metab ($ko_norm_wis)
  prepare_layers_from_cam_all <- function(
    soil_meta, gene_res, metab_res,
    enzyme_guess = c("bg","cbh","lap","nag","phos","fda","poxc"),
    gas_guess    = c("co2","ch4","n2o"),
    covars = NULL,      # if NULL => all non-numeric (except id)
    n_gene = 2000,
    n_metab = 2000,
    n_soil  = Inf       # set to a number (e.g., 150) to cap soil vars by variance
  ){
    # Clean metadata
    sm <- clean_meta(soil_meta)
    if (!"id" %in% names(sm)) stop("Expected an 'ID' column; after clean_names() it should be 'id'.")
    
    # Covariates
    if (is.null(covars)) {
      covars <- names(sm)[!vapply(sm, is.numeric, logical(1))]
      covars <- setdiff(covars, "id")
    }
    covars <- intersect(covars, names(sm))
    
    # Numeric candidates in sample_data_4
    numeric_vars <- names(sm)[vapply(sm, is.numeric, logical(1))]
    
    # Case-insensitive enzyme/gas matching
    enzymes_all <- names(sm)[tolower(names(sm)) %in% tolower(enzyme_guess)]
    gases_all   <- names(sm)[tolower(names(sm)) %in% tolower(gas_guess)]
    
    # Remaining soil variables
    soil_vars <- setdiff(numeric_vars, c(enzymes_all, gases_all, "id"))
    
    # Optional cap by variance
    if (is.finite(n_soil) && length(soil_vars) > 0) {
      soil_mat_tmp <- sm %>%
        dplyr::select(dplyr::any_of(c("id", soil_vars))) %>%
        column_to_rownames("id") %>% as.matrix()
      soil_mat_tmp <- top_var(soil_mat_tmp, n = n_soil)
      soil_vars <- colnames(soil_mat_tmp)
    }
    
    # Bring in gene & metabolite matrices, sanitize
    G_raw <- gene_res$ko_norm_wis
    M_raw <- metab_res$ko_norm_wis
    if (is.null(G_raw) || is.null(M_raw))
      stop("plot_gene()/plot_metab() must return 'ko_norm_wis' in their lists.")
    
    G_df  <- sanitize_feature_matrix(G_raw,  "gene")  %>% rownames_to_column("id")
    M_df  <- sanitize_feature_matrix(M_raw,  "metab") %>% rownames_to_column("id")
    
    # Variance capping (stability)
    G_sel <- G_df %>% column_to_rownames("id") %>% top_var(n_gene)  %>% as.data.frame() %>% rownames_to_column("id")
    M_sel <- M_df %>% column_to_rownames("id") %>% top_var(n_metab) %>% as.data.frame() %>% rownames_to_column("id")
    
    # Merge: ID + covars + enzymes + gases + soil vars + genes + metabolites
    keep_meta <- c("id", covars, enzymes_all, gases_all, soil_vars) %>% unique()
    sm_keep   <- sm %>% dplyr::select(dplyr::any_of(keep_meta))
    
    merged <- sm_keep %>%
      left_join(G_sel, by = "id") %>%
      left_join(M_sel, by = "id")
    
    # Layer map
    layers <- list(
      enzymes     = enzymes_all,
      gases       = gases_all,
      soil_vars   = soil_vars,
      genes       = setdiff(colnames(G_sel), "id"),
      metabolites = setdiff(colnames(M_sel), "id")
    )
    layers <- layers[lengths(layers) > 0]
    
    list(merged = merged, layers = layers, covars = covars)
  }
  
  # ---------- Association helpers (partial Spearman) ----------
  assoc_pairs <- function(df, A, B, covars = NULL, adjust = "BH", method = "spearman"){
    one <- function(x,y){
      xx <- df[[x]]; yy <- df[[y]]
      ok <- is.finite(xx) & is.finite(yy)
      if (!is.null(covars) && length(covars) > 0){
        Z <- df[ok, covars, drop=FALSE] %>% mutate(across(everything(), as.numeric))
        res <- try(ppcor::pcor.test(as.numeric(xx[ok]), as.numeric(yy[ok]), Z, method = method), silent = TRUE)
        if (inherits(res, "try-error")) return(tibble(var1=x,var2=y,rho=NA_real_,p=NA_real_,n=sum(ok)))
        tibble(var1=x,var2=y,rho=res$estimate,p=res$p.value,n=sum(ok))
      } else {
        rc <- suppressWarnings(Hmisc::rcorr(cbind(as.numeric(xx), as.numeric(yy)), type = method))
        tibble(var1=x,var2=y,rho=rc$r[1,2],p=rc$P[1,2],n=sum(ok))
      }
    }
    edges <- purrr::map_dfr(A, \(a) purrr::map_dfr(B, \(b) one(a,b)))
    mutate(edges, fdr = p.adjust(p, method = adjust))
  }
  
  assoc_within <- function(df, vars, covars = NULL, adjust="BH", method="spearman"){
    if (length(vars) < 2) return(tibble(var1=character(), var2=character(), rho=double(), p=double(), n=integer(), fdr=double()))
    cmb <- t(combn(vars,2))
    purrr::pmap_dfr(list(cmb[,1], cmb[,2]), \(x,y) assoc_pairs(df, x, y, covars = covars, adjust = adjust, method = method))
  }
  
  layer_pairs <- function(names_vec) combn(names_vec, 2, simplify = FALSE)
  
  # ---------- Network builder & plot ----------
  build_integrated_network <- function(df, layers,
                                       edge_specs = NULL,   # NULL => connect all layer pairs
                                       covars = NULL,
                                       method = "spearman",
                                       fdr_thresh = 0.05,
                                       abs_rho_min = 0.3,
                                       do_within = TRUE, within_skip = c("gases")) {
    
    if (is.null(edge_specs)) {
      edge_specs <- list(
        between = layer_pairs(names(layers)),
        within  = if (do_within) setdiff(names(layers), within_skip) else character(0)
      )
    }
    
    between_edges <- purrr::map_dfr(edge_specs$between, function(pair){
      A <- layers[[pair[1]]]; B <- layers[[pair[2]]]
      assoc_pairs(df, A, B, covars = covars, method = method) %>%
        mutate(layer1 = pair[1], layer2 = pair[2], edge_type = "between")
    })
    
    within_edges <- purrr::map_dfr(edge_specs$within, function(layer_nm){
      vars <- layers[[layer_nm]]
      assoc_within(df, vars, covars = covars, method = method) %>%
        mutate(layer1 = layer_nm, layer2 = layer_nm, edge_type = "within")
    })
    
    all_edges <- bind_rows(between_edges, within_edges) %>%
      mutate(sig = fdr < fdr_thresh & abs(rho) >= abs_rho_min) %>%
      filter(sig)
    
    v_tbl <- tibble(name = unique(c(all_edges$var1, all_edges$var2))) %>%
      mutate(layer = purrr::map_chr(name, function(v){
        nm <- names(layers)[purrr::map_lgl(layers, ~ v %in% .x)]
        if (length(nm)==0) "unknown" else nm[1]
      }))
    
    g <- graph_from_data_frame(
      all_edges %>% transmute(from = var1, to = var2, rho, p, fdr, n, edge_type, layer1, layer2),
      vertices = v_tbl,
      directed = FALSE
    )
    
    list(graph = g, edges = all_edges, vertices = v_tbl)
  }
  
  fast_build_integrated_network <- function(df, layers,
                                            edge_specs = NULL,   # NULL => connect all layer pairs
                                            covars = NULL,       # character vector of covariate columns in df
                                            fdr_thresh = 0.05,
                                            abs_rho_min = 0.3,
                                            do_within = TRUE, within_skip = c("gases"),
                                            verbose = TRUE) {
    
    # union of variables across layers
    all_vars <- unique(unlist(layers, use.names = FALSE))
    all_vars <- all_vars[all_vars %in% colnames(df)]
    if (length(all_vars) < 2) stop("Not enough variables to correlate.")
    
    # covariate design matrix (can be NULL)
    rows_ok <- rep(TRUE, nrow(df))
    Z <- NULL; q <- 0L
    if (!is.null(covars) && length(covars) > 0) {
      rows_ok <- rows_ok & stats::complete.cases(df[, covars, drop = FALSE])
      Z <- stats::model.matrix(~ . , data = df[rows_ok, covars, drop = FALSE])
      q <- qr(Z)$rank - 1L  # subtract intercept
    }
    
    # matrix of features; require complete cases across selected vars
    rows_ok <- rows_ok & stats::complete.cases(df[, all_vars, drop = FALSE])
    Y <- as.matrix(df[rows_ok, all_vars, drop = FALSE])
    Y <- apply(Y, 2, function(x) as.numeric(x))  # coerce safely
    n <- nrow(Y)
    if (n < 10) stop("Too few complete rows for correlation after NA filtering.")
    
    # Spearman via rank transform
    Y_rank <- apply(Y, 2, function(x) rank(x, ties.method = "average") / (length(x) + 1))
    
    # partial out covariates with one QR projection
    Y_res <- Y_rank
    if (!is.null(Z)) {
      qrZ <- qr(Z)
      Y_res <- Y_rank - qr.fitted(qrZ, Y_rank)
    }
    
    # correlation matrix on residuals
    R <- suppressWarnings(cor(Y_res, method = "pearson", use = "pairwise.complete.obs"))
    
    # p-values (df adjusted for covariates)
    df_eff <- max(n - q - 2L, 1L)
    rvec <- R[upper.tri(R)]
    tvec <- rvec * sqrt(df_eff / pmax(1e-12, 1 - rvec^2))
    pvec <- 2 * pt(abs(tvec), df = df_eff, lower.tail = FALSE)
    
    # edge frame from upper triangle
    idx <- which(upper.tri(R), arr.ind = TRUE)
    edge_df <- tibble::tibble(
      var1 = colnames(R)[idx[,1]],
      var2 = colnames(R)[idx[,2]],
      rho  = rvec,
      p    = pvec,
      fdr  = p.adjust(pvec, method = "BH")
    )
    
    # default edge_specs: all between pairs, within all except within_skip
    layer_pairs <- function(nm) combn(nm, 2, simplify = FALSE)
    if (is.null(edge_specs)) {
      edge_specs <- list(
        between = layer_pairs(names(layers)),
        within  = if (do_within) setdiff(names(layers), within_skip) else character(0)
      )
    }
    
    # map variable -> layer
    var2layer <- rep(names(layers), times = vapply(layers, length, integer(1)))
    names(var2layer) <- unlist(layers, use.names = FALSE)
    
    # keep only allowed pairs
    allowed_between <- edge_specs$between %||% list()
    allowed_within  <- edge_specs$within  %||% character(0)
    
    allowed_pair <- function(a, b) {
      la <- var2layer[[a]]; lb <- var2layer[[b]]
      if (is.null(la) || is.null(lb)) return(FALSE)
      if (la == lb) {
        return(la %in% allowed_within)
      } else {
        for (pr in allowed_between) {
          if ((la == pr[1] && lb == pr[2]) || (la == pr[2] && lb == pr[1])) return(TRUE)
        }
        return(FALSE)
      }
    }
    
    edge_df <- edge_df %>%
      dplyr::rowwise() %>% dplyr::mutate(allowed = allowed_pair(var1, var2)) %>%
      dplyr::ungroup() %>% dplyr::filter(allowed) %>%
      dplyr::mutate(sig = (fdr < fdr_thresh) & (abs(rho) >= abs_rho_min)) %>%
      dplyr::filter(sig) %>%
      dplyr::select(-allowed, -sig)
    
    # vertices and graph
    v_names <- unique(c(edge_df$var1, edge_df$var2))
    v_tbl <- tibble::tibble(
      name  = v_names,
      layer = vapply(v_names, function(v) var2layer[[v]] %||% "unknown", character(1))
    )
    
    g <- igraph::graph_from_data_frame(
      edge_df %>% dplyr::transmute(from = var1, to = var2, rho, p, fdr),
      vertices = v_tbl, directed = FALSE
    )
    
    if (verbose) {
      message(sprintf(
        "fast_build_integrated_network: n=%d rows, q=%d (covariate df removed), vars=%d, edges kept=%d",
        n, q, length(all_vars), igraph::ecount(g)
      ))
    }
    
    list(graph = g, edges = edge_df, vertices = v_tbl)
  }
  
  
  
  
  
  
  plot_network_cam <- function(
    g_obj,
    label_mode = c("hubs","all","none"),
    top_n = 40,
    font_family = "sans",
    repel = TRUE,
    max_overlaps = Inf,
    palette = cbPalette2,
    layer_col = NULL,              # <-- NEW: name of the column/attribute with layer info
    legend_title = "Layer"         # optional nicety
  ){
    label_mode <- match.arg(label_mode)
    g <- g_obj$graph
    
    # resolve layer vector for vertices named in vdf$name
    resolve_layer <- function(vdf, vertices_tbl, g, layer_col){
      # 1) explicit: use provided column (in vertices_tbl or vdf or igraph attr)
      if (!is.null(layer_col)) {
        if (layer_col %in% names(vertices_tbl)) {
          return(vertices_tbl[[layer_col]][match(vdf$name, vertices_tbl$name)])
        }
        if (layer_col %in% names(vdf)) {
          return(vdf[[layer_col]])
        }
        if (layer_col %in% igraph::vertex_attr_names(g)) {
          va <- igraph::vertex_attr(g, layer_col)
          return(va[match(vdf$name, igraph::V(g)$name)])
        }
        warning(sprintf("layer_col='%s' not found; falling back to auto-detection.", layer_col))
      }
      
      # 2) try common names in vertices_tbl
      candidates <- c("layer","Layer","group","Group","type","Type","category","Category","set","Set")
      hit <- candidates[candidates %in% names(vertices_tbl)][1]
      if (!is.na(hit)) {
        return(vertices_tbl[[hit]][match(vdf$name, vertices_tbl$name)])
      }
      
      # 3) try common names in igraph vertex attributes
      va_names <- igraph::vertex_attr_names(g)
      hit2 <- candidates[candidates %in% va_names][1]
      if (!is.na(hit2)) {
        va <- igraph::vertex_attr(g, hit2)
        return(va[match(vdf$name, igraph::V(g)$name)])
      }
      
      # 4) last resort
      rep("nodes", nrow(vdf))
    }
    
    # --- no-edges branch
    if (igraph::ecount(g) == 0) {
      lay <- ggraph::create_layout(g, layout = "fr") |> as_tibble()
      vdf <- lay |>
        dplyr::left_join(g_obj$vertices, by = "name") |>
        dplyr::mutate(label = name)
      
      vdf$layer <- resolve_layer(vdf, g_obj$vertices, g, layer_col)
      
      p <- ggraph::ggraph(g, layout = "fr") +
        ggraph::geom_node_point(data = vdf, aes(x = x, y = y, color = layer), size = 3) +
        ggraph::geom_node_text(data = vdf, aes(x = x, y = y, label = label),
                               family = font_family, size = 3, repel = TRUE) +
        ggraph::theme_graph(base_family = font_family)
      
      if (is.character(palette) && length(palette) == 1 &&
          palette %in% rownames(RColorBrewer::brewer.pal.info)) {
        p <- p + scale_color_brewer(palette = palette, name = legend_title)
      } else {
        p <- p + scale_color_manual(values = palette, name = legend_title)
      }
      return(p)
    }
    
    # --- edges present
    lay <- ggraph::create_layout(g, layout = "fr") |> as_tibble()
    
    w <- abs(igraph::E(g)$rho)
    igraph::E(g)$w <- if (length(w)) w else 1
    strength <- igraph::strength(g, weights = igraph::E(g)$w)
    degree   <- igraph::degree(g)
    
    vdf <- lay |>
      dplyr::left_join(g_obj$vertices, by = "name") |>
      dplyr::mutate(
        strength = strength[match(name, igraph::V(g)$name)],
        degree   = degree[match(name, igraph::V(g)$name)]
      )
    
    vdf$layer <- resolve_layer(vdf, g_obj$vertices, g, layer_col)
    
    to_label <- switch(
      label_mode,
      "none" = rep(FALSE, nrow(vdf)),
      "all"  = rep(TRUE,  nrow(vdf)),
      "hubs" = {
        ord <- order(dplyr::coalesce(vdf$strength, 0), dplyr::coalesce(vdf$degree, 0), decreasing = TRUE)
        flag <- rep(FALSE, nrow(vdf))
        flag[ord[seq_len(min(top_n, length(ord)))]] <- TRUE
        flag
      }
    )
    vdf$to_label <- to_label
    
    set.seed(12)
    p <- ggraph::ggraph(g, layout = "manual", x = vdf$x, y = vdf$y) +
      ggraph::geom_edge_link(aes(alpha = abs(rho), linetype = rho > 0)) +
      ggraph::geom_node_point(data = vdf, aes(x = x, y = y, color = layer), size = 3) +
      {
        if (label_mode != "none") {
          ggraph::geom_node_text(
            data = dplyr::filter(vdf, to_label),
            aes(x = x, y = y, label = name),
            repel = repel,
            max.overlaps = if (label_mode == "all") max_overlaps else 1000,
            family = font_family, size = 3
          )
        } else NULL
      } +
      scale_edge_alpha(range = c(0.15, 0.9), guide = "none") +
      scale_edge_linetype_manual(values = c("FALSE"="dashed","TRUE"="solid"), name = "Sign") +
      ggraph::theme_graph(base_family = font_family)
    
    if (is.character(palette) && length(palette) == 1 &&
        palette %in% rownames(RColorBrewer::brewer.pal.info)) {
      p <- p + scale_color_brewer(palette = palette, name = legend_title)
    } else {
      p <- p + scale_color_manual(values = palette, name = legend_title)
    }
    
    p
  }
  
  
  # Normalize edge columns to var1/var2 no matter how they were stored
  .normalize_edges_varcols <- function(net_obj) {
    stopifnot(!is.null(net_obj$edges))
    e <- net_obj$edges
    
    # Already normalized?
    if (all(c("var1","var2") %in% names(e))) {
      e2 <- e %>% dplyr::select(var1, var2) %>% dplyr::mutate(var1 = as.character(var1),
                                                              var2 = as.character(var2))
      return(e2)
    }
    
    # igraph-from/to style?
    if (all(c("from","to") %in% names(e))) {
      e2 <- e %>% dplyr::transmute(var1 = as.character(.data$from),
                                   var2 = as.character(.data$to))
      return(e2)
    }
    
    # Sometimes edges may come in as V1/V2 etc.; try a gentle guess
    cand1 <- intersect(names(e), c("v1","V1","source","src","i","a"))
    cand2 <- intersect(names(e), c("v2","V2","target","tgt","j","b"))
    if (length(cand1) == 1 && length(cand2) == 1) {
      e2 <- e %>% dplyr::transmute(var1 = as.character(.data[[cand1]]),
                                   var2 = as.character(.data[[cand2]]))
      return(e2)
    }
    
    stop("candidate_triplets_from_network(): couldn't find edge columns. ",
         "Expected either (var1,var2) or (from,to). Got: ",
         paste(names(e), collapse = ", "))
  }
  
  candidate_triplets_from_network <- function(net_obj,
                                              role_layers = list(X = NULL, M = NULL, Y = c("gases")),
                                              require_edge_xm = TRUE) {
    stopifnot(!is.null(net_obj$vertices), !is.null(net_obj$edges))
    v <- net_obj$vertices %>% dplyr::select(name, layer)
    
    # normalize edges to var1/var2
    e <- .normalize_edges_varcols(net_obj)
    
    # pools by role; if role layer is NULL, allow all nodes
    pick <- function(role) {
      allowed_layers <- role_layers[[role]]
      if (is.null(allowed_layers)) v$name
      else v %>% dplyr::filter(layer %in% allowed_layers) %>% dplyr::pull(name)
    }
    X_pool <- intersect(pick("X"), v$name)
    M_pool <- intersect(pick("M"), v$name)
    Y_pool <- intersect(pick("Y"), v$name)
    
    # build X–M pairs: either require an observed edge, or all cross-pairs
    if (require_edge_xm) {
      xm_edges <- e %>%
        dplyr::filter((var1 %in% X_pool & var2 %in% M_pool) |
                        (var2 %in% X_pool & var1 %in% M_pool)) %>%
        dplyr::transmute(
          X = dplyr::if_else(var1 %in% X_pool, var1, var2),
          M = dplyr::if_else(var1 %in% X_pool, var2, var1)
        ) %>%
        dplyr::distinct()
    } else {
      xm_edges <- tidyr::crossing(X = X_pool, M = M_pool) %>%
        dplyr::filter(X != M)
    }
    
    # expand with Y from allowed layers; ensure all three are distinct
    trips <- tidyr::crossing(xm_edges, Y = Y_pool) %>%
      dplyr::filter(Y != X, Y != M) %>%
      dplyr::distinct()
    
    tibble::as_tibble(trips)
  }
  
  ensure_id_column <- function(df) {
    nm <- names(df)
    
    has_ID <- "ID" %in% nm
    has_id <- "id" %in% nm
    
    if (has_ID || has_id) return(df)  # already present
    
    # Find Site and Aggregate columns in either case style
    site_col <- dplyr::case_when(
      "Site" %in% nm ~ "Site",
      "site" %in% nm ~ "site",
      TRUE ~ NA_character_
    )
    agg_col <- dplyr::case_when(
      "Aggregate" %in% nm ~ "Aggregate",
      "aggregate" %in% nm ~ "aggregate",
      TRUE ~ NA_character_
    )
    
    # Prefer Sample.ID / sample_id if present
    sid_col <- dplyr::case_when(
      "Sample.ID" %in% nm ~ "Sample.ID",
      "sample_id" %in% nm ~ "sample_id",
      TRUE ~ NA_character_
    )
    
    if (!is.na(sid_col)) {
      df$ID <- df[[sid_col]]
    } else if (!is.na(site_col) && !is.na(agg_col)) {
      df$ID <- paste0(df[[site_col]], "_", df[[agg_col]])
    } else {
      # Fallback: sequential IDs
      df$ID <- seq_len(nrow(df))
    }
    
    df
  }
  
  collapse_by_site_aggregate <- function(prep, keep_covars = FALSE) {
    df <- prep$merged
    
    # Detect column names in either case style
    site_col <- if ("site" %in% names(df)) "site" else if ("Site" %in% names(df)) "Site" else NA_character_
    agg_col  <- if ("aggregate" %in% names(df)) "aggregate" else if ("Aggregate" %in% names(df)) "Aggregate" else NA_character_
    if (is.na(site_col) || is.na(agg_col)) stop("Could not find Site/Aggregate columns in prep$merged.")
    
    df_grp <- df %>%
      dplyr::group_by(.data[[site_col]], .data[[agg_col]]) %>%
      dplyr::summarise(
        dplyr::across(where(is.numeric), ~ mean(.x, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      # rebuild an id that prepare_* expects to exist (even though it's not used later)
      dplyr::mutate(id = paste0(.data[[site_col]], "_", .data[[agg_col]])) %>%
      # move id to first column for neatness
      dplyr::relocate(id, .before = 1)
    
    prep$merged <- df_grp
    
    # Optionally drop covariates entirely (often safer for sparse data)
    if (!keep_covars) {
      prep$covars <- NULL
    } else {
      # keep only site/aggregate as covars if you want minimal adjustment
      prep$covars <- intersect(names(df_grp), c("site","Site","aggregate","Aggregate"))
    }
    
    prep
  }
  fast_build_integrated_network_pairwise <- function(df, layers,
                                                     edge_specs = NULL,
                                                     covars = NULL,
                                                     fdr_thresh = 0.05,
                                                     abs_rho_min = 0.3,
                                                     do_within = TRUE, within_skip = c("gases"),
                                                     min_pair_n = 8,
                                                     verbose = TRUE) {
    `%||%` <- function(a,b) if (is.null(a)) b else a
    
    # ---------------- gather feature vars ----------------
    all_vars <- unique(unlist(layers, use.names = FALSE))
    all_vars <- all_vars[all_vars %in% colnames(df)]
    if (length(all_vars) < 2) stop("Not enough variables to correlate.")
    
    # ---------------- covariates (prune) ----------------
    C <- NULL
    if (!is.null(covars) && length(covars) > 0) {
      covars <- intersect(covars, colnames(df))
      if (length(covars)) {
        C0 <- df[, covars, drop = FALSE]
        C0[] <- lapply(C0, function(x){
          if (is.logical(x) || is.character(x)) factor(x) else if (is.factor(x)) droplevels(x) else x
        })
        keep_nonallna <- vapply(C0, function(x) any(!is.na(x)), logical(1))
        C0 <- C0[, keep_nonallna, drop = FALSE]
        keep_multilevel <- vapply(C0, function(x){
          if (is.factor(x)) nlevels(droplevels(x)) >= 2 else length(unique(stats::na.omit(x))) >= 2
        }, logical(1))
        C0 <- C0[, keep_multilevel, drop = FALSE]
        if (ncol(C0)) C <- C0
      }
    }
    
    cov_ok <- rep(TRUE, nrow(df))
    if (!is.null(C) && ncol(C) > 0) cov_ok <- stats::complete.cases(C)
    
    Z <- NULL; q <- 0L
    if (!is.null(C) && ncol(C) > 0) {
      C1 <- C[cov_ok, , drop = FALSE]
      keep_after <- vapply(C1, function(x){
        if (is.factor(x)) nlevels(droplevels(x)) >= 2 else length(unique(stats::na.omit(x))) >= 2
      }, logical(1))
      C1 <- C1[, keep_after, drop = FALSE]
      if (ncol(C1) > 0) {
        Z <- stats::model.matrix(~ . , data = C1)
        q <- max(qr(Z)$rank - 1L, 0L)
      } else {
        cov_ok <- rep(TRUE, nrow(df)); Z <- NULL; q <- 0L
      }
    }
    
    # ---------------- residualized rank matrix (pairwise) ----------------
    p <- length(all_vars)
    Y_res <- matrix(NA_real_, nrow = nrow(df), ncol = p, dimnames = list(NULL, all_vars))
    if (!is.null(Z)) z_rows <- which(cov_ok)
    
    for (j in seq_len(p)) {
      y <- df[[ all_vars[j] ]]
      if (!is.numeric(y)) y <- suppressWarnings(as.numeric(y))
      obs <- !is.na(y) & cov_ok
      if (sum(obs) >= 3) {
        r <- rank(y[obs], ties.method = "average") / (sum(obs) + 1)
        if (is.null(Z)) {
          Y_res[obs, j] <- r
        } else {
          Zsub <- Z[match(which(obs), z_rows, nomatch = 0L), , drop = FALSE]
          keep <- rowSums(!is.na(Zsub)) > 0
          if (any(keep)) {
            Zsub <- Zsub[keep, , drop = FALSE]
            rsub <- r[keep]
            if (nrow(Zsub) > 1 && ncol(Zsub) <= nrow(Zsub)) {
              qrZ <- qr(Zsub)
              Y_res[which(obs)[keep], j] <- rsub - qr.fitted(qrZ, rsub)
            } else {
              Y_res[which(obs)[keep], j] <- rsub
            }
          }
        }
      }
    }
    
    R <- suppressWarnings(cor(Y_res, use = "pairwise.complete.obs", method = "pearson"))
    if (!is.matrix(R) || all(is.na(R))) {
      if (verbose) message("No pairwise overlap among features; returning empty graph.")
      return(list(graph = igraph::make_empty_graph(),
                  edges = tibble::tibble(), vertices = tibble::tibble(), layers = layers))
    }
    
    idx <- which(upper.tri(R), arr.ind = TRUE)
    if (nrow(idx) == 0) {
      return(list(graph = igraph::make_empty_graph(),
                  edges = tibble::tibble(), vertices = tibble::tibble(), layers = layers))
    }
    
    get_pair_n <- function(i, j) sum(stats::complete.cases(Y_res[, i], Y_res[, j]))
    n_ij <- apply(idx, 1, function(rc) get_pair_n(rc[1], rc[2]))
    
    rvec <- R[upper.tri(R)]
    df_eff <- pmax(n_ij - q - 2L, 1L)
    tvec <- rvec * sqrt(df_eff / pmax(1e-12, 1 - rvec^2))
    pvec <- 2 * stats::pt(abs(tvec), df = df_eff, lower.tail = FALSE)
    
    edge_df <- tibble::tibble(
      var1 = colnames(R)[idx[,1]],
      var2 = colnames(R)[idx[,2]],
      n    = as.integer(n_ij),
      rho  = rvec,
      p    = pvec
    ) %>%
      dplyr::filter(!is.na(rho), n >= min_pair_n) %>%
      dplyr::mutate(fdr = p.adjust(p, method = "BH"))
    
    layer_pairs <- function(nm) combn(nm, 2, simplify = FALSE)
    if (is.null(edge_specs)) {
      edge_specs <- list(
        between = layer_pairs(names(layers)),
        within  = if (do_within) setdiff(names(layers), within_skip) else character(0)
      )
    }
    
    var2layer <- rep(names(layers), times = vapply(layers, length, integer(1)))
    names(var2layer) <- unlist(layers, use.names = FALSE)
    
    allowed_between <- edge_specs$between %||% list()
    allowed_within  <- edge_specs$within  %||% character(0)
    allowed_pair <- function(a, b) {
      la <- var2layer[[a]]; lb <- var2layer[[b]]
      if (is.null(la) || is.null(lb)) return(FALSE)
      if (la == lb) la %in% allowed_within else {
        for (pr in allowed_between) if ((la==pr[1] && lb==pr[2]) || (la==pr[2] && lb==pr[1])) return(TRUE)
        FALSE
      }
    }
    
    edge_df <- edge_df %>%
      dplyr::rowwise() %>% dplyr::mutate(allowed = allowed_pair(var1, var2)) %>%
      dplyr::ungroup() %>% dplyr::filter(allowed) %>%
      dplyr::mutate(sig = (fdr < fdr_thresh) & (abs(rho) >= abs_rho_min)) %>%
      dplyr::filter(sig) %>% dplyr::select(-allowed, -sig)
    
    v_names <- unique(c(edge_df$var1, edge_df$var2))
    v_tbl <- tibble::tibble(
      name  = v_names,
      layer = vapply(v_names, function(v) var2layer[[v]] %||% "unknown", character(1))
    )
    
    g <- igraph::graph_from_data_frame(
      edge_df %>% dplyr::transmute(from = var1, to = var2, rho, p, fdr, n),
      vertices = v_tbl, directed = FALSE
    )
    
    if (verbose) {
      message(sprintf(
        "fast_build_integrated_network(pairwise): vars=%d, edges kept=%d (min_pair_n=%d, q=%d)",
        length(all_vars), igraph::ecount(g), min_pair_n, q
      ))
    }
    list(graph = g, edges = edge_df, vertices = v_tbl, layers = layers)
  }
  
  # Reconcile candidate triplets against available columns
  # - trips: data.frame/tibble with columns X, M, Y (character)
  # - df_cols: character vector of column names present in your data frame (e.g., colnames(prep_all$merged))
  # - y_whitelist: optional character vector of allowed Y variable names (e.g., gases), NULL = no restriction
  # - verbose: print a short summary of filtering
  reconcile_triplets <- function(trips,
                                 df_cols,
                                 y_whitelist = NULL,
                                 verbose = TRUE) {
    # basic checks
    stopifnot(is.data.frame(trips))
    needed <- c("X","M","Y")
    if (!all(needed %in% names(trips))) {
      stop("reconcile_triplets(): trips must have columns X, M, Y.")
    }
    
    # coerce to character, trim whitespace
    t0 <- trips %>%
      dplyr::transmute(
        X = trimws(as.character(.data$X)),
        M = trimws(as.character(.data$M)),
        Y = trimws(as.character(.data$Y))
      )
    
    log_tbl <- tibble::tibble(stage = character(), n = integer())
    
    add_log <- function(stage, n) {
      log_tbl <<- dplyr::bind_rows(log_tbl, tibble::tibble(stage = stage, n = n))
    }
    
    add_log("input", nrow(t0))
    
    # drop empty/NA role names
    t1 <- t0 %>% dplyr::filter(!is.na(X), !is.na(M), !is.na(Y),
                               X != "", M != "", Y != "")
    add_log("drop_empty_or_na_roles", nrow(t0) - nrow(t1))
    
    # optional: restrict Y to a whitelist (e.g., gases)
    if (!is.null(y_whitelist)) {
      t2 <- t1 %>% dplyr::filter(Y %in% y_whitelist)
      add_log("drop_Y_not_in_whitelist", nrow(t1) - nrow(t2))
    } else {
      t2 <- t1
    }
    
    # keep only triplets whose columns exist in df_cols
    exists_mask <- mapply(function(x,m,y) all(c(x,m,y) %in% df_cols), t2$X, t2$M, t2$Y)
    t3 <- t2[exists_mask, , drop = FALSE]
    add_log("drop_vars_missing_from_df", nrow(t2) - nrow(t3))
    
    # drop self/duplicate-role triplets (X==M or X==Y or M==Y)
    t4 <- t3 %>% dplyr::filter(X != M, X != Y, M != Y)
    add_log("drop_roles_not_distinct", nrow(t3) - nrow(t4))
    
    # distinct triplets
    t5 <- t4 %>% dplyr::distinct()
    add_log("drop_duplicates", nrow(t4) - nrow(t5))
    
    # attach log
    attr(t5, "reconcile_log") <- log_tbl
    if (verbose) {
      msg <- paste0(
        "reconcile_triplets(): kept ", nrow(t5), " of ", nrow(t0), " triplets.\n",
        paste(sprintf(" - %-26s: %d", log_tbl$stage, log_tbl$n), collapse = "\n")
      )
      message(msg)
    }
    t5
  }
  
  
  
  # --- Split soil_vars into thematic sublayers ---
  split_soil_layers <- function(prep_all, Biomass = c("mbc","mbn"),
                                nutrients = c("nh4","no3","olsen_p","x_k","x_na","x_mg","x_ca","x_k","x_k"),
                                Disolved_C_N = c("toc","fum_toc","tn","fum_tn", "poxc"),
                                CUE = c("cue"),
                                physical  = c("p_h","loi","whc","mwd","proportion_mass","infiltration","dry"),
                                MAOM = c("fPOM_TC","fPOM_13C.","oPOM_TC","oPOM_13C.","MAOM_TC","MAOM_13C.","fPOM_TN","fPOM_15N.","oPOM_TN","oPOM_15N.","MAOM_TN","MAOM_15N.")) {
    
    # lowercase everything for matching
    all_lower <- tolower(names(prep_all$merged))
    soil_lower <- tolower(prep_all$layers$soil_vars %||% character(0))
    
    pick <- function(keywords) {
      matches <- soil_lower[grepl(paste0("\\b(", paste(keywords, collapse="|"), ")\\b"), soil_lower)]
      prep_all$layers$soil_vars[soil_lower %in% matches]
    }
    
    prep_all$layers$soil_Biomass    <- pick(Biomass)
    prep_all$layers$soil_nutrients <- pick(nutrients)
    prep_all$layers$soil_Disolved_C_N <- pick(Disolved_C_N)
    prep_all$layers$soil_CUE <- pick(CUE)
    prep_all$layers$soil_physical  <- pick(physical)
    prep_all$layers$soil_MAOM <- pick(MAOM)
    
    # Remove the old single soil_vars layer
    prep_all$layers$soil_vars <- NULL
    
    # Drop any empty lists
    prep_all$layers <- prep_all$layers[lengths(prep_all$layers) > 0]
    
    prep_all
  }
  
 
 
  
  
  
  
  
  
  
  
  
  # =========================
  # Example usage
  # =========================
  
  # (Optional) If you need to merge extra metadata into sample_data_4:
  # sample_data_4 <- sample_data_4 %>% left_join(sample_data_11, by = "ID")
  
  # 1) You already ran your gene/metabolite functions; here they are named:
  # gg_gene  <- plot_gene(sample_data_5, sample_data_4, sample_data_6)
  # gg_metab <- plot_metab(sample_data_7, sample_data_4, sample_data_6)
  # Make sure both return $ko_norm_wis
  sample_data_T <- sample_data_4 %>%
    filter(Addition =="16O") %>%
    select(-c(Sample.ID, Addition, Replicate, Notes,
              Incubation, Extraction.number, CUEFilt, Run.ID))
    
  
  #sample_data_T <- ensure_id_column(sample_data_T)
  
  # 1) Rebuild prep with caps to curb tests (tweak as you like)
  prep_all <- prepare_layers_from_cam_all(
    soil_meta = sample_data_T,
    gene_res  = gg_gene,
    metab_res = gg_metab,
    gas_guess    = c("co2","ch4","n2o"),  # include your real names
    enzyme_guess = c("bg","cbh","lap","nag","phos","fda"),
    covars = NULL,
    n_gene = 1000, n_metab = 1500, n_soil = 22
  )
  
  prep_all <- split_soil_layers(prep_all)
  names(prep_all$layers)
  
  
  # 2) Limit pairwise edges to the key biological routes
  edge_specs_fast <- list(
    between = list(
      # original routes
      c("genes","metabolites"),      # main heavy hitter; keep it if you care about gene↔metabolite links
      c("metabolites","gases"),
      c("enzymes","gases"),
      
      # soil sublayers → gases
      c("soil_physical","gases"),
      c("soil_nutrients","gases"),
      c("soil_Disolved_C_N","gases"),
      c("soil_Biomass","gases"),
      c("soil_CUE","gases"),
      
      # soil-process chain (adjacent links)
      c("soil_physical","soil_nutrients"),
      c("soil_nutrients","soil_Disolved_C_N"),
      c("soil_Disolved_C_N","soil_Biomass"),
      c("soil_Biomass","soil_CUE")
    ),
    within = character(0)             # skip all within-layer edges
  )
  
  
  #prep_all$merged <- prep_all$merged %>%
   # select(-c("replicate", "cue_filt", "notes", "addition", "run_id", "sample_id"))
  
  # 3) Build the network with the FAST function
  net_all <- fast_build_integrated_network_pairwise(
    df        = prep_all$merged,
    layers    = prep_all$layers,
    edge_specs= edge_specs_fast,
    covars    = prep_all$covars,   # ok if NULL
    fdr_thresh= 0.10,
    abs_rho_min = 0.25,
    do_within = FALSE,
    min_pair_n = 3,
    verbose   = TRUE
  )
  
  p_all <- plot_network_cam(net_all, label_mode = "hubs", top_n = 40, font_family = "sans")
  print(p_all)
  
  
  
  # 4) Candidate triplets (any X/M layer; Y restricted to gases)
  trips_all <- candidate_triplets_from_network(
    net_all,
    role_layers = list(X = NULL, M = NULL, Y = c("gases"))
  )
  
  df_cols <- colnames(prep_all$merged)
  trips_all_ok <- reconcile_triplets(trips_all, df_cols)
  #trips_all_ok <- reconcile_triplets(trips_all, colnames(prep_all$merged))
  med_res_all <- run_mediation_screen(
    df     = prep_all$merged,
    trips  = trips_all_ok,
    covars = prep_all$covars,
    boot   = 2000,
    min_n  = 2,
    impute = "median_xm"
  )
  
  hits_all <- med_res_all %>%
    dplyr::filter(!is.na(acme_p), n >= 12, acme_fdr < 0.1, acme * tot > 0) %>%
    dplyr::arrange(acme_fdr)
  
  knitr::kable(head(hits_all, 30), digits = 3)
  
  list(
    
  )
  
}

