plot_respiration = function(respiration_processed){
  
inc.lab<-c("2 °C","4 °C","6 °C","8 °C","10 °C")
names(inc.lab) <- c("2","4","6","8","10")

  gg_res =
    respiration_processed %>%
    mutate(Inc_temp = factor(Inc_temp, levels=c("2","4","6","8","10")),
           pre_inc = factor(pre_inc,levels=c("-2","-6"))) %>%
    ggplot(aes(x = JD2, y = Res, color = pre_inc))+
    geom_point(position = position_dodge(width = 0.4),
               size = 2)+
    geom_smooth(se=F)+
    #stat_smooth(method= "lm")+
    #stat_cor(label.y=c(90,100), size=2)+
    #stat_regline_equation(label.y=c(110,120), size=2)+
    #geom_text(data = res_lm , aes(y = 300, label = p.value))+
    scale_y_continuous(expand=c(0,0),oob=rescale_none)+
    ylab(expression(paste( "Respiration (",mu,"g-C",day^-1, ")")))+
    facet_wrap(~Inc_temp,labeller = labeller(Inc_temp =inc.lab ), nrow=1)+
    theme_light()+
    scale_colour_manual(values=cbPalette2, breaks=c("-2","-6"), labels=c("Mild frozen", "Moderate frozen"))+
    scale_fill_manual(values=cbPalette2)+
    ylab(expression(paste( "Respiration (",mu,"g-C ",hour^-1, ")")))+
    xlab("Incubation day")+
    labs(color='Pre-incubation') +
    ggtitle("Soil respiration")+
    theme_CKM()
  
  gg_cumres =
    respiration_processed %>%
    mutate(Inc_temp = factor(Inc_temp, levels=c("2","4","6","8","10")),
           pre_inc = factor(pre_inc,levels=c("-2","-6"))) %>%
    ggplot(aes(x = JD2, y = val, color =  pre_inc))+
    geom_point(position = position_dodge(width = 0.4),
               size = 2)+
    stat_smooth(method= "lm")+
    stat_cor(label.y=c(285,305), size=2)+
    stat_regline_equation(label.y=c(245,265), size=2)+
    ylab(expression(paste( "Respiration (",mu,"g-C)")))+
    facet_wrap(~Inc_temp,labeller = labeller(Inc_temp =inc.lab ))+
    theme_light()+
    scale_colour_manual(values=cbPalette2, breaks=c("-2","-6"), labels=c("Mild frozen", "Moderate frozen"))+
    scale_fill_manual(values=cbPalette2)+
    ylab(expression(paste( "Respiration (",mu,"g-C)")))+
    xlab("Incubation day")+
    labs(color='Pre-incubation') +
    ggtitle("Cumulative soil respiration")+
    theme_CKM()
  
  gg_Avgres =
    respiration_processed %>%
    mutate(Inc_temp = factor(Inc_temp, levels=c("2","4","6","8","10")),
           pre_inc = factor(pre_inc,levels=c("-2","-6"))) %>%
    ggplot(aes(x=JD2, y=Res, color=pre_inc))+
    stat_summary(fun = mean,geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    stat_smooth(method= "lm")+
    stat_cor(label.y=c(90,100), size=2)+
    stat_regline_equation(label.y=c(110,120), size=2)+
    facet_wrap(~Inc_temp,labeller = labeller(Inc_temp =inc.lab ))+
    theme_light()+
    scale_colour_manual(values=cbPalette2, breaks=c("-2","-6"), labels=c("Mild frozen", "Moderate frozen"))+
    scale_fill_manual(values=cbPalette2)+
    ylab(expression(paste( "Respiration (",mu,"g-C ",hour^-1, ")")))+
    xlab("Incubation day")+
    labs(color='Pre-incubation') +
    ggtitle("Average soil respiration")+
    theme_CKM()
  
  gg_Avgcumres =
    respiration_processed %>%
    mutate(Inc_temp = factor(Inc_temp, levels=c("2","4","6","8","10")),
           pre_inc = factor(pre_inc,levels=c("-2","-6"))) %>%
  ggplot(aes(x=JD2, y=val, color=pre_inc))+
    stat_summary(fun = mean,geom = "point",size = 2) +
    stat_summary(fun.data = mean_se, geom = "errorbar")+
    stat_smooth(method= "lm")+
    stat_cor(label.y=c(285,305), size=2)+
    stat_regline_equation(label.y=c(245,265), size=2)+
    facet_wrap(~Inc_temp,labeller = labeller(Inc_temp =inc.lab ))+
    theme_light()+
    scale_colour_manual(values=cbPalette2, breaks=c("-2","-6"), labels=c("Mild frozen", "Moderate frozen"))+
    scale_fill_manual(values=cbPalette2)+
    ylab(expression(paste( "Respiration (",mu,"g-C)")))+
    xlab("Incubation day")+
    labs(color='Pre-incubation') +
    ggtitle("Average cumulative soil respiration")+
    theme_CKM()
  
  
  
  LASTRES<- respiration_processed %>%
    filter(JD2==5)
  fit_aov = function(LASTRES){
    
    a = aov(val ~ pre_inc, data = LASTRES)
    broom::tidy(a) %>% 
      filter(term == "pre_inc") %>% 
      dplyr::select(`p.value`) %>% 
      mutate(asterisk = case_when(`p.value` <= 0.05 ~ "*"))
    
  }  
  
  rescum_aov = 
    LASTRES %>% 
    group_by(Inc_temp) %>% 
    filter(pre_inc!="none")%>%
    do(fit_aov(.)) %>% 
    mutate(pre_inc = "-2") %>% 
    # factor the Inc_temp so they can line up in the graph
    mutate(Inc_temp = factor(Inc_temp, levels=c("2","4","6","8","10")))
  
  
  gg_CumresLastday =
    LASTRES %>%
    mutate(Inc_temp = factor(Inc_temp, levels=c("T0","Pre","-2","-6","2","4","6","8","10")),
           pre_inc = factor(pre_inc,levels=c("T0","-2","-6"))) %>%
    ggplot(aes(x = Inc_temp, y = val, color = pre_inc, group = pre_inc, fill = pre_inc))+
    geom_boxplot(show.legend = F, 
                 outlier.colour = NULL,
                 outlier.fill = NULL,
                 position = position_dodge(width = 0.6), 
                 alpha = 0.2,
                 width=0.5,
                 aes(group = interaction(Inc_temp, pre_inc)))+
    geom_point(position = position_dodge(width = 0.6), size = 3)+
    guides(color=guide_legend(title="Pre-Incubation "),fill="none")+
    scale_y_continuous(expand=c(0,0),limits=c(50,375),oob=rescale_none)+
    geom_text(data = rescum_aov, aes(y = 350, label = asterisk), size=6, color="black")+
    theme_light()+
    scale_colour_manual(values=cbPalette2, breaks=c("-2","-6"), labels=c("Mild frozen", "Moderate frozen"))+
    scale_fill_manual(values=cbPalette2,labels=c('-2 °C', '-6 °C'))+
    ylab(expression(paste( "Total respired C (",mu,"g-C)")))+
    xlab("Incubation temp. (°C)")+
    labs(color='Pre-incubation') +
    ggtitle("Cumulative respiration")+
    theme_CKM()
  
  respiration_legend = get_legend(gg_CumresLastday+ guides(color = guide_legend(nrow = 1)) +
                                 theme(legend.position = "bottom"))
S<-ggplot() + theme_void()
SS<-plot_grid(gg_CumresLastday,S,
          nrow=1)
  gg_Ncombine= plot_grid(
    gg_res + theme(legend.position="none"),
    gg_CumresLastday,
    align="none",
    rel_widths= c(2,1),
    labels = c("A", "B"),
    #label_x= 0.1,
    hjust = -1,
    vjust= 1,
    ncol = 1
  )
  
  gg_N_Legend=gg_Ncombine
  
  
  list(#"Respiration" = gg_res,
        gg_N_Legend=gg_N_Legend,
       "Average Respiration" = gg_Avgres,
       "Cumulative Respiration" = gg_cumres,
       "Average Cumulative Respiration" = gg_Avgcumres
       )
  
}

