library(tidyverse) #for tidy processing and plotting
library(vegan) # for PERMANOVA analysis
library(ggbiplot) #for PCA biplots
library(reshape2)
library(pracma)
library(janitor)
library(ggpubr)
library(cowplot)
library(nlme)
library(knitr)
library(agricolae)
library(pmartR)
library(trelliscopejs)
library(ropls)
# to install {ggbiplot}:
# library(devtools)
# install_github("vqv/ggbiplot")
#install_github("haozhu233/kableExtra")
#webshot::install_phantomjs()
#if (!require("BiocManager", quietly = TRUE))
#install.packages("BiocManager")
#BiocManager::install("ropls")
# custom ggplot theme

cbPalette <- c("#888888","#FF1493","#00FFFF" , "#117733", "#332288", "#AA4499", 
                        "#44AA99", "#882255", "#661100", "#6699CC","#DDCC77")
cbPalette2 <- c("#FF1493","#00FFFF", "#117733", "#332288", "#AA4499", 
                         "#44AA99", "#882255", "#661100", "#6699CC", "#DDCC77", "#888888")

Scale_inc= scale_color_manual(values=cbPalette2,limits=c("Pre","2","4","6","8","10"))

theme_CKM <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'lines'),
          legend.background = element_rect(colour = NA),
          panel.border = element_rect(color="black",size=2, fill = NA),
          
          plot.title = element_text(hjust = 0, size = 14),
          axis.text = element_text(size = 14, color = "black"),
          axis.title = element_text(size = 14, face = "bold", color = "black"),
          
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour=NA, fill=NA), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=12, face="bold"), #facet labels
          strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
    )
}
theme_CKM2 <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.text = element_text(size = 12),
          legend.key.size = unit(1.5, 'lines'),
          legend.background = element_rect(colour = NA),
          panel.border = element_rect(color="black",size=2, fill = NA),
          
          plot.title = element_text(hjust = 0, size = 14),
          axis.text = element_text(size = 12, color = "black"),
          axis.title = element_text(size = 14, face = "bold", color = "black"),
          
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour=NA, fill=NA), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=12, face="bold"), #facet labels
          strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
    )
}
theme_CKM3 <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.text = element_text(size = 16),
          legend.title = element_text(size = 18),
          legend.key.size = unit(1.5, 'lines'),
          legend.background = element_rect(colour = NA),
          panel.border = element_rect(color="black",size=2, fill = NA),
          
          plot.title = element_text(hjust = 0, size = 14),
          axis.text = element_text(size = 14, color = "black"),
          axis.title = element_text(size = 16, face = "bold", color = "black"),
          
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour=NA, fill=NA), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=12, face="bold"), #facet labels
          strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
    )
}
theme_CKM4 <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.text = element_text(size = 22),
          legend.title = element_text(size=24),
          legend.key.size = unit(1.5, 'lines'),
          legend.background = element_rect(colour = NA),
          panel.border = element_rect(color="black",size=2, fill = NA),
          
          plot.title = element_text(hjust = 0, size = 20),
          axis.text = element_text(size = 16, color = "black"),
          axis.title = element_text(size = 18, face = "bold", color = "black"),
          
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour=NA, fill=NA), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=12, face="bold"), #facet labels
          strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
    )
}
theme_CKM5 <- function() {  # this for all the elements common across plots
  theme_bw() %+replace%
    theme(legend.text = element_text(size = 10),
          legend.title = element_text(size=12),
          legend.key.size = unit(1.5, 'lines'),
          legend.background = element_rect(colour = NA),
          panel.border = element_rect(color="black",size=2, fill = NA),
          
          plot.title = element_text(hjust = 0, size = 14),
          axis.text = element_text(size = 12, color = "black"),
          axis.title = element_text(size = 14, face = "bold", color = "black"),
          
          # formatting for facets
          panel.background = element_blank(),
          strip.background = element_rect(colour=NA, fill=NA), #facet formatting
          panel.spacing.x = unit(1.5, "lines"), #facet spacing for x axis
          panel.spacing.y = unit(1.5, "lines"), #facet spacing for x axis
          strip.text.x = element_text(size=12, face="bold"), #facet labels
          strip.text.y = element_text(size=12, face="bold", angle = 270) #facet labels
    )
}

ggplotRegression <- function (fit) {
       
       require(ggplot2)
       
        ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
             geom_point() +
             stat_smooth(method = "lm", col = "#AA4499") +
             labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                                          "Intercept =",signif(fit$coef[[1]],5 ),
                                          " Slope =",signif(fit$coef[[2]], 5),
                                    " P =",signif(summary(fit)$coef[2,4], 5)))
   }

compare.coeff <- function(b1,se1,b2,se2){
  return((b1-b2)/sqrt(se1^2+se2^2))
}

fit_lm = function(LASTRES){
  
  a = lm(value ~ inc, data = LASTRES)
  broom::tidy(a)  
  
}    