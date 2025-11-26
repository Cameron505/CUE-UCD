enzyme commsion
cazy 
```{r setup, include=FALSE}
library(tidyverse)
library(vegan)
library(edgeR)
library(pathview)
library(ggpubr)
```

```{r}
ko_cnt=read_delim('Data/2024_RR_ko_counts_merged.tsv', delim = '\t') %>%
  rename_with(~gsub("_gene-counts.tsv", "", .x))
basic_meta=read_delim('Data/2024_RR_basic_meta.csv', delim = ',')
soil_meta=read_delim("Data/DNA extraction sample sheet.csv", delim = ',')
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

```



### Norm and ord
```{r fig.height=7.5, fig.width=7.5}

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

decorana(ko_norm_wis) #First Axis length <3, data is considered homogonous, linear methods(pca,rda) OK

# Check how many axis  for nmds
goeveg::dimcheckMDS(ko_norm_wis, distance = "bray", k = 6, trymax = 20, autotransform = F, na.rm = TRUE )


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



```


```{r}
edge_data=column_to_rownames(ko_cnt,'KO')
edge_data=edge_data[,meta_data$Site== c('O',"C")]
edge_meta=meta_data[meta_data$Site== c('O',"C"),]
```


```{r}
#  edge_data=column_to_rownames(ko_cnt,'Gene_ko')
edge_data[is.na(edge_data)] <- 0

lib_sizes <- edge_data %>%
  summarise_if(is.numeric, sum)

lib_sizes<-unlist(lib_sizes)
#  edge_meta=meta_data # set diploid as control
#edge_anno=edge_anno[rownames(edge_data),]

edge_meta=mutate(edge_meta, group=as.factor(paste(Site, Aggregate, sep = '.')))
edge_meta$Aggregate<- factor(edge_meta$Aggregate, levels= c(  "Bulk","A", "B" ,"C","D"))
#design=model.matrix(~Site+Aggregate, data=edge_meta) # look at land effect hile controlling for replicate
design=model.matrix(~0+group, data= edge_meta)
colnames(design)=levels(edge_meta$group)
dge=DGEList(edge_data, group = edge_meta$group, lib.size = lib_sizes)#, genes = edge_anno)
#run edger
keep <- filterByExpr(dge, design, min.total.count=100, min.count=5)
dge <- dge[keep, , keep.lib.sizes=FALSE]

dge <- calcNormFactors(dge, method="RLE")
dge <- estimateGLMCommonDisp(dge, design)
dge <- estimateGLMTagwiseDisp(dge, design)
dge = estimateDisp(dge,design)

edgeR::plotBCV(dge)
#cons=list(Site=c(0,1,0,0,0,0), AvBulk=c(0,0,1,0,0,0),BvBulk=c(0,0,0,1,0,0),CvBulk=c(0,0,0,0,1,0),DvBulk=c(0,0,0,0,0,1)) # Define contrasts
cons=makeContrasts(A=C.A-O.A, B=C.B-O.B, C= C.C-O.C, D= C.D-O.D, Bulk=C.Bulk-O.Bulk,CvO= (C.Bulk+C.A+C.B+C.C+C.D)-(O.Bulk+O.A+O.B+O.C+O.D),levels=design)
fits=list()
edger_res=data.frame(ID=character())#edge_anno
for (cs in 1:ncol(cons)){
  lfit <- glmFit(dge, design) # fit
  lrt <- glmLRT(lfit, contrast = cons[,cs]) # test
  #lfit=glmQLFit(dge,design, robust = T)
  #lrt=glmQLFTest(lfit,contrast = cons[[cs]])
  fits[[colnames(cons)[cs]]]=lrt # save test
  lrt$table$FDR=p.adjust(lrt$table$PValue, method = 'BH')
  colnames(lrt$table)=paste(colnames(cons)[cs], colnames(lrt$table), sep = "_") # append test sufix
  edger_res=full_join(edger_res, rownames_to_column(lrt$table, 'ID')) # join
}

edger_res=full_join(edger_res, ko_anno, by=c('ID'='gene'))

#edger_sig=filter(edger_res, (abs(OvC_logFC) > 1 & OvC_FDR <= 0.05 & OvC_logCPM > 0) | (abs(HvC_logFC) > 1  & HvC_FDR <= 0.05 & HvC_logCPM > 0))
#write.csv(edger_sig, 'data/edger_results.csv')


```

## Kegg vol-ma
```{r fig.height=4, fig.width=6}
plot_data=edger_res
sig_limit=0.05
fc_limit=1
cpm_limit=0
Title=', KO terms'
plots=list()
for (con in colnames(cons)){
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

ggpubr::ggarrange(plotlist = plots, common.legend = T) #%>%
# ggexport(filename = "Graphs/ko_vol-ma.png", width = 600, height = 400)


```

## Kegg cluster
```{r}
library(clusterProfiler)
library(enrichplot)
#dges_sig=filter(edger_res, HvC_FDR <= 0.05 & abs(HvC_logFC)> 0.5 & HvC_logCPM > 0)
#dges=dges_sig$HvC_logFC
gene_anno=list()
for (con in colnames(cons)){
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
    
    plots[[i]][[j]][['dot']]=dotplot(cur, showCategory=show_Category) + ggtitle(name) + scale_fill_gradient( low = "blue",  high = "red", space = "Lab",  na.value = "grey50", guide = "colourbar", aesthetics = "color")
    plots[[i]][[j]][['cnet']]=cnetplot(cur, categorySize="pvalue", showCategory=show_Category, foldChange=fc)+ ggtitle(name)+ scale_fill_gradient2( low = "blue", mid = "white",  high = "red", midpoint = 0, space = "Lab",  na.value = "grey50", guide = "colourbar", aesthetics = "fill")
    plots[[i]][[j]][['heat']]=heatplot(cur, foldChange=fc, showCategory = show_Category)+ ggtitle(name)+ scale_fill_gradient2( low = "blue", mid = "white",  high = "red", midpoint = 0, space = "Lab",  na.value = "grey50", guide = "colourbar", aesthetics = "fill")
    #plots[[i]][[j]][['emap']]=emapplot(cur,showCategory=show_Category, color='pvalue')+ ggtitle(name)+ scale_fill_gradient( low = "blue",  high = "red",  space = "Lab",  na.value = "grey50", guide = "colourbar", aesthetics = "color")
    if (class(cur) == 'enrichResult'){
      plots[[i]][[j]][['upset']]=upsetplot(cur,n=show_Category)
      plots[[i]][[j]][['bar']]=barplot(cur, showCategory=show_Category)
    }
    if (class(cur)=='gseaResult') {
      plots[[i]][[j]][['ridge']]=ridgeplot(cur, showCategory = show_Category, fill = 'pvalue' )
      plots[[i]][[j]][['cnet']] + scale_fill_gradient2( low = "blue", mid = "white",  high = "red", midpoint = 0, space = "Lab",  na.value = "grey50", guide = "colourbar", aesthetics = "color")
    }
  }
}

write_rds(plots, 'data/results/plots/kegg_gsea-geo.RDS')


```


##Plotting KEGG pathways
```{r fig.height=36, fig.width=12}
# go_sig=edger_res_go%>%
# filter(abs(PvPF_logFC) > 2.5 & PvPF_FDR < 0.01 & PvPF_logCPM > 1
# )%>%
# inner_join(.,go_anno, by=c('ID'='GOID'))


setwd("C://Users//cmcm//OneDrive - University of California, Davis//Documents//Github//CUE-UCD//Graphs//KEGG//CvO")
ko_sig=edger_res%>%
  mutate(ID=str_c('ko:', ID))%>%
  filter(CvO_FDR < 0.05 & abs(CvO_logFC) >.5)%>%
  left_join(.,ko_anno, by=c('ID'='gene'))

pv_data=as.numeric(ko_sig$CvO_logFC)
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

kegg_maps='/Graphs/KEGG/CvO'
for (pw in 1:nrow(pathways)){
  # define limit
  #lim_filt=mutate(paths_df, Path=str_sub(Path,start = 6), Gene=str_sub(Gene,start = 4))%>%
   # dplyr::filter(Path==pathways[[pw,1]])
  #if (length(lim_filt$Gene) <= 5) next
  lim=floor(max(abs(pv_data[names(pv_data) %in% lim_filt$Gene])))+1
  
  pv.out = pathview(gene.data = pv_data, pathway.id = pathways[pw,1], species = "ko", out.suffix =  paste0(str_remove_all(pathways[pw,2],'/'),"CvO.ko.data"), kegg.native = T, same.layer = F,
                    low = list(gene = "red"), mid = list(gene = "gray"), high = list(gene = "green"),
                    bins = list(gene=25),
                    kegg.dir = "C://Users//cmcm//OneDrive - University of California, Davis//Documents//Github//CUE-UCD//Graphs//KEGG//CvO")
}

setwd("C://Users//cmcm//OneDrive - University of California, Davis//Documents//Github//CUE-UCD")
```

#PDF print?
# Save all plots into one multi-page PDF
pdf("enrichment_plots.pdf", width = 11, height = 8)

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

##Heatmap

library(ggdendro)
sig_data=edger_res%>%
  filter(if_any(ends_with("FDR"), ~ .x<=0.05)) #%>%
  #filter(if_any(ends_with("logFC"), ~ .x>=2))

plot_data=data.frame(Name=character(), test=character(), FDR=double(), logCPM=double(), FDR=double())
for (con in colnames(cons)){
  FDR=as.name(paste(con, 'FDR', sep = '_'))
  logFC=as.name(paste(con, 'logFC', sep = '_'))
  logCPM=as.name(paste(con, 'logCPM', sep = '_'))
  
  plot_data=rbind(plot_data, transmute(sig_data, ID = ID, Name=Name, test=con, logFC=!!logFC, logCPM=!!logCPM, FDR=!!FDR))
}

dendo=column_to_rownames(sig_data, 'ID')%>%select(ends_with('logFC'))%>%vegan::vegdist(., 'euclidean')%>%hclust(.)%>%as.dendrogram(.)
dendo_plot=dendo%>%ggdendrogram(., rotate = T) + theme(axis.text.y = element_text(size = 6))
plot_data$ID <- factor(x = plot_data$ID, levels = plot_data$ID[order.dendrogram(dendo)], ordered = TRUE)


heat= ggplot(plot_data, aes(y=ID, x=factor(test, levels=colnames(cons)), fill=logFC))+
  #scale_y_discrete(labels=plot_data$Name)+
  geom_tile()+
  scale_fill_gradient2()

heat

ggarrange(heat, dendo_plot, ncol = 2, align = 'hv', common.legend = T, label.y = c(1,0))