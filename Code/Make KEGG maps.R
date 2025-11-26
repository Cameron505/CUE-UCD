
##Run load everything so that these objects can be pulled out
edger_res<-gg_gene$edger_res
ko_anno<-gg_gene$ko_anno
paths_df<-gg_gene$paths_df
paths_names<-gg_gene$paths_names


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

kegg_maps='/Graphs/KEGG/C'
withr::with_dir("C://Users//cmcm//OneDrive - University of California, Davis//Documents//Github//CUE-UCD//Graphs//KEGG//C",
for (pw in 1:nrow(pathways)){
  lim_filt=mutate(paths_df, Path=str_sub(Path,start = 6), Gene=str_sub(Gene,start = 4))%>%
    dplyr::filter(Path==pathways[[pw,1]])
  lim=floor(max(abs(pv_data[names(pv_data) %in% lim_filt$Gene])))+1
  
  pv.out = pathview(gene.data = pv_data, pathway.id = pathways[pw,1], species = "ko", out.suffix =  paste0(str_remove_all(pathways[pw,2],'/'),"C.ko.data"), kegg.native = T, same.layer = F,
                    low = list(gene = "red"), mid = list(gene = "gray"), high = list(gene = "green"),
                    bins = list(gene=25),
                    kegg.dir = "C://Users//cmcm//OneDrive - University of California, Davis//Documents//Github//CUE-UCD//Graphs//KEGG//C")
}
)





##Plotting KEGG pathways CvO




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

kegg_maps='/Graphs/KEGG/Site'
withr::with_dir("C://Users//cmcm//OneDrive - University of California, Davis//Documents//Github//CUE-UCD//Graphs//KEGG//Site",
for (pw in 1:nrow(pathways)){
  lim_filt=mutate(paths_df, Path=str_sub(Path,start = 6), Gene=str_sub(Gene,start = 4))%>%
    dplyr::filter(Path==pathways[[pw,1]])
  lim=floor(max(abs(pv_data[names(pv_data) %in% lim_filt$Gene])))+1
  
  pv.out = pathview(gene.data = pv_data, pathway.id = pathways[pw,1], species = "ko", out.suffix =  paste0(str_remove_all(pathways[pw,2],'/'),"Site.ko.data"), kegg.native = T, same.layer = F,
                    low = list(gene = "red"), mid = list(gene = "gray"), high = list(gene = "green"),
                    bins = list(gene=25),
                    kegg.dir = "C://Users//cmcm//OneDrive - University of California, Davis//Documents//Github//CUE-UCD//Graphs//KEGG//Site")
}
)




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

kegg_maps='/Graphs/KEGG/D'
withr::with_dir("C://Users//cmcm//OneDrive - University of California, Davis//Documents//Github//CUE-UCD//Graphs//KEGG//D",
for (pw in 1:nrow(pathways)){
  lim_filt=mutate(paths_df, Path=str_sub(Path,start = 6), Gene=str_sub(Gene,start = 4))%>%
    dplyr::filter(Path==pathways[[pw,1]])
  lim=floor(max(abs(pv_data[names(pv_data) %in% lim_filt$Gene])))+1
  
  pv.out = pathview(gene.data = pv_data, pathway.id = pathways[pw,1], species = "ko", out.suffix =  paste0(str_remove_all(pathways[pw,2],'/'),"D.ko.data"), kegg.native = T, same.layer = F,
                    low = list(gene = "red"), mid = list(gene = "gray"), high = list(gene = "green"),
                    bins = list(gene=25),
                    kegg.dir = "C://Users//cmcm//OneDrive - University of California, Davis//Documents//Github//CUE-UCD//Graphs//KEGG//D")
}
)

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

kegg_maps='/Graphs/KEGG/A'

withr::with_dir("C://Users//cmcm//OneDrive - University of California, Davis//Documents//Github//CUE-UCD//Graphs//KEGG//A",
                for (pw in 1:nrow(pathways)){
                  # define limit
                  lim_filt=mutate(paths_df, Path=str_sub(Path,start = 6), Gene=str_sub(Gene,start = 4))%>%
                    dplyr::filter(Path==pathways[[pw,1]])
                  #if (length(lim_filt$Gene) <= 5) next
                  lim=floor(max(abs(pv_data[names(pv_data) %in% lim_filt$Gene])))+1
                  
                  pv.out = pathview(gene.data = pv_data, pathway.id = pathways[pw,1], species = "ko", out.suffix =  paste0(str_remove_all(pathways[pw,2],'/'),"A.ko.data"), kegg.native = T, same.layer = F,
                                    low = list(gene = "red"), mid = list(gene = "gray"), high = list(gene = "green"),
                                    bins = list(gene=25),
                                    kegg.dir = "C://Users//cmcm//OneDrive - University of California, Davis//Documents//Github//CUE-UCD//Graphs//KEGG//A")
                }
)

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

kegg_maps='/Graphs/KEGG/B'
withr::with_dir("C://Users//cmcm//OneDrive - University of California, Davis//Documents//Github//CUE-UCD//Graphs//KEGG//B",
                for (pw in 1:nrow(pathways)){
                  lim_filt=mutate(paths_df, Path=str_sub(Path,start = 6), Gene=str_sub(Gene,start = 4))%>%
                    dplyr::filter(Path==pathways[[pw,1]])
                  lim=floor(max(abs(pv_data[names(pv_data) %in% lim_filt$Gene])))+1
                  
                  pv.out = pathview(gene.data = pv_data, pathway.id = pathways[pw,1], species = "ko", out.suffix =  paste0(str_remove_all(pathways[pw,2],'/'),"B.ko.data"), kegg.native = T, same.layer = F,
                                    low = list(gene = "red"), mid = list(gene = "gray"), high = list(gene = "green"),
                                    bins = list(gene=25),
                                    kegg.dir = "C://Users//cmcm//OneDrive - University of California, Davis//Documents//Github//CUE-UCD//Graphs//KEGG//B")
                }
)

