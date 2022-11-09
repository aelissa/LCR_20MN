library(sf)
library(ggplot2)
library(tidyverse)
library(spdep)
library(colorspace)
library(ggrepel)
library(gridExtra)
library(ggmap)

model_results<-st_read("var_norm_modelled.gpkg")

mvars_nb <- poly2nb(as_Spatial(model_results))  #queen contiguity
coords<-coordinates(as_Spatial(model_results))
vars_w<-nb2listw(mvars_nb)
vars_w

###check for global spatial autocorrelation
moran.test(as_Spatial(model_results)$gwr_std_resid,listw=vars_w)
moran.mc(as_Spatial(models_results)$gwr_std_resid,listw=vars_w,nsim = 5000) 

moran.test(as_Spatial(models_results)$mgwr_std_resid,listw=vars_w)
moran.mc(as_Spatial(models_results)$mgwr_std_resid,listw=vars_w,nsim = 5000) 


#######visualize variables and coefficients
borders<-st_read("./data/LCR_LA.gpkg")


#filter the data to correct for multiple hypothesis testing
model_results_filtered<-model_results %>%
  mutate(
    gwr_intercept=ifelse(gwr_t_intercept==0,NA,gwr_intercept),
    gwr_estPop2019=ifelse(gwr_t_estPop2019==0,NA,gwr_estPop2019),
    gwr_blackPct=ifelse(gwr_t_blackPct==0,NA,gwr_blackPct),
    gwr_mltEthnPct=ifelse(gwr_t_mltEthnPct==0,NA,gwr_mltEthnPct),
    gwr_otherEthn=ifelse(gwr_t_otherEthn==0,NA,gwr_otherEthn),
    gwr_carOwn=ifelse(gwr_t_carOwn==0,NA,gwr_carOwn),
    gwr_IMDeduS=ifelse(gwr_t_IMDeduS==0,NA,gwr_IMDeduS),
    gwr_housePrice5yrsAvg=ifelse(gwr_t_housePrice5yrsAvg==0,NA,gwr_housePrice5yrsAvg),
    gwr_IMDtraffic=ifelse(gwr_t_IMDtraffic==0,NA,gwr_IMDtraffic),
    gwr_IMDair=ifelse(gwr_t_IMDair==0,NA,gwr_IMDair)
    )
  

model_results_filtered<-model_results_filtered %>%
  mutate(
    mgwr_intercept=ifelse(mgwr_t_intercept==0,NA,mgwr_intercept),
    mgwr_estPop2019=ifelse(mgwr_t_estPop2019==0,NA,mgwr_estPop2019),
    mgwr_blackPct=ifelse(mgwr_t_blackPct==0,NA,mgwr_blackPct),
    mgwr_mltEthnPct=ifelse(mgwr_t_mltEthnPct==0,NA,mgwr_mltEthnPct),
    mgwr_otherEthn=ifelse(mgwr_t_otherEthn==0,NA,mgwr_otherEthn),
    mgwr_carOwn=ifelse(mgwr_t_carOwn==0,NA,mgwr_carOwn),
    mgwr_IMDeduS=ifelse(mgwr_t_IMDeduS==0,NA,mgwr_IMDeduS),
    mgwr_housePrice5yrsAvg=ifelse(mgwr_t_housePrice5yrsAvg==0,NA,mgwr_housePrice5yrsAvg),
    mgwr_IMDtraffic=ifelse(mgwr_t_IMDtraffic==0,NA,mgwr_IMDtraffic),
    mgwr_IMDair=ifelse(mgwr_t_IMDair==0,NA,mgwr_IMDair)
  )



vars<-sub("gwr_","",colnames(model_results)[20:28])
for(v in vars){
  compare_surfaces(model_results_filtered,v)
}

####car ownership plot


uac<-st_read("./data/UAC_LCR.gpkg") %>%
  st_drop_geometry()

CO_results<-model_results_filtered %>%
  st_drop_geometry() %>%
  inner_join(uac,by=c("OA11CD"="OA11CD"))%>%
  select(carOwn,mgwr_carOwn,RUC11) %>%
  mutate(mgwr_carOwn_sig=ifelse(is.na(mgwr_carOwn),"Not significant","Significant"),
         RUC11class=ifelse((RUC11=="Rural town and fringe" | RUC11=="Rural village and dispersed"),"Rural",RUC11)) %>%
  group_by(mgwr_carOwn_sig,RUC11class)%>%
  summarise(count=n()) 

tot_ruc<-CO_results %>%
  group_by(RUC11class)%>%
  summarise(tot_ruc=sum(count))

CO_results<-CO_results %>%
  inner_join(tot_ruc,by=c("RUC11class"="RUC11class"))%>%
  mutate(
    pctRUC=(count/tot_ruc))
    #pctSIG=(count/tot_sig),
    #rate=(count/tot_sig)/(count/tot_ruc))

p1<-ggplot(CO_results,aes(x=pctRUC*100,y=RUC11class,fill=mgwr_carOwn_sig))+
  geom_bar(stat = "identity", position = "dodge",width = 0.6)+
  scale_fill_viridis_d()+
  labs(x="obs. (%)")+
  theme_minimal()+
  theme(axis.title.y = element_blank(),legend.position = "top",legend.title = element_blank())

ggplot(models_results_filtered_95[!is.na(models_results_95$mgwr_carOwn),],aes(x=mgwr_carOwn))+
  geom_density(color="yellow",fill="yellow")+theme_minimal()+labs(x="carOwn coefficient, significant at 95%")+
  geom_vline(xintercept=0.0)

m1<-ggplot()+
  geom_sf(aes(fill=mgwr_carOwn),model_results_filtered,show.legend = T,color="NA")+
  scale_fill_continuous_diverging(palette="Green-Brown",mid=0,na.value="grey")+
  geom_sf(data = borders, fill="NA",color="white")+
  labs(fill="MGWR")+
  theme_void()

mossley<-st_read("mossley_hill_95.gpkg") %>%
  st_transform(4326)

b <- sp::bbox(as_Spatial(mossley))
(base <- ggmap(get_map(location = b)))

m<-as_Spatial(mossley[mossley$mgwr_t_carOwn!=0,])
m<-fortify(m)
m_data<-st_drop_geometry(mossley[mossley$mgwr_t_carOwn!=0,]) %>%
         mutate(id=as.character(seq(1:nrow(mossley[mossley$mgwr_t_carOwn!=0,]))))                  


m<-left_join(m,m_data,by=c("id"="id"))

m2<-base +
  geom_polygon(data=m,aes(x=long,y=lat,group=group,fill=mgwr_carOwn), alpha=0.5)+
  scale_fill_continuous_sequential(palette="Oranges")+
  #scale_fill_continuous_diverging(palette="Green-Brown",mid=0,na.value="grey")+
  #geom_sf(data = borders, fill="NA",color="white")+
  labs(fill="MGWR")+
  theme_void()

ggpubr::ggarrange(m1,
                  ggpubr::ggarrange(m2,p1,nrow=2,labels=c("B","C")),
                  ncol=2,
                  labels = "A")


####ED plot
ED_results<-model_results_filtered %>%
  st_drop_geometry()%>%
  select(IMDeduS,mgwr_IMDeduS)%>%
  mutate(
    rel=ifelse(mgwr_IMDeduS>0,"Positive","Negative")
  )


p1<-ggplot(ED_results[!is.na(ED_results$mgwr_IMDeduS),],aes(x=rel,colour=rel,fill=rel,y=IMDeduS))+
  geom_boxplot(width=0.2)+
  scale_fill_discrete_diverging(palette="Green-Brown")+
  scale_color_discrete_diverging(palette="Green-Brown")+
  labs(y="Education Deprivation Score (standardized)")+
  stat_summary(geom = "crossbar", width=0.2, fatten=0, color="white", fun.data = function(x){ return(c(y=median(x), ymin=median(x), ymax=median(x))) })+
  theme_minimal()+
  theme(axis.title.x=element_blank(), axis.text.y = element_text(colour="grey"), axis.ticks.y= element_line(colour="grey"), legend.position="none")


m1<-ggplot()+
  geom_sf(aes(fill=mgwr_IMDeduS),model_results_filtered,show.legend = T,color="NA")+
  scale_fill_continuous_diverging(palette="Green-Brown",mid=0,na.value="grey")+
  geom_sf(data = borders, fill="NA",color="white")+
  labs(fill="MGWR")+
  theme_void()

M<-ggpubr::ggarrange(m1,p1, widths=c(2, 1),labels=c("A","B"))
ggsave("img/edu_overview.png",M,dpi = 600)

#####
######house prices####

m1<-m1<-ggplot()+
  geom_sf(aes(fill=mgwr_housePrice5yrsAvg),model_results_filtered[model_results_filtered$housePrice5yrsAvg<0 | is.na(model_results_filtered$mgwr_housePrice5yrsAvg),],show.legend = T,color="NA")+
  scale_fill_continuous_diverging(palette="Green-Brown",mid=0,na.value="grey")+
  geom_sf(data = borders, fill="NA",color="grey30")+
  labs(fill="MGWR")+
  theme_void()
m2<-m1<-ggplot()+
  geom_sf(aes(fill=mgwr_housePrice5yrsAvg),model_results_filtered[model_results_filtered$housePrice5yrsAvg>0 | is.na(model_results_filtered$mgwr_housePrice5yrsAvg),],show.legend = T,color="NA")+
  scale_fill_continuous_diverging(palette="Green-Brown",mid=0,na.value="grey")+
  geom_sf(data = borders, fill="NA",color="grey30")+
  labs(fill="MGWR")+
  theme_void()

M<-ggpubr::ggarrange(m2,m1,labels=c("A","B"))
ggsave("img/hp_overview.png",M,dpi = 600)



####air dep

Ngbnd<-st_read("output/maps_output/20minNeighbnd.gpkg")

m1<-ggplot()+
  geom_sf(aes(fill=IMDair),model_results_filtered,show.legend = T,color="NA")+
  scale_fill_continuous_diverging(palette="Green-Brown",mid=0,na.value="grey")+
  geom_sf(data = borders, fill="NA",color="white")+
  geom_sf(data = Ngbnd, fill="NA",color="black",stroke=0.4)+
  labs(fill="IMDAir(Std)")+
  theme_void()

m2<-ggplot()+
  geom_sf(aes(fill=mgwr_IMDair),model_results_filtered,show.legend = T,color="NA")+
  scale_fill_continuous_diverging(palette="Green-Brown",mid=0,na.value="grey")+
  geom_sf(data = borders, fill="NA",color="white")+
  labs(fill="MGWR")+
  theme_void()

M<-ggpubr::ggarrange(m1,m2,labels=c("A","B"))
ggsave("img/air_overview.png",M,dpi = 600)
