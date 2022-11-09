library(readr)
library(tidyverse)
library(sf)
library(spdep)
library(ggrepel)
library(gridExtra)
library(ggpubr)


oa_lad<-st_read("./data/oa_lad.gpkg")
oa_vars<-read_csv("data/oa_vars_HP.csv")
access<-read_csv("./output/post_processing_output/access.csv") %>%
  rename_with(~sub("walkTime","walkTimeAverage",.),.cols = starts_with("walkTime"))

####Count the number of services types that are reacheable in a 10 minute walk

access_count<-access %>%
  tidyr::pivot_longer(starts_with("walkTime")) %>%
  mutate(ngbCondition=ifelse(value<=600,1,0))%>%
  group_by(fromPlace) %>%
  dplyr::summarise(n=sum(ngbCondition,na.rm = T))

####plot the proportion of population that can access each service in ten minute walk

plot_services<-access %>%
  select(fromPlace, starts_with("walkTime")) %>%
  inner_join(oa_vars,by=c("fromPlace"="OA11CD")) %>%
  tidyr::pivot_longer(starts_with("walkTime")) %>%
  filter(value<=600)%>%
  group_by(name)%>%
  dplyr::summarise(countPop=(sum(estPop2019, na.rm = T)/sum(oa_vars$estPop2019,na.rm = T))*100)

 
fct_reorder(plot_services$name, plot_services$countPop, .desc = T)
level<-plot_services$name
plot_services$name<-factor(plot_services$name,levels = level, labels = c("Primary Schools","Entertainment","Specialised Food Shops","Supermarket","Pharmacies","Doctors Surgeries","Places of Worship","Public Parks","Recreation","Sport","Bus stops","Train Stations"))
  
ggplot(plot_services, aes(y=fct_reorder(name,countPop), x=round(countPop,1)))+
    geom_bar(stat = "identity",width = 0.5,colour="#36454f",fill="#666699")+
    #scale_y_discrete(labels=)+
    theme_minimal()+
    theme(axis.title.y = element_blank(),text = element_text(size = 14,colour = "#36454f"))+
  labs(x="Population (%)")+
  geom_text(aes(label=round(countPop),hjust=-0.09))
    
ggsave(paste0("img/services.png"),width = 25,height = 15,units = "cm")  
                          
####Spatial clustering to identify areas that approximate a 20 minute neighbourhood

oa_vars_access<-oa_lad %>%
  inner_join(access_count[c("fromPlace","n")],by=c("OA11CD"="fromPlace"))%>%
  inner_join(oa_vars,by=c("OA11CD"="OA11CD"))

st_write(oa_vars_access,"output/maps_output/oa_vars_access.gpkg")
oa_vars_access<-as_Spatial(oa_vars_access)
vars_nb <- poly2nb(oa_vars_access)  #queen contiguity

coords<-coordinates(oa_vars_access)

vars_w<-nb2listw(vars_nb)
vars_w

###check for global spatial autocorrelation
moran.test(oa_vars_access$n,listw=vars_w)
moran.mc(oa_vars_access$n,listw=vars_w,nsim = 5000) 
####High spatial correlation (as expected)

####Local spatial autocorrelation

moran.local <-localmoran(oa_vars_access$n,listw=vars_w) 
oa_vars_access$s_avg <- scale(oa_vars_access$n) %>% as.vector()
oa_vars_access$lm_avg <- moran.local[,1]%>% as.vector()

moran.map<-oa_vars_access %>%
  st_as_sf %>%
  select(OA11CD,s_avg,lm_avg) %>%
  cbind(moran.local) %>% 
  mutate(quad_sig = case_when(s_avg > 0 & 
                             lm_avg > 0 & 
                             `Pr.z...0.` <= 0.05 ~ 
                           "High Access",
                              s_avg <= 0 & 
                              lm_avg <= 0 & 
                             `Pr.z...0.` <= 0.05 ~ 
                            "low-high", 
                              s_avg > 0 & 
                              lm_avg <= 0 & 
                             `Pr.z...0.` <= 0.05 ~ 
                            "high-low",
                              s_avg <= 0 & 
                              lm_avg > 0 & 
                             `Pr.z...0.` <= 0.05 ~
                            "Low Access", 
                             `Pr.z...0.` > 0.05 ~
                            "non-significant"))


oa_lad<- oa_lad %>% inner_join(st_drop_geometry(moran.map[c("OA11CD","quad_sig")]),by=c("OA11CD"="OA11CD")) %>%
  mutate(quad_sig=as.factor(quad_sig)) %>%
  inner_join(oa_vars[c("OA11CD","estPop2019")],by=c("OA11CD"="OA11CD")) %>%
  inner_join(access_count[c("fromPlace","n")],by=c("OA11CD"="fromPlace"))

# ###make borders for mapping LADs
# lad<-unique(oa_lad$LAD20NM)
# labels<-NULL
# borders<-NULL
# for(l in lad){
#   oa_lad_temp<-oa_lad[oa_lad$LAD20NM==l,]
#   oa_lad_temp<-oa_lad_temp %>% 
#     st_union() %>% 
#     st_sf() 
#   borders<-rbind(borders,oa_lad_temp)
#   
#   oa_lad_temp<-oa_lad_temp %>%
#     st_centroid() %>%
#     mutate(LAD20NM=l)
#   oa_lad_temp<-oa_lad_temp %>%
#     mutate(lat=st_coordinates(oa_lad_temp)[,2],
#            lon=st_coordinates(oa_lad_temp)[,1])
#   labels<-rbind(labels,oa_lad_temp)
#   print(l)
# }

lcr<-st_read("./data/LCR.gpkg")
borders<-st_read("./data/LCR_LA.gpkg")
labels<-st_centroid(borders)
labels<-labels %>%
  mutate(
    lat=st_coordinates(labels)[,2],
    lon=st_coordinates(labels)[,1]
  )

m1<-ggplot()+
  geom_sf(aes(fill=quad_sig, group=quad_sig),moran.map,show.legend = T,color="NA")+
  scale_fill_discrete(type = c("#EFE51C","#414287","#E1E1E1"))+
  geom_sf(data = borders, fill="NA")+
  geom_text_repel(mapping = aes(x=lon,y=lat,label=name), data = labels,size=2.5,
                  color = "white",     # text color
                  bg.color = "grey30", # shadow color
                  bg.r = 0.15,
                  max.overlaps=Inf,
                  segment.color = "black")+
  #ggspatial::annotation_scale()+
  theme_void() +
  #labs(title="Liverpool City Region"
  #     )+
  theme(text = element_text(size=12), legend.title = element_blank(),legend.position = "bottom")


m2<-ggplot()+
  geom_sf(aes(fill=n),st_as_sf(oa_vars_access),show.legend = T,color="NA")+
  scale_fill_viridis_c(name="Service Types (Count)",breaks = c(1,4,8,12))+
  #ggspatial::annotation_scale()+
  geom_sf(data = borders, fill="NA")+
  geom_text_repel(mapping = aes(x=lon,y=lat,label=name), data = labels,size=2.5,
                  color = "white",     # text color
                  bg.color = "grey30", # shadow color
                  bg.r = 0.15,
                  max.overlaps=Inf,
                  segment.color = "black")+
  theme_void() +
  theme(text = element_text(size=11),legend.position = "bottom")

#M<-grid.arrange(m1,m2,nrow=1) 
M<-ggpubr::ggarrange(m1,m2,labels = "AUTO")
ggsave(paste0("img/maps_overview.png"),M,dpi=600)

#tm_basemap(leaflet::providers$Esri.WorldTopoMap) + tm_shape(borders)+tm_borders()

pop20<-oa_lad %>%
  st_drop_geometry()%>%
  select(OA11CD,LAD20NM,quad_sig,estPop2019) %>%
  group_by(LAD20NM,quad_sig) %>%
  dplyr::summarise(pop=sum(estPop2019)) 
pop20pct<-oa_lad %>%
  st_drop_geometry()%>%
  group_by(LAD20NM)%>%
  dplyr::summarise(tot=sum(estPop2019))%>%
  inner_join(pop20,by=c("LAD20NM"="LAD20NM"))%>%
  mutate(pct=(pop/tot)*100)

ggplot(pop20pct[pop20pct$quad_sig=="High Access",], aes(x=pct,y=LAD20NM,fill=LAD20NM))+
  geom_bar(stat = "identity",width = 0.5) +
  theme_minimal()+
  scale_fill_viridis_d()+
  theme(legend.position = "none",axis.title.y = element_blank())+
  labs(x="Population (%)")+
  geom_text(aes(label=round(pct,1),hjust=-0.05))+
  theme(text = element_text(size=16))

ggsave(paste0("img/pop20.png"),width = 25,height = 10,units = "cm")  

summary(oa_lad[oa_lad$quad_sig=="High Access",]$n)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#8.00   10.00   10.00   10.12   11.00   12.00 
summary(oa_lad[oa_lad$quad_sig=="Low Access",]$n)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#2.000   3.500   5.000   4.421   5.000   6.000 
summary(oa_lad$n)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   6.000   8.000   7.318   9.000  12.000 

oa_lad_neighb<-oa_lad %>% 
  filter(quad_sig=="High Access") %>%
  st_union() %>% 
  st_sf()

st_write(oa_lad,"output/maps_output/20minNeigh.gpkg")
st_write(oa_lad_neighb,"output/maps_output/20minNeighbnd.gpkg")
