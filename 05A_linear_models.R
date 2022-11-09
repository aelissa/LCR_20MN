library(ggcorrplot)
library(ggplot2)
library(RColorBrewer)
library(GWmodel)
library(parallel)
library(tmap)
library(tidyverse)
library(sf)
library(bestNormalize)
library(sp)
library(scales)
library(spdep)

oa_vars_access<-st_read("data/oa_vars_access.gpkg")

####Normalize variables

vars<-oa_vars_access%>%
  st_as_sf()%>%
  st_drop_geometry()%>%
  select(OA11CD, resPop,estPop2019,carOwn,medianAge,depChild,IMDeduS,IMDincomeS,blackPct,whitePct,asianPct,mltEthnPct,otherEthn,IMDdisRatio,whitePct,IMDtraffic,IMDair,HP_5.year.avg, n) %>%
  mutate(carOwn=(carOwn/resPop)*100) %>%
  rename(housePrice5yrsAvg=HP_5.year.avg)
BN_vars<-list()
l<-0
vars_norm<-vars[c("OA11CD")]
chosen_norm<-NULL
for(i in 2:ncol(vars)){
  l<-l+1
  BN_vars[[l]]<-bestNormalize(vars[,i], r = 1, k = 5)
  chosen_norm[l]<-class(BN_vars[[l]]$chosen_transform)
  vars_norm<-cbind(vars_norm,BN_vars[[l]]$x.t)
}

names(vars_norm)<-names(vars)

###OrdNorm Ordered quantile normalization
###Box–Cox transformation
###arcsine square root transformation
###Yeo-Johnson transformation is very similar to the Box-Cox power transformation
###square-root transformation

#####feature selection

M <- cor(vars_norm[c(3:18)])
#res1 <- cor.mtest(vars_norm[c(2:19)], conf.level = .95)
#res2 <- cor.mtest(vars_norm[c(2:19)], conf.level = .99)
ggcorrplot(M,hc.order = F, type = "lower",colors = brewer.pal(n = 3, name = "RdYlBu"))

ggsave("img/corrplor.jpg",units = c("cm"),width = 30,height = 30)

####demo
  
lm1<-lm(n ~ estPop2019 + medianAge + blackPct + whitePct + asianPct + mltEthnPct + otherEthn, data=vars_norm)

car::vif(lm1)

summary(lm1)

resids<-residuals(lm1)

lm2<-lm(n ~ estPop2019 + medianAge + blackPct + asianPct + mltEthnPct + otherEthn, data=vars_norm)

car::vif(lm2)

summary(lm2)

####socio-economic variables: IMD & car

lm3<-lm(n ~ estPop2019 + medianAge + blackPct + asianPct + mltEthnPct + otherEthn + carOwn + IMDdisRatio + IMDincomeS + IMDeduS + housePrice5yrsAvg, data = vars_norm)

car::vif(lm3)

summary(lm3)

resids<-residuals(lm3)

lm4<-lm(n ~ estPop2019 + medianAge + blackPct + + asianPct + mltEthnPct + otherEthn + carOwn + housePrice5yrsAvg + IMDeduS, data = vars_norm)

car::vif(lm4)

summary(lm4)

####Environmental vars

lm5<-lm(n ~ estPop2019 + medianAge + blackPct + asianPct + mltEthnPct + otherEthn + carOwn + housePrice5yrsAvg + IMDtraffic + IMDair, data=vars_norm)

car::vif(lm5)

summary(lm5)

lm6<-lm(n ~ estPop2019 + blackPct + mltEthnPct + otherEthn + carOwn + housePrice5yrsAvg + IMDtraffic + IMDair, data=vars_norm)

car::vif(lm6)

summary(lm6)

resids <- residuals(lm6)


# test moran
map.resids <- cbind(st_as_sf(oa_vars_access[c("OA11CD")]), resids)
coords<-sp::coordinates(as_Spatial(oa_vars_access))
vars_nb <- poly2nb(as_Spatial(oa_vars_access))  #queen contiguity
vars_w<-nb2listw(vars_nb)
vars_w
moran.test(map.resids$resids,listw=vars_w)

qtm(map.resids, fill = "resids", borders = NULL)

####scale variables for GWR

vars_norm<-vars_norm %>%
  mutate(across(where(is.numeric), scale))

vars_norm_centroids<-st_centroid(vars_norm)
  
vars_norm<-vars_norm %>%
  mutate(
    x=st_coordinates(st_as_sf(vars_norm))[,1],
    y=st_coordinates(st_as_sf(vars_norm))[,2])

st_write(vars_norm,"model_input.gpkg")






