##########################################
######           HELPERS            ######
##########################################
#All functions implemented for this study are here. Run this first to use 
#them throughout the code.

##########################################
###### DATA PRE-PROCESSING FUNCTION ######
##########################################

#This function returns an origin destination matrix where the number of destinations is based on the n nearest neighbour of choice

fromToPlaces<-function(fromPlace,toPlace,nn){
  toPlace<-toPlace %>%
    mutate(id=as.integer(rownames(toPlace)),
           toPlaceLon=st_coordinates(st_as_sf(toPlace))[,1],
           toPlaceLat=st_coordinates(st_as_sf(toPlace))[,2])
  fromPlace<-fromPlace %>%
    select(Postcode,oa11cd)
  nn_out<-unlist(st_nn(fromPlace,toPlace,k=nn,parallel = T))
  
  nn_out<-data.frame(id=rep(paste0("n",1:nn),nrow(fromPlace)),nn_out=nn_out) %>%
    mutate(pc_id=rep(1:nrow(fromPlace),each=nn)) %>%
    tidyr::pivot_wider(names_from = id, values_from = nn_out)
  df<-data.frame(fromPlace,nn_out)
  df<-df %>% 
    mutate(fromPlaceLon=st_coordinates(st_as_sf(df))[,1],
           fromPlaceLat=st_coordinates(st_as_sf(df))[,2]) %>%
    tidyr::pivot_longer(cols = starts_with("n"),names_to = "nn_out", values_to = "toPlace") %>%
    inner_join(toPlace,by=c("toPlace"="id")) %>%
    select(oa11cd,ref_no,fromPlaceLat,fromPlaceLon,toPlaceLat,toPlaceLon)
  return(df)
}


#############################################
######## POST-PROCESSING FUNCTION ###########
#############################################

# This function transforms the routing output to in a dataframe with all the postcode origins and the
# respective routed time and distance to n neighbors services. It takes four parameters: the routing output, the accessibility domain (i.e. food, health, education etc.), the aggregating function and the number of nearest neighbors to include. 
#The aggregating function determines how routed time and distance from each postcode origin  to n nearest neighbors will be summarised. Also, it is possible to set the desired number of nearest neighbors. In this study we iterate this function with the min, average and max as aggregating function and 1, 3 and 5 nearest neighbors.

walk_transform<-function(x,name,FUN,n){
  col1<-paste0("walkTime",name)
  col2<-paste0("walkDistance",name)
  x<-x %>%
    dplyr::group_by(fromPlace,toPlace,.groups='drop') %>%
    dplyr::summarise(walkTime=mean(walkTime),
                     walkDistance=mean(walkDistance)) %>%
    group_by(fromPlace, .groups='drop')%>%
    slice_min(n = n, order_by = walkTime) %>%
    dplyr::summarise(walkDistance=exec(FUN,walkDistance),
                     walkTime=exec(FUN,walkTime)) %>%
    rename(!!col1:=walkTime,
           !!col2:=walkDistance)%>%
    select(-.groups)
  
  return(x)
}


###########################################################################
#####################VISUALIZATION#########################################
###########################################################################

compare_surfaces<-function(results,varName){
  
  if(varName=="housePrice5yrsAvg"){varNameFill="housePrice"}else{varNameFill=varName}
  
  m1<-ggplot()+
    geom_sf(aes_string(fill=varName),results,show.legend = T,color="NA")+
    scale_fill_continuous_diverging(palette="Green-Brown",mid=0)+
    geom_sf(data = borders, fill="NA",color="white")+
    labs(fill=varNameFill)+
    theme_void()
  gwr_varName<-paste0("gwr_",varName)
  m2<-ggplot()+
    geom_sf(aes_string(fill=gwr_varName),results,show.legend = T,color="NA")+
    scale_fill_continuous_diverging(palette="Green-Brown",mid=0,na.value="grey")+
    geom_sf(data = borders, fill="NA",color="white")+
    labs(fill="GWR")+
    theme_void()
  mgwr_varName<-paste0("mgwr_",varName)
  m3<-ggplot()+
    geom_sf(aes_string(fill=mgwr_varName),results,show.legend = T,color="NA")+
    scale_fill_continuous_diverging(palette="Green-Brown",mid=0,na.value="grey")+
    geom_sf(data = borders, fill="NA",color="white")+
    labs(fill="MGWR")+
    theme_void()
  
  M<-ggpubr::ggarrange(m1,m2,m3,nrow=1,labels = "AUTO") 
  
  file=paste0("img/models/",varName,".png")
  
  ggsave(file=file,dpi = 800, M,width = 30,
         height = 10,
         units = c("cm"))
  
}

