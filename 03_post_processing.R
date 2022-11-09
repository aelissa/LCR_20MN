library(sf)
library(tidyverse)

file_list <- list.files(path="/output",full.names = T, pattern = "_walk.rds") 


#####################################################
######### COMBINING ACCESSIBILITY DOMAINS ###########
#####################################################
####To Services

#fn<-c("mean")
#n=1
#for(ngb in c(1)){
  dfname<-"access"
  for (i in 1:length(file_list)){
    print(file_list[i])
    d<-str_extract(file_list[i], "(?<=output/)(.*?)(?=_)")
    if(i==1){
      data<-readRDS(file_list[i])
      name<-str_extract(file_list[i], "(?<=output/)(.*?)(?=_)") %>%
        str_to_title()
      tdata<-walk_transform(data,name,"mean",1) 
      assign(dfname,tdata)
    }else{
      data<-readRDS(file_list[i])
      name<-str_extract(file_list[i], "(?<=output/)(.*?)(?=_)") %>%
        str_to_title()
      tdata<-walk_transform(data,name,"mean",1) 
      #Combine access domains
      assign(dfname,inner_join(get(dfname),tdata,by=c("fromPlace"="fromPlace")))
    }
  }
  
write.csv(get(dfname),paste0("/output/post_processing_output/access.csv"),row.names = F) #name access file based on number of n

access_score<-access %>%
  tidyr::pivot_longer(starts_with("walkTime")) %>%
  mutate(ngbCondition=ifelse(value<=568,1,0))%>%
  mutate(name=sub("\\..*","",name),#) %>%
          name=sub("\\.", "",name),
          name=sub("([0-9]|[1-9][0-9]+).*$", "", name)) %>%
  group_by(fromPlace,name) %>%
  summarise(tot=n(),
            n=sum(ngbCondition,na.rm = T))%>%
  mutate(
    score=n/tot
  )
  

write.csv(access_score,"/output/post_processing_output/access_score.csv",row.names = F) #name access file based on number of n


 
