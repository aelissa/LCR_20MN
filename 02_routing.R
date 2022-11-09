library(opentripplanner)
library(tidyverse)
library(nngeo)


otpcon <- otp_connect(hostname =  "localhost",
                      router = "default",
                      port = 8080)


###################
##### ROUTING #####
###################

####To services by walking

data<-do.call("list",mget(ls(pattern = "d_")))
label<-gsub("d_","",ls(pattern = "d_"))

for(d in 1:length(data)){
  
  m<-fromToPlaces(pc,data[[d]],5)
  fid<-m$OA11CD
  tid<-as.character(m$ref_no)
  route <- otp_plan(otpcon, 
                    fromPlace = as.matrix(m[,4:3]), 
                    toPlace = as.matrix(m[,6:5]),
                    fromID = fid,
                    toID = tid,
                    mode=c("WALK"), 
                    ncores = 80, 
                    numItineraries = 1,
                    get_geometry = F)
  
  file<-paste0("output/",label[d],"_walk.rds") 
  saveRDS(route,file)
}





