library(sf)
library(tidyverse)


####POI####
poi<-st_read("data/poi.gpkg") %>% st_transform(4326)

###Green Space
d_pp<-st_read("~/LCR/public_parks_access.gpkg") %>% st_transform(4326) %>%
  rename(ref_no=refToGSite)

####Transit
d_tran1<-poi %>%
  filter(classname=="Bus Stops")

d_tran2<-poi %>%
  filter(classname=="Railway Stations, Junctions and Halts")

###Education
d_edu1<-poi %>%
  filter(classname=="First, Primary and Infant Schools")

###Health
d_health1<- poi %>%
  filter(classname=="Chemists and Pharmacies")
d_health2<- poi %>%
  filter(classname=="Doctors Surgeries")
d_health3<- poi %>%
  filter(classname=="Hospitals")

###Recreation and community infrastructure
d_rci<- poi %>%
  filter(categoryname=="Recreational")

###Places of worship
d_pow<- poi %>%
  filter(classname=="Places Of Worship")

###Sport
d_sport<- poi %>%
  filter(categoryname=="Sports Complex")

###Entertainment
d_ent<- poi %>%
  filter(classname=="Cinemas" | classname=="Nightclubs" | classname=="Social Clubs" | classname=="Theatres and Concert Halls") #25 Venues, stage and screen 02 Eating and drinking


####Food
d_food1 <- poi %>%
  filter(classname=="Bakeries" | classname=="Butchers" | classname=="Confectioners" | classname=="Delicatessens" | classname=="Fishmongers" | classname=="Tea and Coffee Merchants" | classname=="Herbs and Spices" | classname=="Grocers, Farm Shops and Pick Your Own") 

d_food2 <- poi %>%
  filter(classname=="Supermarket Chains" | classname=="Convenience Stores and Independent Supermarkets")


#####POSTCODES####

pc<-st_read("data/pc.gpkg") %>% st_transform(4326) #load postcodes
lookup<-read_csv("~/Projects/base_layers/lookup/NSPCL_FEB19_UK_LU.csv") %>%
  tidyr::pivot_longer(starts_with("pcd"),names_to = "pc-type", values_to = "Postcode") %>%
  select(oa11cd,Postcode)

pc_oa<-pc %>%
  inner_join(lookup,by=c("Postcode"="Postcode"))%>%
  filter(!duplicated(Postcode))
