### Coexistence mapping 
### Jessie Golding, Sushmita Bhandari, Mackenzie Waller

################################## Intro #######################################

# Description:  R code to load data, filter data, and perform analyses for 
# Waller et al. (In Prep) "Novel dataset synthesis of urban environment impacts 
# on human wildlife coexistence" manuscript
# Note: Start with the README file
# Date Updated: 8/21/2025

############################ Set working directory #############################
# Set working directory to where you have downloaded the data and want to save plots

# Change the "yourdirectory" portion of the code below with the appropriate filepath
# setwd("C:/yourdirectory")

############################## Load packages ###################################

library(tidyverse)
library(mapview)
library(sf)
library(raster)
library(osmdata)
library(viridis)
library(spdep)
library(tmap)
devtools::install_github("thomasp85/patchwork")
library(patchwork)

################################ Load data #####################################
### Spatial data

## Jurisdictional boundaries 
# City of Tucson
j <-st_read("./project_area.shp")%>%
  st_transform(., crs="EPSG:32612")

## Built-environment features (buffered shapefiles) 
# Water features (500 ft buffer)
w_500ft_buff <-st_read("./tucson_water_500ft_buffer.shp")%>%
  st_transform(., crs = crs(j)) 
st_area(w_500ft_buff)

# Parks (500 ft buffer)
p_500ft_buff <-st_read("./tucson_park_500ft_buffer.shp")%>%
  st_transform(., crs = crs(j)) 
st_area(p_500ft_buff)

# Roads (100 ft buffer)
r_100ft_buff <-st_read("./tucson_roads.shp")%>%
  st_transform(., crs = crs(j))%>%
  group_by(OBJECTID_1) %>% 
  summarise()%>%
  st_buffer(30.48) #add 100 ft (30.48m) buffer 
st_area(r_100ft_buff)

# Neighborhood boundaries
n <-st_read("./tucson_neighborhoods.shp")%>%
  st_transform(., crs="EPSG:32612")

### Interaction data

## Define data years - the consecutive years common to all interaction data sets
data_years <-c(2018,2019,2020,2021,2022,2023)

## Coexistence interactions

## 1. Mammal observation data
# Original data set years: 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 
# 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024   
# Filter data to only data years and transform projection and remove data with 
# >500m location uncertainty

d.p1 <-read_tsv("./pima_county_gbif_mammal_observations.csv",quote="") %>%
  st_as_sf(., coords = c("decimalLongitude", "decimalLatitude"), crs="EPSG:4326")%>%
  filter(year %in% data_years)%>%
  filter(coordinateUncertaintyInMeters<500)%>%
  st_transform(., crs = crs(j))

# Filter mammal observation data to only city of Tucson
# Final coexistence data set for analysis
mammal_obs = st_intersection(d.p1,j)

## Create species summary file
mammal_obs_sum <- mammal_obs %>%
  group_by(species)%>%
  summarise(n = n())%>%
  st_drop_geometry()%>%
  mutate(common_name = case_when(
    species == "Xerospermophilus tereticaudus" ~ "round-tailed ground squirrel",
    species == "Odocoileus hemionus" ~ "mule deer",
    species == "Canis latrans" ~ "coyote",
    species == "Sylvilagus audubonii"~ "desert cottontail",
    species == "Neotoma albigula" ~ "white-throated woodrat",
    species == "Sigmodon arizonae" ~ "Arizona cotton rat",
    species == "Otospermophilus variegatus" ~ "rock squirrel",
    species == "Tadarida brasiliensis" ~ "Mexican free-tailed bat",
    species == "Pecari tajacu" ~ "javelina",
    species == "Mus musculus" ~ "house mouse",
    species == "Parastrellus hesperus" ~ "canyon bat",
    species == "Felis catus" ~ "house cat",
    species == "Lepus californicus" ~ "black-tailed jackrabbit",
    species == "Procyon lotor" ~ "racoon",
    species == "Lynx rufus" ~ "bobcat",
    species == "Thomomys bottae" ~ "Botta's pocket gopher",
    species == "Taxidea taxus" ~ "badger",
    species == "Mephitis mephitis" ~ "striped skunk",
    species == "Sigmodon ochrognathus" ~ "yellow-nosed cotton rat",
    species == "Conepatus leuconotus" ~ "western hog-nosed skunk",
    species == "Spilogale gracilis" ~ "western spotted skunk",
    species == "Urocyon cinereoargenteus" ~ "gray fox",
    species == "Aeorestes cinereus" ~ "hoary bat",
    species == "Chaetodipus penicillatus" ~ "desert pocket mouse",
    species == "Lepus alleni" ~ "antelope jackrabbit",
    species == "Vulpes macrotis" ~ "kit fox",
    species == "Antrozous pallidus" ~ "pallid bat",
    species == "Puma concolor" ~ "mountain lion", 
    species == "Ammospermophilus harrisii" ~ "Harris's antelope squirrel", 
    species == "Canis lupus" ~ "gray wolf",
    species == "Didelphis virginiana" ~ "Virginia opossum", 
    species == "Leptonycteris yerbabuenae" ~ "lesser long-nosed bat",
    species == "Nasua narica" ~ "white-nosed coati", 
    species == "Peromyscus eremicus" ~ "cactus mouse"))

## Conflict interactions

## 1. Bite data
# Original data set years: 2009, 2010, 2012, 2014, 2015, 2016, 2018, 2019, 2020, 
# 2021, 2022, 2023
# Filter data to only data years and transform projection

d.n1 <-read_csv("./tucson_animal_bites.csv",quote="")%>%
  st_as_sf(., coords = c("Longitude", "Latitude"), crs="EPSG:4326")%>%
  filter(YEAR %in% data_years)%>%
  st_transform(., crs = crs(j))

# Filter bite data to only city of Tucson
# Final bite data set for analysis
bite_obs = st_intersection(d.n1, j)  

## 2. Roadkill
# Original data set years: 2012, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024
# Filter data to only data years and transform projection

d.n2 <-read_csv("./tucson_roadkill.csv")%>%
  filter(iconic_taxon_name == "Mammalia")%>%
  mutate(year = substr(observed_on, 1, 4))%>%
  st_as_sf(., coords = c("longitude", "latitude"), crs="EPSG:4326") %>%
  filter(year %in% data_years)%>%
  st_transform(., crs = crs(j))

# Filter roadkill to only city of Tucson
# Final roadkill data set for analysis
rdkill_obs = st_intersection(d.n2,j)  

############################## Map to check data ###############################
### Create interactive map to view interaction data
### Note that you can use the layer icon to toggle layers on/off

mapview(j, alpha.regions = 0, color = "black", lwd = 4)+
mapview(n, alpha.regions = 0, color = "gray23", lwd = 3)+
mapview(w_500ft_buff, alpha.regions = 0, color = "cornflowerblue", lwd = 1.5)+
mapview(p_500ft_buff, alpha.regions = 0, color = "seagreen3", lwd = 1.5)+
mapview(r_100ft_buff, alpha.regions = 0, color = "ivory4", lwd = 1.5)+
mapview(mammal_obs, col.regions="#0078ae")+
mapview(bite_obs, col.regions ="#ba4609")+ 
mapview(rdkill_obs, col.regions ="#ba4609") 

######################## Proportion within buffers #############################
### Calculate the proportion of interactions within built-environment feature 
### buffers
### Manuscript question 1: Do built‑environment features increase coexistence or
### conflict interactions?

## Coexistence interactions - 957 total mammal observations, 2018-2023
## Proportion of coexistence interactions within 500 ft buffer of water features
mammal_obs_in_500f_water_buff <-sum(lengths(st_intersects(mammal_obs, w_500ft_buff))) # 669 records
m_prop_water <-mammal_obs_in_500f_water_buff/nrow(mammal_obs) # 69% (669/957)

## Proportion of coexistence interactions within 500 ft buffer of park features
mammal_obs_in_500f_park_buff <-sum(lengths(st_intersects(mammal_obs, p_500ft_buff))) # 675 records
m_prop_park <-mammal_obs_in_500f_park_buff/nrow(mammal_obs) # 70% (675/957)

## Proportion of coexistence interactions within 100 ft buffer of road features
mammal_obs_in_100ft_road_buff <-sum(lengths(st_intersects(mammal_obs, r_100ft_buff))) # 283 records
m_prop_road <-mammal_obs_in_100ft_road_buff/nrow(mammal_obs) # 29% (283/957)

## Conflict interactions - 60 total wild animal bites, 2018-2023
## Proportion of bites within 500 ft buffer of water features
bite_obs_in_500f_water_buff <-sum(lengths(st_intersects(bite_obs, w_500ft_buff))) # 20 records
b_prop_water <-bite_obs_in_500f_water_buff/nrow(bite_obs) # 33% (20/60)

## Proportion of bites within 500 ft buffer of park features
bite_obs_in_500f_park_buff <-sum(lengths(st_intersects(bite_obs, p_500ft_buff))) # 13 records
b_prop_park <-bite_obs_in_500f_park_buff/nrow(bite_obs) # 21% (13/60)

## Proportion of bites within 100 ft buffer of road features 
bite_obs_in_100ft_road_buff <-sum(lengths(st_intersects(bite_obs, r_100ft_buff))) # 60 records
b_prop_road <-bite_obs_in_100ft_road_buff/nrow(bite_obs) # 100% (60/60)


## Conflict interactions - 51 total wild roadkill, 2018-2023
## Proportion of roadkill records within 500 ft buffer of water features
rdkill_obs_in_500f_water_buff <-sum(lengths(st_intersects(rdkill_obs, w_500ft_buff))) # 28 records
rk_prop_water <-rdkill_obs_in_500f_water_buff/nrow(rdkill_obs) # 54% (28/51)

## Proportion of roadkill records within 500 ft buffer of park features
rdkill_obs_in_500f_park_buff <-sum(lengths(st_intersects(rdkill_obs, p_500ft_buff))) # 23 records
rk_prop_park <-rdkill_obs_in_500f_park_buff/nrow(rdkill_obs) # 45% (23/51)

## Proportion of roadkill records within 100 ft buffer of road features
rdkill_obs_in_100ft_road_buff <-sum(lengths(st_intersects(rdkill_obs, r_100ft_buff))) # 28 records
rk_prop_road <-rdkill_obs_in_100ft_road_buff/nrow(rdkill_obs) # 55% (28/51)


########################### Generate random points #############################
### Generate random points to compare to interaction data. Use set.seed to generate
### the same points used in the analysis

## Generate random points (with the same as total number of records - 1,117 points [957+60+49+51])
set.seed(25)
random_points2 <-sf::st_sample(j, size=(nrow(mammal_obs) + nrow(bite_obs)+49+nrow(rdkill_obs)))%>%
  st_as_sf() 

## Proportion of random points within 500 ft buffer of water features
random_in_500f_water_buff2 <-sum(lengths(st_intersects(random_points2, w_500ft_buff))) # 325 points
r_prop_water2 <-random_in_500f_water_buff2/nrow(random_points2) # 29% (325/1,117)

## Proportion of random points within 500 ft buffer of park features
random_in_500f_park_buff2 <-sum(lengths(st_intersects(random_points2, p_500ft_buff))) # 166 points
r_prop_park2 <-random_in_500f_park_buff2/nrow(random_points2) # 15% (166/1,117)

## Proportion of random points within 100 ft buffer of road features
random_in_100f_road_buff2 <-sum(lengths(st_intersects(random_points2, r_100ft_buff))) # 345 points
r_prop_road2 <-random_in_100f_road_buff2/nrow(random_points2) # 31% (345/1,117)


############################## Map to check data ###############################
### Create interactive map to add random data
### Note that you can use the layer icon to toggle layers on/off

mapview(j, alpha.regions = 0, color = "black", lwd = 4)+
mapview(n, alpha.regions = 0, color = "gray23", lwd = 3)+
mapview(w_500ft_buff, alpha.regions = 0, color = "cornflowerblue", lwd = 1.5)+
mapview(p_500ft_buff, alpha.regions = 0, color = "seagreen3", lwd = 1.5)+
mapview(r_100ft_buff, alpha.regions = 0, color = "ivory4", lwd = 1.5)+
mapview(mammal_obs, col.regions="#0078ae")+
mapview(bite_obs, col.regions ="#ba4609")+ 
mapview(rdkill_obs, col.regions ="#ba4609")+
mapview(random_points2, col.regions = "deeppink")

########################### Combine data in data frames ########################
### Create new data frames that contain all data for plotting

## Create new data frames to hold buffer data
interaction2 <-c("Observations", "Animal bites", "Random Points", "Roadkill")
water2 <-c("Within 500ft of wash/stream", ">500 ft from wash/stream")
park <-c("Within 500ft of park", ">500 ft from park")
road <-c("Within 100ft of road", ">100 ft from road")

water_buffs <-expand.grid(as.character(interaction2), as.character(water2))
colnames(water_buffs) <-c("Interaction_type", "Distance_water")

park_buffs <-expand.grid(as.character(interaction2), as.character(park))
colnames(park_buffs) <-c("Interaction_type", "Distance_park")

road_buffs <-expand.grid(as.character(interaction2), as.character(road))
colnames(road_buffs) <-c("Interaction_type", "Distance_road")

water_buffs2 <-(water_buffs)%>%
  mutate(Interaction_type=as.character(Interaction_type))%>%
  mutate(proportion = case_when(
    Interaction_type == "Animal bites" & Distance_water == "Within 500ft of wash/stream" ~ b_prop_water,
    Interaction_type == "Animal bites" & Distance_water == ">500 ft from wash/stream" ~ (1-b_prop_water),
    Interaction_type == "Observations" & Distance_water == "Within 500ft of wash/stream" ~ m_prop_water,
    Interaction_type == "Observations" & Distance_water == ">500 ft from wash/stream" ~ (1-m_prop_water),
    Interaction_type == "Random Points" & Distance_water == "Within 500ft of wash/stream" ~ r_prop_water2,
    Interaction_type == "Random Points" & Distance_water == ">500 ft from wash/stream" ~ (1-r_prop_water2),
    Interaction_type == "Roadkill" & Distance_water == "Within 500ft of wash/stream" ~ rk_prop_water,
    Interaction_type == "Roadkill" & Distance_water == ">500 ft from wash/stream" ~ (1-rk_prop_water)))%>%
  mutate(total_in_buff = case_when(
    Interaction_type == "Animal bites" ~ bite_obs_in_500f_water_buff,
    Interaction_type == "Observations" ~ mammal_obs_in_500f_water_buff,
    Interaction_type == "Random Points" ~ random_in_500f_water_buff2,
    Interaction_type == "Roadkill" ~ rdkill_obs_in_500f_water_buff))%>%
  mutate(category = case_when(
    Interaction_type == "Animal bites" ~ "Conflict",
    Interaction_type == "Observations" ~ "Coexistence",
    Interaction_type == "Random Points"~ "Random",
    Interaction_type == "Roadkill" ~ "Conflict"))%>%
  mutate(percent = paste(round(proportion*100,1),"%", sep=""))%>%
  mutate(city_feature = "Water")

park_buffs2 <-(park_buffs)%>%
  mutate(Interaction_type=as.character(Interaction_type))%>%
  mutate(proportion = case_when(
    Interaction_type == "Animal bites" & Distance_park == "Within 500ft of park" ~ b_prop_park,
    Interaction_type == "Animal bites" & Distance_park == ">500 ft from park" ~ (1-b_prop_park),
    Interaction_type == "Observations" & Distance_park== "Within 500ft of park" ~ m_prop_park,
    Interaction_type == "Observations" & Distance_park == ">500 ft from park" ~ (1-m_prop_park),
    Interaction_type == "Random Points" & Distance_park == "Within 500ft of park" ~ r_prop_park2,
    Interaction_type == "Random Points" & Distance_park == ">500 ft from park" ~ (1-r_prop_park2),
    Interaction_type == "Roadkill" & Distance_park == "Within 500ft of park" ~ rk_prop_park,
    Interaction_type == "Roadkill" & Distance_park == ">500 ft from park" ~ (1-rk_prop_park)))%>%
  mutate(total_in_buff = case_when(
    Interaction_type == "Animal bites" ~ bite_obs_in_500f_park_buff,
    Interaction_type == "Observations" ~ mammal_obs_in_500f_park_buff,
    Interaction_type == "Random Points" ~ random_in_500f_park_buff2,
    Interaction_type == "Roadkill" ~ rdkill_obs_in_500f_park_buff))%>%
  mutate(category = case_when(
    Interaction_type == "Animal bites" ~ "Conflict",
    Interaction_type == "Observations" ~ "Coexistence",
    Interaction_type == "Random Points"~ "Random",
    Interaction_type == "Roadkill" ~ "Conflict"))%>%
  mutate(percent = paste(round(proportion*100,1),"%", sep=""))%>%
  mutate(city_feature = "Parks")

road_buffs2 <-(road_buffs)%>%
  mutate(Interaction_type=as.character(Interaction_type))%>%
  mutate(proportion = case_when(
    Interaction_type == "Animal bites" & Distance_road == "Within 100ft of road" ~ b_prop_road,
    Interaction_type == "Animal bites" & Distance_road == ">100 ft from road" ~ (1-b_prop_road),
    Interaction_type == "Observations" & Distance_road == "Within 100ft of road" ~ m_prop_road,
    Interaction_type == "Observations" & Distance_road == ">100 ft from road" ~ (1-m_prop_road),
    Interaction_type == "Random Points" & Distance_road == "Within 100ft of road" ~ r_prop_road2,
    Interaction_type == "Random Points" & Distance_road == ">100 ft from road" ~ (1-r_prop_road2),
    Interaction_type == "Roadkill" & Distance_road == "Within 100ft of road" ~ rk_prop_road,
    Interaction_type == "Roadkill" & Distance_road == ">100 ft from road" ~ (1-rk_prop_road)))%>%
  mutate(total_in_buff = case_when(
    Interaction_type == "Animal bites" ~ bite_obs_in_100ft_road_buff,
    Interaction_type == "Observations" ~ mammal_obs_in_100ft_road_buff,
    Interaction_type == "Random Points" ~ random_in_100f_road_buff2,
    Interaction_type == "Roadkill" ~ rdkill_obs_in_100ft_road_buff))%>%
  mutate(category = case_when(
    Interaction_type == "Animal bites" ~ "Conflict",
    Interaction_type == "Observations" ~ "Coexistence",
    Interaction_type == "Random Points"~ "Random",
    Interaction_type == "Roadkill" ~ "Conflict"))%>%
  mutate(percent = paste(round(proportion*100,1),"%", sep=""))%>%
  mutate(city_feature = "Roads")

################## Plot proportions within feature buffers #####################
### Plot proportions within features as pie charts

## Coexistence interactions - proportion of observations within 500 ft buffer of parks
p1o <-ggplot(park_buffs2[park_buffs2$Interaction_type == "Observations",], 
             aes(x="", y=proportion, group=Distance_park, color=Distance_park, 
                 fill=Distance_park)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  theme_void()+
  scale_fill_manual(name = "Parks", 
                    labels = c("Within buffer","Outside of buffer"),
                    values = c("black","white"))+
  geom_text(aes(label = percent), size=3, color = "white", 
            position = position_stack(vjust = .5)) +
  coord_polar("y", start=0) + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")

## Proportion of random points within 500 ft buffer of parks
p1r <-ggplot(park_buffs2[park_buffs2$Interaction_type == "Random Points",], 
             aes(x="", y=proportion, group=Distance_park, color=Distance_park, 
                 fill=Distance_park)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  theme_void()+
  scale_fill_manual(name = "Parks", 
                    labels = c("Within buffer", "Outside of buffer"),
                    values = c("black","white"))+
  geom_text(aes(label = percent), size=3, color = "white", 
            position = position_stack(vjust = .5)) +
  coord_polar("y", start=0) + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")

## Coexistence interactions - proportion of observations within 500 ft buffer of water features
p2o <-ggplot(water_buffs2[water_buffs2$Interaction_type == "Observations",], 
             aes(x="", y=proportion, group=Distance_water, color=Distance_water, 
                 fill=Distance_water)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  theme_void()+
  scale_fill_manual(name = "Water", 
                    labels = c("Within buffer","Outside of buffer"),
                    values = c("black","white"))+
  geom_text(aes(label = percent), size=3, color = "white", 
            position = position_stack(vjust = .5)) +
  coord_polar("y", start=0) + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")

## Random points - proportion within 500 ft buffer of water features
p2r <-ggplot(water_buffs2[water_buffs2$Interaction_type == "Random Points",], 
             aes(x="", y=proportion, group=Distance_water, color=Distance_water, 
                 fill=Distance_water)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  theme_void()+
  scale_fill_manual(name = "Water", 
                    labels = c("Within buffer", "Outside of buffer"),
                    values = c("black","white"))+
  geom_text(aes(label = percent), size=3, color = "white", 
            position = position_stack(vjust = .5)) +
  coord_polar("y", start=0) + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")

## Coexistence interactions - proportion of observations within 100 ft buffer of roads
p3o <-ggplot(road_buffs2[road_buffs2$Interaction_type == "Observations",], 
             aes(x="", y=proportion, group=Distance_road, color=Distance_road, 
                 fill=Distance_road)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  theme_void()+
  scale_fill_manual(name = "",
                    labels = c("Within buffer","Outside of buffer"),
                    values = c("black","white"))+
  geom_text(aes(label = percent), size=3, color = "white", 
            position = position_stack(vjust = .5)) +
  coord_polar("y", start=0) + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        axis.title = element_blank())

## Random points - proportion within 100 ft buffer of roads
p3r <-ggplot(road_buffs2[road_buffs2$Interaction_type == "Random Points",], 
             aes(x="", y=proportion, group=Distance_road, color=Distance_road, 
                 fill=Distance_road)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  theme_void()+
  scale_fill_manual(name = "", 
                    labels = c("Within buffer", "Outside of buffer"),
                    values = c("black","white"))+
  geom_text(aes(label = percent), size=3, color = "white", 
            position = position_stack(vjust = .5)) +
  coord_polar("y", start=0) + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        axis.title = element_blank())

## Create stitched together plot of coexistence interactions using patchwork 
## package (need devtools to use) - double check patchwork is loaded by running 
## library command again
library(patchwork)
pos_occ_plot <-p1o + p2o + p3o + p1r + p2r + p3r + plot_layout(ncol = 3) + plot_annotation(tag_levels = 'A')

# Code to save coexistence interaction plot files as png or svg
# ggsave(file="./pos_occ_all.svg", plot=pos_occ_plot, width=10, height=12)
# ggsave(file="./pos_occ_all.png", plot=pos_occ_plot, width=10, height=12)


## Conflict interactions - proportion of bites within 500 ft buffer of parks
n1b <-ggplot(park_buffs2[park_buffs2$Interaction_type == "Animal bites",], 
             aes(x="", y=proportion, group=Distance_park, color=Distance_park, 
                 fill=Distance_park)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  theme_void()+
  scale_fill_manual(name = "Parks", 
                    labels = c("Within buffer","Outside of buffer"),
                    values = c("black","white"))+
  geom_text(aes(label = percent), size=3, color = "white", 
            position = position_stack(vjust = .5)) +
  coord_polar("y", start=0) + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")

## Conflict interactions - proportion of collisions within 500 ft buffer of parks
n1c <-ggplot(park_buffs2[park_buffs2$Interaction_type == "Animal/vehicle collisions",], 
             aes(x="", y=proportion, group=Distance_park, 
                 color=Distance_park, fill=Distance_park)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  theme_void()+
  scale_fill_manual(name = "Parks", 
                    labels = c("Within buffer","Outside of buffer"),
                    values = c("black","white"))+
  geom_text(aes(label = percent), size=3, color = "white", 
            position = position_stack(vjust = .5)) +
  coord_polar("y", start=0) + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")

## Conflict interactions - proportion of roadkill within 500 ft buffer of parks
n1rk <-ggplot(park_buffs2[park_buffs2$Interaction_type == "Roadkill",], 
              aes(x="", y=proportion, group=Distance_park, color=Distance_park, 
                  fill=Distance_park)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  theme_void()+
  scale_fill_manual(name = "Parks", 
                    labels = c("Within buffer","Outside of buffer"),
                    values = c("black","white"))+
  geom_text(aes(label = percent), size=3, color = "white", 
            position = position_stack(vjust = .5)) +
  coord_polar("y", start=0) + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")

## Proportion of random points within 500 ft buffer of parks
n1r <-ggplot(park_buffs2[park_buffs2$Interaction_type == "Random Points",], 
             aes(x="", y=proportion, group=Distance_park, color=Distance_park, 
                 fill=Distance_park)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  theme_void()+
  scale_fill_manual(name = "Parks", 
                    labels = c("Within buffer", "Outside of buffer"),
                    values = c("black","white"))+
  geom_text(aes(label = percent), size=3, color = "white", 
            position = position_stack(vjust = .5)) +
  coord_polar("y", start=0) + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")

## Proportion of bites within 500 ft buffer of water features
n2b <-ggplot(water_buffs2[water_buffs2$Interaction_type == "Animal bites",], 
             aes(x="", y=proportion, group=Distance_water, color=Distance_water, 
                 fill=Distance_water)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  theme_void()+
  scale_fill_manual(name = "Water", 
                    labels = c("Within buffer","Outside of buffer"),
                    values = c("black","white"))+
  geom_text(aes(label = percent), size=3, color = "white", 
            position = position_stack(vjust = .5)) +
  coord_polar("y", start=0) + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")

## Proportion of roadkill within 500 ft buffer of water features
n2rk <-ggplot(water_buffs2[water_buffs2$Interaction_type == "Roadkill",], 
              aes(x="", y=proportion, group=Distance_water, 
                  color=Distance_water, fill=Distance_water)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  theme_void()+
  scale_fill_manual(name = "Water", 
                    labels = c("Within buffer","Outside of buffer"),
                    values = c("black","white"))+
  geom_text(aes(label = percent), size=3, color = "white", 
            position = position_stack(vjust = .5)) +
  coord_polar("y", start=0) + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")

## Proportion of random points within 500 ft buffer of water features
n2r <-ggplot(water_buffs2[water_buffs2$Interaction_type == "Random Points",], 
             aes(x="", y=proportion, group=Distance_water, color=Distance_water, 
                 fill=Distance_water)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  theme_void()+
  scale_fill_manual(name = "Water", 
                    labels = c("Within buffer", "Outside of buffer"),
                    values = c("black","white"))+
  geom_text(aes(label = percent), size=3, color = "white", 
            position = position_stack(vjust = .5)) +
  coord_polar("y", start=0) + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        axis.title = element_blank(),
        legend.position = "none")

## Proportion of bites within 100 ft buffer of roads
n3b <-ggplot(road_buffs2[road_buffs2$Interaction_type == "Animal bites",], 
             aes(x="", y=proportion, group=Distance_road, color=Distance_road, 
                 fill=Distance_road)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  theme_void()+
  scale_fill_manual(name = "",
                    labels = c("Within buffer","Outside of buffer"),
                    values = c("black","white"))+
  coord_polar("y", start=0) + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        axis.title = element_blank())

## Proportion of roadkill within 100 ft buffer of roads
n3rk <-ggplot(road_buffs2[road_buffs2$Interaction_type == "Roadkill",], 
              aes(x="", y=proportion, group=Distance_road, color=Distance_road, 
                  fill=Distance_road)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  theme_void()+
  scale_fill_manual(name = "",
                    labels = c("Within buffer","Outside of buffer"),
                    values = c("black","white"))+
  geom_text(aes(label = percent), size=3, color = "white", 
            position = position_stack(vjust = .5)) +
  coord_polar("y", start=0) + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        axis.title = element_blank())

## Proportion of random points within 100 ft buffer of roads
n3r <-ggplot(road_buffs2[road_buffs2$Interaction_type == "Random Points",], 
             aes(x="", y=proportion, group=Distance_road, color=Distance_road, 
                 fill=Distance_road)) +
  geom_bar(width = 1, stat = "identity", color = "black") +
  theme_void()+
  scale_fill_manual(name = "", 
                    labels = c("Within buffer", "Outside of buffer"),
                    values = c("black","white"))+
  geom_text(aes(label = percent), size=3, color = "white", 
            position = position_stack(vjust = .5)) +
  coord_polar("y", start=0) + 
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank(),
        axis.title = element_blank())

# Create stitched together plot using patchwork package
neg_occ_plot <- n1b + n2b + n3b + n1rk + n2rk + n3rk + n1r + n2r + n3r + plot_layout(ncol = 3) + plot_annotation(tag_levels = 'A')

# Code to save conflict interaction plot files as png or svg
# ggsave(file="./neg_occ_all.svg", plot=neg_occ_plot, width=10, height=12)
# ggsave(file="./neg_occ_all.png", plot=neg_occ_plot, width=10, height=12)


########################### Summarize by neighborhood ##########################
### Summary by neighborhoods 
### Manuscript question 4: Are built environment spatial planning units 
### (e.g., neighborhoods) meaningful scales to understand human–wildlife 
### interactions?

# Create data frame (n) that includes a correction factor for area (square meters to
# square feet conversion - multiply by 10.7)

n$mammal_obs <-lengths(st_intersects(n, mammal_obs))
n$bites <-lengths(st_intersects(n, bite_obs))
n$rdkill <-lengths(st_intersects(n, rdkill_obs))
n$mammal_obs_per_area <-(n$mammal_obs)/as.numeric((st_area(n)*10.7))
n$bites_per_area <-(n$bites)/as.numeric((st_area(n)*10.7))
n$rdkill_per_area <-(n$rdkill)/as.numeric((st_area(n)*10.7))

## Create plots of summaries by neighborhood

# Coexistence interaction - mammal observations - by neighborhood
obsplot <-ggplot(n) +
  geom_sf(aes(fill = mammal_obs_per_area))+
  scale_fill_gradient(low = "white",  high = "#0078ae", na.value = NA)+
  labs(fill = "Mammal observations per sq ft\nby neighborhood (2018-2023)") + 
  theme_minimal()

# Conflict interaction - wild animal bites - by neighborhood
biteplot <-ggplot(n)+
  geom_sf(aes(fill = bites_per_area))+
  scale_fill_gradient(low = "white",  high = "#ba4609", na.value = NA)+
  labs(fill = "Bites per sq ft\nby neighborhood (2018-2023)") + 
  theme_minimal()

# Conflict interaction - roadkill - by neighborhood
rdkillplot <-ggplot(n) +
  geom_sf(aes(fill = rdkill_per_area))+
  scale_fill_gradient(low = "white",  high = "#ba4609", na.value = NA)+
  labs(fill = "Roadkill per sq ft\nby neighborhood (2018-2023)") + 
  theme_minimal()

# Code to save plot files as png or svg
# ggsave(file="./mammal_obs_neighborhood.svg", plot=obsplot, width=10, height=12)
# ggsave(file="./mammal_obs_neighborhood.png", plot=obsplot, width=10, height=12)
# ggsave(file="./mammal_bites_neighborhood.svg", plot=biteplot, width=10, height=12)
# ggsave(file="./mammal_bites_neighborhood.png", plot=biteplot, width=10, height=12)
# ggsave(file=".mammal_crashes_neighborhood.svg", plot=crashplot, width=10, height=12)
# ggsave(file=".mammal_crashes_neighborhood.png", plot=crashplot, width=10, height=12)
# ggsave(file="./mammal_rdkill_neighborhood.svg", plot=rdkillplot, width=10, height=12)
# ggsave(file="./mammal_rdkill_neighborhood.png", plot=rdkillplot, width=10, height=12)



################ Create scatter plot of conflict/coexistence ###################
### Create scatter plot to compare counts of conflict and coexistence interactions
## X axis - # of conflict points within particular buffer (conflict = bite, crash, roadkill)
## Y axis - # of coexistence points within particular buffer (coexistence = mammal obs)

all_int <-bind_rows(water_buffs2, road_buffs2, park_buffs2)%>%
  dplyr::select(Interaction_type, total_in_buff, city_feature, category)%>%
  filter(category != "Random")%>%
  distinct()

all_int_plot <- all_int%>%
  group_by(city_feature, category) %>%
  summarise(across(total_in_buff, sum))%>%
  pivot_wider(names_from = category, values_from = total_in_buff)

scatter <-ggplot(all_int_plot, aes(Conflict, Coexistence, color = city_feature)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE) +
  labs(title = "Coexistence and conflict urban wildlife interactions\nwithin buffers of built environment features\nin Tucson 2018-2023")+
  geom_text(aes(label = city_feature), size=3, color = "black", position = position_stack(vjust = .95)) +
  ylab("Count of COEXISTENCE interactions") +
  xlab("Count of CONFLICT interactions") +
  scale_x_continuous(limits = c(0, max(all_int_plot$Conflict)+5), breaks = scales::pretty_breaks(n = 15))+
  scale_y_continuous(limits = c(0, max(all_int_plot$Coexistence)+5), breaks = scales::pretty_breaks(n = 15))+
  theme_minimal() 

# Code to save plot as png or svg
#ggsave(file="./scatter_plot_sum.svg", plot=scatter, width=10, height=12)
#ggsave(file="./scatter_plot_sum.png", plot=scatter, width=10, height=12)


################ Center and normalize data for z-score comparison ##############
### Counts of coexistence are much higher than conflict so to compare counts,
### center and scale both data sets on 0 and compare standard deviations from the
### mean (z-score) for each category of interactions within buffers
### Manuscript question 3: Do increased interactions between humans and wildlife 
### lead to more of one type of interaction (coexistence or conflict)?


# Sample data
coex <- c(all_int_plot$Coexistence)
conf <- c(all_int_plot$Conflict)

# Applying Centering and scaling
normalized_coex2 <- scale(coex, center = TRUE, scale=TRUE)
normalized_conf2 <- scale(conf, center = TRUE, scale=TRUE)
