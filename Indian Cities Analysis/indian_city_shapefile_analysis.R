library(ggplot2)
library(reshape2)
library(maptools)
library(plyr)
library(dplyr)
library(ggthemes)
library(mapproj)
require(gridExtra)

cdb <- read.csv("~/MSPA/Kaggle/cities_r2.csv")
cdb<-transform(cdb,location=colsplit(location,pattern=",",names=c("lat","lon")))

#Add region information
region <- read.csv("~/MSPA/Kaggle/regionwise_state.csv")
#correcting the column name to lower case so as to merge with cityDb
colnames(region)[1] = "state_name"
cityDb<-merge(cdb,region,by="state_name")
#Remove special char & to match the map id
cityDb$state_name<- gsub("&","AND",cityDb$state_name)

#Create india map for later use
indMap <- readShapeSpatial("~/MSPA/Kaggle/India_SHP/INDIA.shp")
#plot(indMap)
names(indMap)
print(indMap$ST_NAME)
indMap$ST_NAME <- toupper(indMap$ST_NAME)
indMap.df <- fortify(indMap,region="ST_NAME")

#Aggregate data state wise
stateDb <- aggregate.data.frame(cityDb[,c(5:13,20:22)],by=list(cityDb$state_name),FUN=sum)
#Rename the column name to merge the region information
colnames(stateDb)[1] = "state_name"
region$state_name <- gsub("&","AND",region$state_name)
stateDb<- merge(stateDb,region,by="state_name")
#Count the no. of cities in top 500 list per state
stateDb$NumCities <- (plyr::count(cityDb,vars=c("state_name")))$freq

#create a dummy frame equivalent to stateDb to add the missing states data.
#This is needed for correct rendering of the map.
dummyStateData <- stateDb[1:7,]
#Adding the missing states names to confirm it to the size of spatial dataframe indMap.df
dummyStateData[,1] <- c("ARUNACHAL PRADESH","DADRA AND NAGAR HAVELI","DAMAN AND DIU",
                        "LAKSHADWEEP","GOA","MANIPUR","SIKKIM")
dummyStateData[,14] <- c("NorthEast","West","West","South","South","NorthEast","NorthEast")
dummyStateData[,c(2:13,15)] <- NA #c(0,0,0,0,0,0,0)
stateDb<-rbind(stateDb,dummyStateData)
stateDb$state_name<- gsub("PUDUCHERRY","PONDICHERRY",stateDb$state_name)
colnames(stateDb)[1] <- "id"
indMap.df$state_name <- indMap.df$id

#Plot sex ratio and child sex ratio
sex_ratio_plot <- 
  ggplot(data=stateDb,aes(map_id=id))+
  geom_map(aes(fill=stateDb$population_female/stateDb$population_male),map=indMap.df) +
  expand_limits(x=indMap.df$long,y=indMap.df$lat) +
  scale_fill_continuous(low="#f7f4f9",high="#91003f",name="Ratio") +
  theme(panel.border = element_rect(fill=NA,color='black'),
          panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
         legend.position = c(0.1,0.2))
  

sex_ratio_plot <- sex_ratio_plot +labs(title="Sex Ratio Per State")

child_sex_ratio_plot <- ggplot(data=stateDb,aes(map_id=id))+
  geom_map(aes(fill=stateDb$X0.6_population_female/stateDb$X0.6_population_male),map=indMap.df) +
  expand_limits(x=indMap.df$long,y=indMap.df$lat) + 
  scale_fill_continuous(low="#f7f4f9",high="#91003f",name="Ratio") +
  theme(panel.border = element_rect(fill=NA,color='black'),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.1,0.2))
child_sex_ratio_plot <- child_sex_ratio_plot +labs(title="Child Sex Ratio Per State")


grid.arrange(sex_ratio_plot,child_sex_ratio_plot,ncol=2)
#By keeping the plots side by side we see that states like assam,HP,west bengal have high child sex ratio 
#but low overall sex ratio. This might suggest some movement of females after adulthood into the previously white 
#states like Haryana, Gujrat,etc.

ggplot()+geom_polygon(data=indMap.df,aes(x=long,y=lat,group=group),col="black",fill="white") +coord_fixed() +
  geom_point(data=cityDb,aes(x=location$lon,y=location$lat,size=population_total),alpha=0.4,col="#33a02c")+
  theme_void()

ggplot(data=stateDb,aes(map_id=id))+
  geom_map(aes(fill=stateDb$literates_male/stateDb$literates_female),map=indMap.df,col='white')+
  scale_fill_continuous(low = '#008837',high = '#fee090',na.value = '#a6cee3',name="M/F literacy ratio") +
  expand_limits(x=indMap.df$long,y=indMap.df$lat) +theme_void()

#city_count_per_state = as.data.frame(table(cityDb$state_code))$Freq
ggplot(data=stateDb,aes(map_id=id))+
  geom_map(aes(fill=NumCities/5),
           map=indMap.df,col='white')+
  scale_fill_continuous(low = '#4575b4',high = '#d73027',na.value = '#a6cee3',name="% cities amongst 500") +
  expand_limits(x=indMap.df$long,y=indMap.df$lat) +theme_void()

ggplot(data=stateDb,aes(map_id=id))+
  geom_map(aes(fill=total_graduates/literates_total * 100),map=indMap.df,col='white')+
  scale_fill_continuous(low = '#4575b4',high = '#d73027',na.value = '#a6cee3',name="% graduates among literates") +
  expand_limits(x=indMap.df$long,y=indMap.df$lat) +theme_void()
  
ggplot(data=cityDb,aes(log(literates_total),log(total_graduates),col=region))+geom_point()+
  scale_color_brewer("",palette = "Set1")+
  facet_grid(. ~region)#+ geom_text(aes(label=name_of_city))

data = as.matrix(mtcars)
dist_state_data = aggregate.data.frame(cityDb[,c(5:13,20:22)],by=list(cityDb$region,cityDb$dist_code),FUN = sum)
transform(dist_state_data[,1:3])
require(data.table)
setDT(dist_state_data[,1:3])

ggplot(data = stateDb,aes(region,id))+geom_tile(aes(fill=log10(literates_total)))+
scale_fill_gradient2(low = "#1f78b4", 
                     mid = "#b2df8a", 
                     high = "#33a02c",midpoint = 6.0) 
