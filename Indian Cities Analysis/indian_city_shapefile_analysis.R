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
#Convert it into a dataframe
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
  geom_map(aes(fill=stateDb$population_female/stateDb$population_male),map=indMap.df,col="darkgray") +
  expand_limits(x=indMap.df$long,y=indMap.df$lat) +
  scale_fill_continuous(low="#ffffcc",high="#006837",name="Ratio") +
  theme(panel.border = element_rect(fill=NA,color='black'),
          panel.background = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
         legend.position = c(0.1,0.2)) +
  geom_text_repel(data=slolat, aes(long,lat,label=tolower(id),group=NULL))
  

sex_ratio_plot <- sex_ratio_plot +labs(title="Sex Ratio Per State")

child_sex_ratio_plot <- ggplot(data=stateDb,aes(map_id=id))+
  geom_map(aes(fill=stateDb$X0.6_population_female/stateDb$X0.6_population_male),map=indMap.df,col="darkgray") +
  expand_limits(x=indMap.df$long,y=indMap.df$lat) + 
  scale_fill_continuous(low="#fff5eb",high="#8c2d04",name="Ratio") +
  theme(panel.border = element_rect(fill=NA,color='black'),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.1,0.2))+
  geom_text_repel(data=slolat, aes(long,lat,label=tolower(id),group=NULL))
child_sex_ratio_plot <- child_sex_ratio_plot +labs(title="Child Sex Ratio Per State")


grid.arrange(sex_ratio_plot,child_sex_ratio_plot,ncol=2)
#By keeping the plots side by side we see that states like assam,HP,west bengal have high child sex ratio 
#but low overall sex ratio. This might suggest some movement of females after adulthood into the previously white 
#states like Haryana, Gujrat,etc.

tot_city_pop <- ggplot()+geom_polygon(data=indMap.df,aes(x=long,y=lat,group=group),col="black",fill="white") +coord_fixed() +
  geom_point(data=cityDb,aes(x=location$lon,y=location$lat,size=literates_total),alpha=0.4,col="#33a02c")+
  theme_void()
tot_city_grads <- ggplot()+geom_polygon(data=indMap.df,aes(x=long,y=lat,group=group),col="black",fill="white") +coord_fixed() +
  geom_point(data=cityDb,aes(x=location$lon,y=location$lat,size=total_graduates),alpha=0.4,col="#8c2d04")+
  theme_void()
grid.arrange(tot_city_pop,tot_city_grads,ncol=2)

mf_lit_plot<-ggplot(data=stateDb,aes(map_id=id))+
  geom_map(aes(fill=stateDb$literates_male/stateDb$literates_female),map=indMap.df,col='white')+
  scale_fill_continuous(low = '#fee391',high = '#993404',na.value = 'grey',name="Ratio") +
  expand_limits(x=indMap.df$long,y=indMap.df$lat) +
  theme(panel.border = element_rect(fill=NA,color='black'),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.1,0.2)) +
  labs(title="Statewise Male-to-Female Literacy ratio Plot")

mf_grad_plot<- ggplot(data=stateDb,aes(map_id=id))+
  geom_map(aes(fill=stateDb$male_graduates/stateDb$female_graduates),map=indMap.df,col='white')+
  scale_fill_continuous(low = '#fee391',high = '#993404',na.value = 'grey',name="Ratio") +
  expand_limits(x=indMap.df$long,y=indMap.df$lat) +
  theme(panel.border = element_rect(fill=NA,color='black'),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.1,0.2)) +
  labs(title="Statewise Male-to-Female Graduates Ratio Plot")
fem_grad_plot <- ggplot(data=stateDb,aes(map_id=id))+
  geom_map(aes(fill=stateDb$female_graduates),map=indMap.df,col='white')+
  scale_fill_continuous(low = '#fee391',high = '#993404',na.value = 'grey',name="F graduates") +
  expand_limits(x=indMap.df$long,y=indMap.df$lat) +
  theme(panel.border = element_rect(fill=NA,color='black'),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.1,0.2)) +
  labs(title="Statewise Female Graduates Plot")
male_grad_plot<- ggplot(data=stateDb,aes(map_id=id))+
  geom_map(aes(fill=stateDb$male_graduates),map=indMap.df,col='white')+
  scale_fill_continuous(low = '#fee391',high = '#993404',na.value = 'grey',name="M graduates") +
  expand_limits(x=indMap.df$long,y=indMap.df$lat) +
  theme(panel.border = element_rect(fill=NA,color='black'),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.1,0.2)) +
  labs(title="Statewise Male Graduates Plot")
fem_lit_plot <- ggplot(data=stateDb,aes(map_id=id))+
  geom_map(aes(fill=stateDb$literates_female),map=indMap.df,col='white')+
  scale_fill_continuous(low = '#fee391',high = '#993404',na.value = 'grey',name="M/F graduates ratio") +
  expand_limits(x=indMap.df$long,y=indMap.df$lat) +
  theme(panel.border = element_rect(fill=NA,color='black'),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.1,0.2)) +
  labs(title="Statewise Female literates Plot")
male_lit_plot <- ggplot(data=stateDb,aes(map_id=id))+
  geom_map(aes(fill=stateDb$literates_male),map=indMap.df,col='white')+
  scale_fill_continuous(low = '#fee391',high = '#993404',na.value = 'grey',name="M literates") +
  expand_limits(x=indMap.df$long,y=indMap.df$lat) +
  theme(panel.border = element_rect(fill=NA,color='black'),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.1,0.2)) +
  labs(title="Statewise Male Literates Plot")
male_pop_plot<-ggplot(data=stateDb,aes(map_id=id))+
  geom_map(aes(fill=stateDb$population_male),map=indMap.df,col='white')+
  scale_fill_continuous(low = '#fee391',high = '#993404',na.value = 'grey',name="M Pop") +
  expand_limits(x=indMap.df$long,y=indMap.df$lat) +
  theme(panel.border = element_rect(fill=NA,color='black'),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.1,0.2)) +
  labs(title="Statewise Male Population Plot")
fem_pop_plot<-ggplot(data=stateDb,aes(map_id=id))+
  geom_map(aes(fill=stateDb$population_female),map=indMap.df,col='white')+
  scale_fill_continuous(low = '#fee391',high = '#993404',na.value = 'grey',name="F Pop") +
  expand_limits(x=indMap.df$long,y=indMap.df$lat) +
  theme(panel.border = element_rect(fill=NA,color='black'),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.1,0.2)) +
  labs(title="Statewise Female Population Plot")
grid.arrange(mf_lit_plot,mf_grad_plot,ncol=2)
  
#city_count_per_state = as.data.frame(table(cityDb$state_code))$Freq
ggplot(data=stateDb,aes(map_id=id))+
  geom_map(aes(fill=NumCities/5),
           map=indMap.df,col='white')+
  scale_fill_continuous(low = '#4575b4',high = '#d73027',na.value = '#a6cee3',name="% cities amongst 500") +
  expand_limits(x=indMap.df$long,y=indMap.df$lat) +theme_void()

ggplot(data=stateDb,aes(map_id=id))+
  geom_map(aes(fill=total_graduates/literates_total * 100),map=indMap.df,col='white')+
  scale_fill_continuous(low = '#4575b4',high = '#d73027',na.value = '#a6cee3',name="% graduates among literates") +
  expand_limits(x=indMap.df$long,y=indMap.df$lat) +  theme_void()
  

fem_lit_ratio <- ggplot(data=stateDb,aes(map_id=id))+
  geom_map(aes(fill=stateDb$literates_female/stateDb$literates_total),map=indMap.df,col="grey") +
  expand_limits(x=indMap.df$long,y=indMap.df$lat) + 
  scale_fill_continuous(low="#f1a340",high="#542788",name="Ratio") +
  theme(panel.border = element_rect(fill=NA,color='black'),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.1,0.2))+labs(title="Female literacy Ratio Per State")

total_pop <- ggplot(data=stateDb,aes(map_id=id))+
  geom_map(aes(fill=stateDb$population_female),map=indMap.df,col="grey") +
  expand_limits(x=indMap.df$long,y=indMap.df$lat) + 
  scale_fill_continuous(low="#f6e8c3",high="#01665e",name="Female Population") +
  theme(panel.border = element_rect(fill=NA,color='black'),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.1,0.2))+labs(title="Female Population Per State")

fem_grad <- ggplot(data=stateDb,aes(map_id=id))+
  geom_map(aes(fill=stateDb$female_graduates/stateDb$total_graduates),map=indMap.df,col="grey") +
  expand_limits(x=indMap.df$long,y=indMap.df$lat) + 
  scale_fill_continuous(low="#f1a340",high="#542788",name="Ratio") +
  theme(panel.border = element_rect(fill=NA,color='black'),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.1,0.2))+labs(title="female Graduates ratio Per State")

fem_child_pop <- ggplot(data=stateDb,aes(map_id=id))+
  geom_map(aes(fill=stateDb$X0.6_population_female),map=indMap.df,col="grey") +
  expand_limits(x=indMap.df$long,y=indMap.df$lat) + 
  scale_fill_continuous(low="#f6e8c3",high="#01665e",name="Population") +
  theme(panel.border = element_rect(fill=NA,color='black'),
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = c(0.1,0.2))+labs(title="Female Child Population Per State")

grid.arrange(total_pop,fem_child_pop,fem_grad,fem_lit_ratio,ncol=2,nrow=2)

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

#Top 3 Dashboard
#Top 3 states by population
library("ggrepel")
l<-stateDb[order(-stateDb$population_total),][0:3,1]
t3_state_pop <- cityDb[cityDb$state_name %in% l,]

is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
t3_state_pop %>%
  group_by(state_name) %>%
  mutate(outlier = ifelse(is_outlier(population_total),as.character(name_of_city),as.numeric(NA))) %>%
  ggplot(.,aes(x=state_name,y=log(population_total))) +geom_boxplot()+
  geom_text_repel(aes(label = outlier), na.rm = TRUE,angle=10,nudge_y =0.2)

t3_state_pop %>%
  group_by(state_name) %>%
  mutate(outlier = ifelse(is_outlier(population_total),as.character(name_of_city),as.numeric(NA))) %>%
  ggplot(.,aes(x=state_name,y=log10(population_total),fill=state_name)) +geom_violin()+
  geom_text_repel(aes(label = outlier), na.rm = TRUE,angle=10,nudge_y =0.2,nudge_x = 0.2,col="black")+
  scale_fill_brewer(type='qual',palette=3)+
  theme(panel.background = element_rect(fill="#ededf0"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour ="black"),
        legend.position = "none")

require(plotrix)
t3_pop_list <- stateDb[order(-stateDb$population_total),][0:5,c(5,1)]
#t3_pop_list[6] = sum(stateDb$population_total,na.rm = TRUE) - sum(t3_pop_list)
pie3D(t3_pop_list[,1], labels = t3_pop_list[,2], 
      main = "Top 5 Population-Wise", explode=0.1, radius=.9, labelcex = 1.2,  start=0.7)
t5_lit_list <- stateDb[order(-stateDb$literates_total),][0:5,c(8,1)]
pie3D(t5_lit_list[,1], labels = as.list(paste(t5_lit_list[,2],paste(round(t5_lit_list[,1]/sum(stateDb$literates_total,na.rm=TRUE)*100),c("%","%","%","%","%")))), 
      main = "Top 5 Literates-Wise", explode=0.1, radius=.9, labelcex = 1.2,  start=0.7)

t5_grad_list <- stateDb[order(-stateDb$total_graduates),][0:5,c(11,1)]
pie3D(t5_grad_list[,1], labels = as.list(paste(t5_grad_list[,2],
                                               paste(round(t5_grad_list[,1]/sum(stateDb$total_graduates,na.rm=TRUE)*100),
                                                     c("%","%","%","%","%")))), 
      main = "Top 5 Graduates-Wise", explode=0.1, radius=.9, labelcex = 1.2,  start=0.7)

