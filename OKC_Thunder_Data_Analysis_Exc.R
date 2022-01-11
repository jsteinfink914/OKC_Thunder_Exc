library(ggplot2)
library(dplyr)
setwd("C:/Users/jstei/Desktop")
DF<-read.csv('shots_data.csv',header=T)
str(DF)
DF$team<-as.factor(DF$team)
##First, shot zones must be determined
##Three categories are 2pt C3 and non C3
##2 pt is any non-three point shot
##C3 is where Y<=abs(7.8) and x>abs(22)
##NC3 is where y>7.8 and sqrt(x^2+y^2)>=23.75

##Defining labels
for (i in 1:nrow(DF)){
  if ( (abs(DF[i,'y'])<=7.8) & (abs(DF[i,'x'])>=22)){
    DF[i,'ShotZone']<-'C3'
  }
  else if( (abs(DF[i,'y'])>7.8) & sqrt((DF[i,'x'])^2 + (DF[i,'y'])^2)>=23.75){
    DF[i,'ShotZone']<-'NC3'
  }
  else{
    DF[i,'ShotZone']<-'2PT'
  }
}
##Using dplyr to group shots based on zones and creating proportions (Team A and B)
ShotDistributionA<-DF[DF$team=='Team A',] %>%
  group_by(ShotZone)%>%
  summarise(freq=n())%>%
  ungroup() %>%
  mutate(total=sum(freq),prop=freq/total)

ShotDistributionB<-DF[DF$team=='Team B',] %>%
  group_by(ShotZone)%>%
  summarise(freq=n())%>%
  ungroup() %>%
  mutate(total=sum(freq),prop=freq/total)

##Visualizing Shot Distribution using ggplot library
##Team A first
ggplot(data=ShotDistributionA,aes(x=ShotZone,y=prop))+
  geom_bar(stat='identity',fill='blue')+
  geom_text(aes(label=scales::percent(prop)),position=position_stack(vjust=.5),colour='white')+
  ggtitle('Shot attempt Distribution by Shot Zone (Team A)')
##Team B
ggplot(data=ShotDistributionB,aes(x=ShotZone,y=prop))+
  geom_bar(stat='identity',fill='blue')+
  geom_text(aes(label=scales::percent(prop)),position=position_stack(vjust=.5),colour='white')+
  ggtitle('Shot attempt Distribution by Shot Zone (Team B)')

##Using dplyr to group data- team A first
efgA<-DF[DF$team=='Team A',] %>%
  group_by(ShotZone)%>%
  summarise(freq=n())%>%
  ungroup()
##Converting to dataframe
efgA<-as.data.frame(efgA)
##Assigning appropriate efg% to the dataframe
efgA[1,'efg']<-(sum(DF$fgmade[DF$ShotZone=='2PT' & DF$team=='Team A'])+.5*(0))/efgA[1,"freq"]
efgA[2,'efg']<-(1.5*sum(DF$fgmade[DF$ShotZone=='C3'& DF$team=='Team A']))/efgA[2,'freq']
efgA[3,'efg']<-(1.5*sum(DF$fgmade[DF$ShotZone=='NC3' & DF$team=='Team A']))/efgA[3,'freq']

##Now team B
efgB<-DF[DF$team=='Team B',] %>%
  group_by(ShotZone)%>%
  summarise(freq=n())%>%
  ungroup()
##Converting to dataframe
efgB<-as.data.frame(efgB)
##Assigning appropriate efg% to the dataframe
efgB[1,'efg']<-(sum(DF$fgmade[DF$ShotZone=='2PT' & DF$team=='Team B'])+.5*(0))/efgB[1,"freq"]
efgB[2,'efg']<-(1.5*sum(DF$fgmade[DF$ShotZone=='C3'& DF$team=='Team B']))/efgB[2,'freq']
efgB[3,'efg']<-(1.5*sum(DF$fgmade[DF$ShotZone=='NC3' & DF$team=='Team B']))/efgB[3,'freq']
##Visualizing efg% (Team A)
ggplot(data=efgA,aes(x=ShotZone,y=efg))+
  geom_bar(fill='orange',stat='identity')+
  geom_text(aes(label=scales::percent(efg)),position=position_stack(vjust=.5),colour='black')+
  ggtitle('eFG% by Shot Zone (Team A)')+
  ylab('eFG%')

 ##Visualizing efg% (Team B) 
ggplot(data=efgB,aes(x=ShotZone,y=efg))+
  geom_bar(fill='orange',stat='identity')+
  geom_text(aes(label=scales::percent(efg)),position=position_stack(vjust=.5),colour='black')+
  ggtitle('eFG% by Shot Zone (Team B)')+
  ylab('eFG%')
  
