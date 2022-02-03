athletes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-03/athletes.csv')
library(tidyverse)



events<-athletes%>%
  mutate(gen=coalesce(gender,event),#event has gender data when gender is NA
         year=as.factor(year))%>% 
  distinct(event,gen,year)%>% 
  group_by(year,gen)%>%
  summarise(n=n())%>%
  pivot_wider(names_from = gen, values_from = n)%>%
  mutate(diff=Women-Men)

ggplot(events,(aes(x=year,y=600)))+
  geom_segment(aes(x=year,xend=year,y=Women,yend=Men),colour="gray42",linetype="twodash",size=1)+
  geom_point(aes(y=Men),shape=21,fill="white",stroke=2,colour="#005AB5",size=3)+
  geom_point(aes(y=Women),shape=21,fill="white",stroke=2,colour="#DC3220",size=3)+
  geom_text(aes(x=year,y=(Men+Women)/2,label=diff),nudge_x = 0.38,colour="red",fontface="bold",family="serif")+
  theme_classic()+
  labs(title="Men/Women participation",x="Year",y="Number of events",caption = "Data from the International Paralympic Committee.")+
  theme(text = element_text(size=15))+
  #manual legend
  geom_point(x=8,y=470,shape=21,fill="white",stroke=2,colour="#005AB5",size=3)+
  geom_point(x=8,y=445,shape=21,fill="white",stroke=2,colour="#DC3220",size=3)+
  geom_text(x=8.5,y=470,label="Men",family = "serif")+
  geom_text(x=8.6,y=445, label="Women",family = "serif")

# second part

athletes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-03/athletes.csv')
library(tidyverse)

events2<-athletes%>%
  distinct(event,type,year)%>%
  group_by(type,year)%>%
  summarise(n=n())%>%
  mutate(year=as.factor(year))%>%
  arrange(desc(type))

e3<-events2%>%
  group_by(type)%>%
  summarise(value=max(n))

e<-inner_join(events2,e3)%>%
  mutate(diff=(value-n)/value)

ggplot(e,aes(x=year,y=type,fill=diff))+
  geom_point(shape=22,size=11,show.legend = FALSE,alpha=0.8)+
  scale_fill_continuous(low = "#132B43", high = "#56B1F7")+
  geom_text(aes(label=n),size=4,colour="black",fontface="bold")+
  theme_classic()+
  labs(x="Year",y="Event type",caption = "Data from the International Paralympic Committee.")+
  theme(text = element_text(size=15))
