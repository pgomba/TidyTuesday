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
  labs(title= "Events per year",x="Year",y="Event type",caption = "Data from the International Paralympic Committee.")+
  theme(text = element_text(size=15))

ggsave("2021/Para_32/plots/events.jpg")
