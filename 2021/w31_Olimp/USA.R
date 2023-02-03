source("2021/Olimp_31/Initial.R")

#USA

usa<-filter(olympics, noc=="USA",season=="Summer", medal!="NA")%>%
  distinct(year,event,.keep_all=TRUE)%>% #to make team sports cound as a single medal
  group_by(year, medal)%>%
  summarise(Freq=n())%>%
  mutate(year=as.factor(year),medal=as.factor(medal))

usa2<-full_join(usa,fillblanks,by=c("year","medal"))%>%
  mutate(Freq=replace_na(Freq,0),
         Freq1=replace_na(Freq1,0),
         count=(Freq+Freq1))%>%
  select(year,medal,count)%>%
  as_tibble()%>%
  mutate(year=factor(year,levels = c("1896","1900", "1904","1906", "1908", "1912" ,"1920" ,"1924", "1928", "1932", "1936", "1948" ,"1952" ,"1956" ,"1960" ,"1964" ,"1968", "1972" ,"1976", "1980", "1984", "1988" ,"1992", "1996", "2000", "2004", "2008" ,"2012", "2016")))%>%
  drop_na()


usa2$medal<-factor(usa2$medal,levels = c("Gold","Silver","Bronze"))

ggplot(usa2,aes(x=year,y=count))+
  geom_col(aes(fill=medal),position="stack",colour="black")+
  scale_fill_manual(values = c("#FFD700","#C0C0C0","#CD7F32" ))+
  theme_classic()+
  labs(x="Year",y="Medals",subtitle="USA Olympic Medals/Year")+
  ggflags::geom_flag(y=135,x=1.5,country="us",size=10)+
  theme(text=element_text(size=18),
        axis.text.x=element_text(angle=60,hjust=1),
        legend.position = "none")+
  geom_curve2(aes(x = 4.5, xend = 3, y = 95, yend = 85), color = "black", curvature = 0.3)+
  annotate("text",x = 5.9, y = 95,label="St. Louis", color = "black",size=5)+
  geom_curve2(aes(x = 12, xend = 9.9, y = 80, yend = 73), color = "black", curvature = -0.1)+
  annotate("text",x = 13, y = 85 ,label="Los Angeles I", color = "black",size=5)+
  geom_curve2(aes(x = 18, xend = 21, y = 125, yend = 135), color = "black", curvature = 0.3)+
  annotate("text",x = 15.5, y = 126 ,label="Los Angeles II", color = "black",size=5)+
  geom_curve2(aes(x = 23.5, xend = 24, y = 110, yend = 85), color = "black", curvature = -0.3)+
  annotate("text",x = 23.5, y = 115 ,label="Atlanta", color = "black",size=5)+
  geom_segment(aes(x=19.6,y=5,xend=20.4,yend=5),size=3,colour="red")

ggsave("2021/Olimp_31/plots/usa_medals.jpeg")
