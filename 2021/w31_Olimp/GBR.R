source("2021/Olimp_31/Initial.R")

#GBR

gbr<-filter(olympics, noc=="GBR",season=="Summer", medal!="NA")%>% #to make team sports cound as a single medal
  distinct(year,event,.keep_all=TRUE)%>% 
  group_by(year, medal)%>%
  summarise(Freq=n())%>%
  mutate(year=as.factor(year),medal=as.factor(medal))

gbr2<-full_join(gbr,fillblanks,by=c("year","medal"))%>%
  mutate(Freq=replace_na(Freq,0),
         Freq1=replace_na(Freq1,0),
         count=(Freq+Freq1))%>%
  slice(-28,-56,-84)%>%
  select(year,medal,count)
gbr2$medal<-factor(gbr2$medal,levels = c("Gold","Silver","Bronze"))

ggplot(gbr2,aes(x=year,y=count))+
  geom_col(aes(fill=medal),position="stack",colour="black")+
  scale_fill_manual(values = c("#FFD700","#C0C0C0","#CD7F32" ))+
  theme_classic()+
  labs(x="Year",y="Medals",subtitle="GB Olympic Medals/Year")+
  ggflags::geom_flag(y=78,x=1.5,country="gb",size=10)+
  theme(text=element_text(size=18),
        axis.text.x=element_text(angle=60,hjust=1),
        legend.position = "none")+
  geom_curve2(aes(x = 7, xend = 5, y = 70, yend = 75), color = "black", curvature = 0.3)+
  annotate("text",x = 8, y = 68,label="London 1908", color = "black",size=5)+
  geom_curve2(aes(x = 13, xend = 12, y = 35, yend = 25), color = "black", curvature = -0.3)+
  annotate("text",x = 13.5, y = 38 ,label="London 1948", color = "black",size=5)+
  geom_curve2(aes(x = 25, xend = 28, y = 60, yend = 55), color = "black", curvature = -0.3)+
  annotate("text",x = 22.5, y = 60 ,label="London 2012", color = "black",size=5)

ggsave("2021/Olimp_31/plots/gbr.jpeg")
