chn<-filter(olympics, noc=="CHN",season=="Summer", medal!="NA")%>%
  distinct(year,event,.keep_all=TRUE)%>%
  group_by(year, medal)%>%
  summarise(Freq=n())%>%
  mutate(year=as.factor(year),medal=as.factor(medal))

chn2<-full_join(fillblanks,chn,by=c("year","medal"))%>%
  mutate(Freq=replace_na(Freq,0),
         Freq1=replace_na(Freq1,0),
         count=(Freq+Freq1))%>%
  slice(-28,-56,-84)%>%
  select(year,medal,count)

chn2$medal<-factor(chn2$medal,levels = c("Gold","Silver","Bronze"))

ggplot(chn2,aes(x=year,y=count))+
  geom_col(aes(fill=medal),position="stack",colour="black")+
  scale_fill_manual(values = c("#FFD700","#C0C0C0","#CD7F32" ))+
  theme_classic()+
  labs(x="Year",y="Medals",title="China Olympic Medals/Year")+
  ggflags::geom_flag(y=75,x=1.5,country="cn",size=10)+
  theme(text=element_text(size=18),
        axis.text.x=element_text(angle=60,hjust=1),
        legend.position = "none")+
  geom_segment(aes(x=1,y=5,xend=8,yend=5),size=4,colour="red")+
  geom_segment(aes(x=12,y=5,xend=19,yend=5),size=4,colour="red")+
  geom_curve2(aes(x = 23, xend = 27, y = 75, yend = 60), color = "black", curvature = 0.3)+
  annotate("text",x = 21.5, y = 76,label="Beijing", color = "black",size=5)

ggsave("2021/Olimp_31/plots/china_medals.jpeg")
