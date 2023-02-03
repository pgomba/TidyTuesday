source("2021/Olimp_31/Initial.R")

#Spain

spa<-filter(olympics, noc=="ESP",season=="Summer", medal!="NA")%>%
  distinct(year,event,.keep_all=TRUE)%>%#to make team sports count as a single medal
  group_by(year, medal)%>%
  summarise(Freq=n())%>%
  mutate(year=as.factor(year),medal=as.factor(medal))

spa2<-full_join(fillblanks,spa,by=c("year","medal"))%>%
  mutate(Freq=replace_na(Freq,0),
         Freq1=replace_na(Freq1,0),
         count=(Freq+Freq1))%>%
  slice(-28,-56,-84)%>%
  select(year,medal,count)

spa2$medal<-factor(spa2$medal,levels = c("Gold","Silver","Bronze"))

ggplot(spa2,aes(x=year,y=count))+
  geom_col(aes(fill=medal),position="stack",colour="black",key_glyph = draw_key_point)+
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 8)))+
  scale_fill_manual(values = c("#FFD700","#C0C0C0","#CD7F32" ))+
  theme_classic()+
  labs(x="Year",y="Medals",subtitle="Spain Olympic Medals/Year")+
  #ggflags::geom_flag(y=20,x=1.5,country="es",size=10)+
  theme(text=element_text(size=18),
        axis.text.x=element_text(angle=60,hjust=1),
        legend.title = element_blank())+
  geom_curve2(aes(x = 20, xend = 23, y = 21, yend = 20), color = "black", curvature = -0.3)+
  annotate("text",x = 18, y = 21,label="Barcelona", color = "black",size=5)+
  geom_segment(aes(x=2.8,y=0.5,xend=6.2,yend=0.5),size=3,colour="red")+
  geom_segment(aes(x=7.8,y=0.5,xend=8.2,yend=0.5),size=3,colour="red")+
  theme(legend.position = c(.25,.75),legend.direction="vertical")+
  annotate("text",x = 10, y = 12.5,label="No participation", color = "black",size=5)+
  geom_segment(aes(x=6,y=12.3,xend=6.5,yend=12.3),size=3,colour="red")

ggsave("2021/Olimp_31/plots/spain_medals.jpeg")
