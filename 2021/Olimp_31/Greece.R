source("2021/Olimp_31/Initial.R")

#Greece

gre<-filter(olympics, noc=="GRE",season=="Summer", medal!="NA")%>%
  #to make team sports cound as a single medal
  distinct(year,event,.keep_all=TRUE)%>%
  group_by(year, medal)%>%
  summarise(Freq=n())%>%
  mutate(year=as.factor(year),medal=as.factor(medal))

gre2<-full_join(fillblanks,gre,by=c("year","medal"))%>%
  mutate(Freq=replace_na(Freq,0),
         Freq1=replace_na(Freq1,0),
         count=(Freq+Freq1))%>%
  slice(-29,-58,-87)%>%
  select(year,medal,count)


gre2$medal<-factor(gre2$medal,levels = c("Gold","Silver","Bronze"))

ggplot(gre2,aes(x=year,y=count))+
  geom_col(aes(fill=medal),position="stack",colour="black")+
  scale_fill_manual(values = c("#FFD700","#C0C0C0","#CD7F32" ))+
  theme_classic()+
  labs(x="Year",y="Medals",subtitle="Greece Olympic Medals/Year")+
  ggflags::geom_flag(y=29,x=10,country="gr",size=10)+
  theme(text=element_text(size=18),
        axis.text.x=element_text(angle=60,hjust=1),
        legend.position = "none")+
  geom_curve2(aes(x = 3, xend = 1, y = 29, yend = 28), color = "black", curvature = 0.3)+
  annotate("text",x = 4.5, y = 29,label="Athens", color = "black",size=5)+
  geom_curve2(aes(x = 6, xend = 4, y = 21, yend = 23), color = "black", curvature = -0.3)+
  annotate("text",x = 11.1, y = 21,label="Athens Intercalated Games", color = "black",size=5)+
  geom_curve2(aes(x = 23, xend = 26, y =17 , yend = 15), color = "black", curvature = 0.3)+
  annotate("text",x = 22, y = 18,label="Athens", color = "black",size=5)

ggsave("2021/Olimp_31/plots/greece.jpeg")
