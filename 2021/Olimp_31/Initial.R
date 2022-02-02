library(tidyverse)
devtools::install_github("rensa/ggflags")
library(ggflags)

olympics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-07-27/olympics.csv')

geom_curve2 <- function(..., curvature = 0.2) {
  geom_curve(curvature = curvature, size = 0.03,
             arrow = arrow(length = unit(1.0, "mm"), type = "closed"),
             ...) 
} 


olyears<-c("1896","1900", "1904","1906", "1908", "1912" ,"1920" ,"1924", "1928", "1932", "1936", "1948" ,"1952" ,"1956" ,"1960" ,"1964" ,"1968", "1972" ,"1976", "1980", "1984", "1988" ,"1992", "1996", "2000", "2004", "2008" ,"2012", "2016")
medalss<-c("Gold","Silver","Bronze")

fillblanks<-expand.grid(olyears,medalss)%>%
  mutate(medal=as.factor(Var2),
         year=as.factor(Var1),
         Freq1=0,)%>%
  select(year,medal,Freq1)

#gbr

gbr<-filter(olympics, noc=="GBR",season=="Summer", medal!="NA")%>%
  #to make team sports cound as a single medal
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

geom_curve2 <- function(..., curvature = 0.2) {
  geom_curve(curvature = curvature, size = 0.03,
             arrow = arrow(length = unit(1.0, "mm"), type = "closed"),
             ...) 
} 

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

#usa

usa<-filter(olympics, noc=="USA",season=="Summer", medal!="NA")%>%
  #to make team sports cound as a single medal
  distinct(year,event,.keep_all=TRUE)%>%
  group_by(year, medal)%>%
  summarise(Freq=n())%>%
  mutate(year=as.factor(year),medal=as.factor(medal))

usa2<-full_join(usa,fillblanks,by=c("year","medal"))%>%
  mutate(Freq=replace_na(Freq,0),
         Freq1=replace_na(Freq1,0),
         count=(Freq+Freq1))%>%
    select(year,medal,count)%>%
  as_tibble()%>%
 # slice(-87,-90,-85)%>%
  mutate(year=factor(year,levels = c("1896","1900", "1904","1906", "1908", "1912" ,"1920" ,"1924", "1928", "1932", "1936", "1948" ,"1952" ,"1956" ,"1960" ,"1964" ,"1968", "1972" ,"1976", "1980", "1984", "1988" ,"1992", "1996", "2000", "2004", "2008" ,"2012", "2016")))%>%
  drop_na()
 
  
usa2$medal<-factor(usa2$medal,levels = c("Gold","Silver","Bronze"))
View(usa2)

ggplot(usa2,aes(x=year,y=count))+
  geom_col(aes(fill=medal),position="stack",colour="black")+
  scale_fill_manual(values = c("#FFD700","#C0C0C0","#CD7F32" ))+
  theme_classic()+
  labs(x="Year",y="Medals",subtitle="USA Olympic Medals/Year")+
  #ggflags::geom_flag(y=135,x=1.5,country="us",size=10)+
  theme(text=element_text(size=18),
        axis.text.x=element_text(angle=60,hjust=1),
        legend.position = "none")+
  geom_curve2(aes(x = 4.5, xend = 3, y = 95, yend = 85), color = "black", curvature = 0.3)+
  annotate("text",x = 5.9, y = 95,label="St. Louis", color = "black",size=5)+
  geom_curve2(aes(x = 12, xend = 9, y = 80, yend = 73), color = "black", curvature = -0.1)+
  annotate("text",x = 13, y = 85 ,label="Los Angeles I", color = "black",size=5)+
  geom_curve2(aes(x = 18, xend = 20, y = 125, yend = 135), color = "black", curvature = 0.3)+
  annotate("text",x = 15.5, y = 126 ,label="Los Angeles II", color = "black",size=5)+
  geom_curve2(aes(x = 23.5, xend = 23, y = 110, yend = 85), color = "black", curvature = -0.3)+
  annotate("text",x = 23.5, y = 115 ,label="Atlanta", color = "black",size=5)+
  geom_segment(aes(x=18.6,y=5,xend=19.4,yend=5),size=3,colour="red")
  

#china

chn<-filter(olympics, noc=="CHN",season=="Summer", medal!="NA")%>%
 #to make team sports count as a single medal
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

geom_curve2 <- function(..., curvature = 0.2) {
  geom_curve(curvature = curvature, size = 0.03,
             arrow = arrow(length = unit(1.0, "mm"), type = "closed"),
             ...) 
} 

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
  geom_curve2(aes(x = 23, xend = 26, y = 75, yend = 80), color = "black", curvature = 0.3)+
  annotate("text",x = 21.5, y = 76,label="Beijing", color = "black",size=5)
 
#greece


gre<-filter(olympics, noc=="GRE",season=="Summer", medal!="NA")%>%
  #to make team sports cound as a single medal
  distinct(year,event,.keep_all=TRUE)%>%
  group_by(year, medal)%>%
  summarise(Freq=n())%>%
  mutate(year=as.factor(year),medal=as.factor(medal))

view(gre)

gre2<-full_join(fillblanks,gre,by=c("year","medal"))%>%
  mutate(Freq=replace_na(Freq,0),
         Freq1=replace_na(Freq1,0),
         count=(Freq+Freq1))%>%
  slice(-29,-58,-87)%>%
  select(year,medal,count)


gre2$year<-factor(gre2$year,levels=C("1896","1900", "1904","1906", "1908", "1912" ,"1920" ,"1924", "1928", "1932", "1936", "1948" ,"1952" ,"1956" ,"1960" ,"1964" ,"1968", "1972" ,"1976", "1980", "1984", "1988" ,"1992", "1996", "2000", "2004", "2008" ,"2012", "2016 "))
gre2$medal<-factor(gre2$medal,levels = c("Gold","Silver","Bronze"))
levels(gre2$year)

View(gre2)
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



 