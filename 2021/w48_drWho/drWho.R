install.packages("datardis")
library(datardis)
library(tidyverse)

geom_curve2 <- function(..., curvature = 0.2) {
  geom_curve(curvature = curvature, size = 0.03,
             arrow = arrow(length = unit(1.0, "mm"), type = "closed"),
             ...) 
}

#Moffat vs Russell

MvsR<-left_join(writers,episodes)%>%
  filter(writer=="Russell T Davies"|writer=="Steven Moffat")%>%
  filter(!is.na(rating))

ggplot(MvsR,aes(x=writer,y=rating,colour=rating))+
  geom_point(position = position_jitter(height = 0,width = 0.3,seed=129),alpha=0.4,size=5)+
  scale_y_continuous(limits = c(75,95),breaks = seq(75,95,5))+
  theme_classic()+
  theme(legend.position = "none",
        text = element_text(size=16))+
  stat_summary(fun = mean,
               geom = "errorbar",
               aes(ymax = ..y.., ymin = ..y..),
               width = 0.7,
               size=2,
               colour="#003b6f",
               alpha=0.5)+
  geom_curve2(aes(x = 0.7, xend = 0.72, y = 93, yend =91.1), color = "black", curvature = -0.3)+
  annotate("text",x = 0.7, y = 93.5,label="The stolen Earth", color = "black",size=3)+
  geom_curve2(aes(x = 1.2, xend = 1.1, y = 92, yend =91.1), color = "black", curvature = -0.2)+
  annotate("text",x = 1.2, y = 92.3,label="Journey's End", color = "black",size=3)+
  geom_curve2(aes(x = 1.6, xend = 1.72, y = 90, yend =89.1), color = "black", curvature = -0.3)+
  annotate("text",x = 1.5, y = 90.5,label="Silence in the Library", color = "black",size=3)+
  geom_curve2(aes(x = 1.8, xend = 1.87, y = 92, yend =89.1), color = "black", curvature = 0.1)+
  annotate("text",x = 1.80, y = 92.5,label="The Big Bang", color = "black",size=3)+
  geom_curve2(aes(x = 2.1, xend = 2.18, y = 93, yend =89.1), color = "black", curvature = 0.1)+
  annotate("text",x = 2.1, y = 94.1,label="Asylum of the Daleks", color = "black",size=3)+
  annotate("text",x = 2.1, y = 93.5,label="Forest of the Dead", color = "black",size=3)+
  labs(y="Ratings",x="Writer",caption=expression(atop("Russel T Davies (31 episodes) vs S. Moffat (48 episodes)")))

ggsave("2021/drWho_48/plots/drwho.jpg")
