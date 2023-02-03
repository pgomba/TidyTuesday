cats_uk_reference <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-31/cats_uk_reference.csv')

library(tidyverse)
library(ggimage)
library(ggrepel)

df<-cats_uk_reference%>%
  mutate(kill_ratio=(prey_p_month/30)/(24-hrs_indoors))%>% #number of kills per hour outdoor
  select(animal_id,kill_ratio,animal_sex)%>%
  arrange(desc(kill_ratio))

df$image<-"https://raw.githubusercontent.com/pgomba/TidyTuesday/main/2023/w5_cats/PngItem_2134337.png"

set.seed(120)
df$y<-runif(101,-1,1) #jitter rats positions

caption<-"Source: doi:10.1111/acv.12563 \ndoi:10.5441/001/1.pf315732"

ggplot(df, aes(x=kill_ratio,y=y))+
  geom_image(aes(image=image,color=animal_sex), size=.09)+
  geom_text_repel(data=df%>%head(10),aes(label=animal_id),color="white",bg.color = "grey30",bg.r = 0.1,nudge_x = .01)+
  labs(x="Kills /per hour outdoor",y="",title = "UK Outdoor cats killing efficiency",
       caption=caption,color="Sex",
       subtitle = "#TidyTuesday. 2023 Week 5. @pagomba")+
  theme_classic()+
  scale_x_continuous(limits = c(0,0.09))+
  theme(text=element_text(size=18),
        plot.title = element_text(size=14))+
  theme(axis.line.x = element_line(color="white",size = 2),
        axis.line.y = element_blank(),
        axis.text.x = element_text(color="white"),
        axis.text.y = element_blank(),
        plot.title = element_text(color="white"),
        plot.caption=element_text(color="white"),
        legend.position = c(.90, .90),
        legend.background = element_rect(fill = "#272822"),
        text=element_text(size=16,color = "white"),
        plot.subtitle = element_text(size=12,color = "white"),
        panel.background = element_rect(fill = "#272822"),
        plot.background = element_rect(fill = "#272822"))
 
ggsave("2023/w5_cats/cats_uk.png")
