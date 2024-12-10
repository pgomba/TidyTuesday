library(tidyverse)
library(viridis)
library(showtext)

showtext_auto()
font_add_google("Roboto", "roboto")

parfumo_data_clean <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-12-10/parfumo_data_clean.csv')

perfume<-parfumo_data_clean%>%
  drop_na(Main_Accords)%>%
  drop_na(Release_Year)

test<-perfume %>%
  mutate(Main_Accords= gsub(",","",Main_Accords))%>%
  separate_rows(.,Main_Accords, sep = "\\s+") %>% # Split words into separate rows
  group_by(Release_Year, Main_Accords) %>%             # Group by year and word
  summarise(frequency = n(), .groups = "drop") %>% # Count occurrences
  arrange(Release_Year, desc(frequency))%>%
  filter(Release_Year>=2000)%>%
  filter(Main_Accords!="Chypre")


ggplot(test,aes(x=Release_Year,y=frequency))+
  geom_col(aes(fill=frequency),colour="black")+
  scale_fill_viridis(breaks = c(0, 200,500,800),option = "turbo")+
  facet_wrap(Main_Accords~.,
             strip.position = "bottom")+
  scale_x_continuous(breaks = seq(2000,2024,4))+
  theme_void()+
  theme(plot.background =  element_rect(fill="#191919"),
        text = element_text(family = "roboto",colour="white",size=80),
        legend.direction = "horizontal",
        legend.position = "bottom",
        axis.text.x = element_text(angle=90,colour="white",size=50,hjust=-2),
        legend.text = element_text(angle=45,size=40)
        
        )+
  labs(fill="Frequency",
       subtitle = " ",
       title = "Fragrance Accord Release Trends (2000–2024)",
       caption = "#tidyTuesday |week 50, 2024 | @pagomba")

ggsave("perfumes.png",dpi="retina",width = 16.9,height = 9.53)  
