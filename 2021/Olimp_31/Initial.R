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

