library(tidyverse)
library(readxl)

folder_path<-"C:/Users/user/Desktop/herşey/R_projects/UpWork_RSS_feed_analysis/data/feeder logs"

files<-dir(folder_path)%>%
  map(~paste("data/feeder logs/",.x,sep="")) %>% 
  map(read_excel) %>% 
  reduce(rbind) %>% 
  
  # select necessary fields and unique jobs
  group_by(Published) %>% 
  summarise(count=n()) %>% 
  select(-count) %>% 

  # date and time
  mutate(
    date = lubridate::as_date(Published),
    time = hms::as_hms(Published)
  ) 

files %>% 
  ggplot()+
    geom_histogram(aes(x=Published))
