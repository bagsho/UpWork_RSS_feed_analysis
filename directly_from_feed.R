library(tidyverse)
library(tidyRSS)
feed <- "https://www.upwork.com/ab/feed/topics/rss?securityToken=94cfe3c62750ff33d53312e9c4bf275dc63708eaaa90d1a1c287b00c5ca063a02740ec85c96e33f73aa747df5f4abf38d94fa74bcbe2296f1790c78e41d70dbd&userUid=1270287403451940864&orgUid=1270287403451940866&topic=most-recent"
feeds <- tidyfeed(feed)
data <- feeds %>% 
  
  # select necessary fields
  select(item_pub_date, item_title, item_description) %>%
 
  # edit item_title
  mutate(
    item_title = str_replace(item_title, " - Upwork", ""),
    item_title = str_trim(item_title, side = "right")
  )%>%

  # hourly range
  mutate(temp = item_description) %>%
  separate(col = temp, sep = "Hourly Range: ", into = c("Hourly_Range", "post")) %>%
  separate(col = post, sep = "Posted On:", into = c("Hourly_Range", "post")) %>%
  select(-post) %>%

  # Budget
  mutate(temp = item_description) %>%
  separate(col = temp, sep = "Budget: ", into = c("Budget", "post")) %>%
  separate(col = post, sep = "Posted On:", into = c("Budget", "post")) %>%
  select(-post) %>%

  # Category
  mutate(temp = item_description) %>%
  separate(col = temp, sep = "Category: ", into = c("Category", "post")) %>%
  separate(col = post, sep = "Skills:", into = c("Category", "post")) %>%
  mutate(Category = str_replace(Category, "&amp;", "&")) %>%
  select(-post) %>%

  # Country
  mutate(temp = item_description) %>%
  separate(col = temp, sep = "Country: ", into = c("Country", "post")) %>%
  separate(col = post, sep = "click to apply", into = c("Country", "post")) %>%
  select(-post) %>%

  # skills
  mutate(temp = item_description) %>%
  separate(col = temp, sep = "Skills:", into = c("Skills", "post")) %>%
  separate(col = post, sep = "Country: ", into = c("Skills", "post")) %>%
  mutate(Skills = str_split(Skills, ",")) %>%
  select(-post) %>%

  # date and time
  mutate(
    date = lubridate::as_date(item_pub_date),
    time = hms::as_hms(item_pub_date)
  ) %>%
  relocate(date:time) %>%
  select(-item_pub_date)
