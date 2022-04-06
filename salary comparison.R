library(tidyverse)
library(readxl)
library(lubridate)
currency <- read_excel("data/salary_data.xlsx", sheet = "currency")
salary <- read_excel("data/salary_data.xlsx", sheet = "Sayfa1")

# wrangle data
data<-salary %>%
  inner_join(currency, by = "term") %>%
  separate(term, into = c("year", "month")) %>%
  rename(worked_day=day) %>% 
  mutate(
    date = make_date(year = year, month = month, day = 1),
    slry_tl = salary / worked_day * 30,
    slry_usd = slry_tl / currency,
    slry_2022_tl = slry_usd * 14.8
  ) 

# monthly salary in usd compared to recent salary
data %>% 
  ggplot() +
    geom_line(aes(x = date, y = slry_usd))+
    scale_x_date(date_breaks = "1 year",date_labels = "%m-%Y")+ 
    geom_hline(yintercept=18000/14.8, linetype="dashed", color = "red")+
    scale_y_continuous(labels=scales::dollar_format())+ 
    geom_label(
      data = data.frame(
        x = make_date(year = 2013, month = 3, day = 1), 
        y = 18000/14.8, 
        value = scales::dollar(round(18000/14.8,0))
      ), 
      aes(x=x,y=y,label = value),
      color = "red"
      )+
    annotate(
      "text", 
      x = make_date(year = 2013, month = 3, day = 1), 
      y = 1600, 
      label = "Recent Monthly Salary",
      col="red")+
    ggtitle("Monthly Salary in USD by Years Compared to the Recent One") +
    ylab("Monthly Salary") + 
    xlab("Year")

# yearly salary in usd compared to recent salary
data %>%
  group_by(year) %>%
  summarise(annual_income = sum(slry_usd)) %>%
  add_row(year = "2022", annual_income=18000/14.8*12) %>% 
  ggplot(aes(x=year)) +
    geom_col(aes(y=ifelse(year=="2022",0,annual_income)))+
    geom_col(aes(y=ifelse(year!="2022",0,annual_income)),fill="red")+
    scale_y_continuous(labels=scales::dollar_format())+ 
    ggtitle("Annual Income in USD compared to Recent One") +
    ylab("Annual Income") + 
    xlab("Year")

