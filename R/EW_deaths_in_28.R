library(tidyverse)
library(here)
library(lubridate)

EW_deaths_in_28 <- read_csv(here('data','data_2022-Jan-22.csv')) %>% 
  filter(areaName %in% c("England","Wales")) %>%
  select(areaName,date,newDeaths28DaysByDeathDate) %>% 
  mutate(year=year(date),quarter=sprintf("%d-Q%d",year(date),quarter(date)),) %>%
  rename(deaths=newDeaths28DaysByDeathDate) %>% 
  filter(quarter < "2021-Q4")


EW_deaths_in_28 %>% 
  group_by(quarter) %>%
  summarise(deaths=sum(deaths)) %>% 
  ggplot(aes(x=quarter,y=deaths,label=format(deaths,big.mark=","))) + 
  geom_col(width=0.5) +
  geom_text(position=position_dodge(width=0.9), vjust=-0.25) + 
  cowplot::theme_cowplot() + 
  ggtitle("England-Wales deaths within 28 days of positive Covid test",
          subtitle=sprintf("Total: %s",format(sum(EW_deaths_in_28$deaths),big.mark=","))) ->
  EW_deaths_in28_plt

pdf(here('figs','EW_deaths_in_28.pdf'),width=10)
EW_deaths_in28_plt
dev.off()


sum(EW_deaths_in_28$deaths)
