library(tidyverse)
library(here)
library(lubridate)

read_csv(here('data','data_2022-Jan-22-death-cert.csv'))

Covid_deathcert <- read_csv(here('data','data_2022-Jan-22-death-cert.csv')) %>% 
  filter(areaName %in% c("England","Wales")) %>%
  select(areaName,date,newDailyNsoDeathsByDeathDate ) %>% 
  mutate(year=year(date),quarter=sprintf("%d-Q%d",year(date),quarter(date)),) %>%
  rename(deaths=newDailyNsoDeathsByDeathDate) %>% 
  filter(quarter < "2021-Q4")

pdf(here('figs','EW_covid_deaths_on_cert.pdf'),width=10)
Covid_deathcert %>% 
  group_by(quarter) %>%
  summarise(deaths=sum(deaths)) %>% 
  ggplot(aes(x=quarter,y=deaths,label=format(deaths,big.mark=","))) + 
  geom_col(width=0.5) +
  geom_text(position=position_dodge(width=0.9), vjust=-0.25) + 
  cowplot::theme_cowplot() + 
  ggtitle("England-Wales deaths with Covid-19 on the death certificate",
          subtitle=sprintf("Total: %s",format(sum(Covid_deathcert$deaths),
                                              big.mark=",")))
dev.off()


sum(EW_deaths_in_28$deaths)
