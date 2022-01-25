library(tidyverse)
library(here)
dir(here('data'))

EW_deaths_preconditions <- read_csv(here('data',"Deaths_From_Covid_EW.csv")) %>% 
  pivot_longer(cols=c('1_64','65_up'),
               names_to="age_group",
               values_to="deaths")

EW_deaths_by_period <- 
  EW_deaths_preconditions %>% 
  group_by(Period) %>% 
  summarize(deaths=sum(deaths)) %>% ungroup()

EW_deaths_by_period

EW_deaths_preconditions %>% 
  group_by(Period,n_conditions) %>% 
  summarize(deaths=sum(deaths)) %>% ungroup()

EW_deaths_preconditions

pdf(here('figs','EW_covid_deaths_from_covid.pdf'),width=10)
EW_deaths_preconditions %>% 
  group_by(Period,n_conditions) %>% 
  summarize(deaths=sum(deaths)) %>% ungroup() %>%
  ggplot(aes(x=Period,y=deaths,label=format(deaths,big.mark=","),fill=n_conditions)) +
  geom_col(width=0.5) +
  geom_text(data=EW_deaths_by_period,aes(x=Period,y=deaths,fill=NULL),
            position=position_dodge(),vjust=-.25) + 
  #geom_text(position=position_dodge(),vjust=-.95) + 
  cowplot::theme_cowplot() + 
  ggtitle("England-Wales deaths from Covid-19",
          subtitle=sprintf("Total: %s",format(sum(EW_deaths_preconditions$deaths),big.mark=","))) + 
  theme(legend.position = c(0.85, 1.0)) + 
  guides(fill=guide_legend(title="Number of\npre-existing\nconditions"))
dev.off()
