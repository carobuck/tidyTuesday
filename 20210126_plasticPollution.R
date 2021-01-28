## 1/26/21
## Tidy Tuesday: Plastic Pollution

## LOAD LIBRARIES ----
library(tidyverse)
#install.packages("geofacet")
# New package try out!! geofacet: https://hafen.github.io/geofacet/articles/geofacet.html#dental-health-in-scotland-1
library(geofacet)
library(magrittr)


## EXPLORE THE DATA ----
tuesdata <- tidytuesdayR::tt_load('2021-01-26')
plastics <- tuesdata$plastics
# Noticed that Nestle spelled a couple ways, so fixing that. 
plastics %<>% mutate(parent_company = ifelse(str_detect(str_to_lower(parent_company),'nestl'),'Nestle',parent_company)) 

plastics %>%
  filter(parent_company != 'Grand Total') %>%
  group_by(country) %>%
  summarise(hdpe_sum = sum(hdpe,na.rm = TRUE))

plastics %>%
  count(parent_company) %>%
  arrange(desc(n)) %>%
  print(n=25)

# How many what's the most popular brands across the countries?
plastics %>%
  count(country,parent_company) %>%
  count(parent_company) %>%
  arrange(desc(n))

# OR take top 5 companies by count? (same top 5 for counting country and company)
# Cocacola, Unilever, Mondelez, Mars, Colgate --> actually Nestle, not Colgate.

# How many countries have data for the top 5 brands? What years?
top_brands <- c('The Coca-Cola Company','Nestle','Unilever','Mondelez International','Mars, Incorporated')
plastics %>%
  filter(parent_company %in% top_brands) %>%
  filter(country == 'Argentina')
# Just 2019 and 2020
plastics %>%
  filter(parent_company %in% top_brands) %>%
  count(year)

# to plot: filter to top 5 companies, group/count by total plastics by company and country (will be combo of 2019 and 2020 data)
# NEXT TIME: would be interesting to explore difference between 2019 and 2020. 
plastics %>%
  filter(parent_company %in% top_brands) %>%
  group_by(country,parent_company) %>%
  summarise(total = sum(grand_total,na.rm = TRUE)) -> test
  
## MAKE THE PLOT ----
# FYI to list poss geo grids avail: 
get_grid_names() 
# Two world grids: world_86countries_grid and world_countries_grid1
# Messy country names, so looking just at Europe so I don't have to clean up too many
plastics %>%
  filter(parent_company %in% top_brands) %>%
  group_by(country,parent_company) %>%
  summarise(total = sum(grand_total,na.rm = TRUE)) %>%
  #mutate(country = case_when(
   # str_detect(country,'Kingdom') ~ 'United Kingdom',
     
  #))
  ggplot(aes(x=total,y=parent_company,fill=parent_company)) +
  geom_col() +
  theme_bw() +
  facet_geo(~country,grid ='eu_grid1',scales = 'free_x')
  #facet_geo(~ country,grid = 'world_86countries_grid',scales = 'free') #,scales = 'free_y')

  # Mostly don't plot b/c they don't have data in the original data set. 
  # things that don't plot in europe: Sweden ,Finland, UK, Estonia,
# poland, belgium, czech republic, slovakia,lux,austria,hungary, croatia, malta,cyprus

## What about plotting diff part of world? South America? Asia?
plastics %>%
  filter(parent_company %in% top_brands) %>%
  group_by(country,parent_company) %>%
  summarise(total = sum(grand_total,na.rm = TRUE)) %>%
  mutate(country = case_when(
    str_detect(country,'Kingdom') ~ 'United Kingdom',
    TRUE ~ country
    )) %>%
  ggplot(aes(x=total,y=parent_company,fill=parent_company)) +
    geom_col(color = 'black') +
    theme_bw() +
    theme(axis.text.y = element_blank()) +
  scale_fill_manual(values = as.vector(pals::kelly())) +
  labs(x='',y='',fill = 'Parent Company',title='How much plastic trash is collected from the top 5 parent companies?',
       caption = 'Data sourced from Break Free From Plastic, courtesy of Sarah Sauve') +
    facet_geo(~country,grid = 'eu_grid1',scales = 'free_x') +
  # Save high res version of plot! 
  ggsave('plasticPolution',device = 'png')

# example
ggplot(eu_gdp, aes(year, gdp_pc)) +
  geom_line(color = "steelblue") +
  facet_geo(~ name, grid = "eu_grid1", scales = "free_y") +
  scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) +
  ylab("GDP Per Capita in Relation to EU Index (100)") +
  theme_bw()

## BONUS TIDY TUESDAY, W/ BERNIE ----
# b/c why not: https://github.com/R-CoderDotCom/ggbernie?fbclid=IwAR2OJjpWrkZXV76aHOScdxmYufQZa590EG8demLJ3ocfS5kNiEC47HJR-9g
# found this pkg as one of my recommended articles on google
remotes::install_github("R-CoderDotCom/ggbernie@main")
library(ggbernie)

# Plot top 2020 parent companies by total plastics count, with Bernie on the end
plastics %>%
  filter(year == '2020') %>%
  group_by(parent_company) %>%
  summarise(total = sum(grand_total,na.rm = TRUE)) %>%
  arrange(desc(total)) %>%
  # take out top three (unbranded and null)
  slice(-c(1:3)) %>%
  janitor::adorn_percentages(denominator = 'col') %>%
  slice(1:20) %>%
  ggplot(aes(y=reorder(parent_company,total),x=total)) +
  geom_col() +
  labs(x='Percentage of Found Plastic Waste',y='',title='Top 20 Parent Companies for Plastic Cleaned up in 2020',
       caption = 'Made with love in R',subtitle = 'Data sourced from Break Free From Plastic, courtesy of Sarah Sauve') +
  scale_x_continuous(labels = scales::percent) +
  ggbernie::geom_bernie() +
  theme_bw() +
  ggsave('plasticPollution_bernie',device = 'png')
  