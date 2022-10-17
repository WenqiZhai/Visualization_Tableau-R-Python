# install and import packages
install.packages("ggchicklet") # for rounded corners 
install.packages("cowplot") #patch plots
install.packages("hrbrthemes") #beautiful theme
install.packages("ggalt") #dumbell plot
install.packages("GGally") # correlation matrix
install.packages("ggpubr")

# import library
library(tidyverse) 
library(ggchicklet)
library(cowplot)
library(hrbrthemes)
library(ggalt)
library(GGally)
library(ggpubr)


# read data
df_2021 <- read.csv("world-happiness-report-2021.csv")
df_all <- read.csv("world-happiness-report.csv")

colnames(df_2021)[1] <- 'Country.name'
#Fig 1. Ten happinest countries in the world
# dimensions
dimensions <- c('Ladder.score','Logged.GDP.per.capita','Social.support','Healthy.life.expectancy','Freedom.to.make.life.choices','Generosity','Perceptions.of.corruption')
head(df_2021)
# map country to regions
country_region_dict = df_2021 %>% select(country = Country.name, region = Regional.indicator) %>% unique()

df_2021_long <- df_2021 %>% 
  select(country = Country.name, all_of(dimensions)) %>%
  mutate(absence_of_corruption = 1- Perceptions.of.corruption) %>%
  pivot_longer(cols = c(all_of(dimensions),'absence_of_corruption'), names_to = 'dimension', values_to = 'score') %>%
  filter(dimension != "Perceptions.of.corruption")

df_2021_tranformed <- df_2021_long %>%
  group_by(dimension) %>%
  mutate(min_value = min(score),
         max_value = max(score)) %>%
  mutate(score_pct = (score-min_value)/(max_value-min_value)) %>%
  ungroup()

# get top 10
df_2021_top10 <- df_2021_tranformed %>%
  filter(dimension == "Ladder.score") %>%
  slice_max(score, n = 10) %>%
  mutate(cat = 'top_10', 
         country_rank = rank(-score),
         country_label = paste0(country, ' (', country_rank, ')'))

# get bottom 10
df_2021_bottom10 <- df_2021_tranformed %>%
  filter(dimension == "Ladder.score") %>%
  mutate(country_rank = rank(score),
         country_label = paste0(country, ' (', country_rank, ')')) %>%
  slice_min(score, n = 10) %>%
  mutate(cat = 'bottom_10')

fig(12,8)

fig1 <- ggplot(df_2021_top10, aes(x = reorder(country_label, score))) + 
  geom_chicklet(aes(y = 10, fill = 4.9), width = 0.618, radius = grid::unit(10, "pt")) +
  geom_chicklet(aes(y = score, fill = score), width = 0.618, radius = grid::unit(10, "pt")) +
  geom_text(aes(y = score), label = round(df_2021_top10$score,2), nudge_y = 0.4, size = 6) + 
  scale_y_continuous(expand = c(0, 0.1), position = "right", limits = c(0, 10)) +
  scale_fill_gradient2(low = 'black', high = '#7FB185', mid = 'white', midpoint = 5) + 
  coord_flip() +
  labs(y="Best possible life = 10", x = '',
       title="10 Happiest Countries in the World",
       subtitle="9/10 of the happinest countries are in Europe",
       caption="Data Source: The World Happiness Report 2021 on Kaggle") + 
  #theme_ipsum(grid = '') +
  theme_minimal_grid() +
  theme(plot.title = element_text(size=24),
        plot.subtitle = element_text(size = 20),
        plot.caption = element_text(size = 15),
        axis.title.x = element_text(size= 15, color = '#555955'),
        axis.text.y = element_text(size = 19, color = 'black'),
        axis.text.x = element_blank(),
        legend.position = 'None')

#Fig 2. Ten saddest countries in the world
  fig(12,8)
  
  fig2 <- ggplot(df_2021_bottom10, aes(x = reorder(country_label, score))) + 
    geom_chicklet(aes(y = 10, fill = 4.9), width = 0.618, radius = grid::unit(10, "pt")) +
    geom_chicklet(aes(y = score, fill = score), width = 0.618, radius = grid::unit(10, "pt")) +
    geom_text(aes(y = score), label = round(df_2021_bottom10$score,2), nudge_y = 0.4, size = 6) + 
    scale_y_continuous(expand = c(0, 0.1), position = "right", limits = c(0, 10)) +
    scale_fill_gradient2(low = 'black', high = '#7FB185', mid = 'white', midpoint = 5) + 
    coord_flip() +
    labs(y="Best possible life = 10", x = '',
         title="10 Saddest Countries in the World",
         subtitle="Most of them are from Africa and Mid-west Asia",
         caption="Source: The World Happiness Report 2021 from Kaggle") + 
    theme_minimal_grid() +
    theme(plot.title = element_text(size=24),
          plot.subtitle = element_text(size = 20),
          plot.caption = element_text(size = 15),
          axis.title.x = element_text(size= 15, color = '#555955'),
          axis.text.y = element_text(size = 19, color = 'black'),
          axis.text.x = element_blank(),
          legend.position = 'None') 
  
#updated Fig3   Scatter plot
  fig(12,8)
  
 fig4 <-  df_2021  %>% 
    ggplot(aes(Ladder.score,Logged.GDP.per.capita))+
    geom_point(aes(color=Regional.indicator))+
    labs(title = "Happiness_Index increases with GDP per Capita",
         subtitle = "Economic importance in the state of a country")+
    xlab("Happy Score / 10")+
    ylab("GDP per Capita")+
    theme_ipsum_es() + # Arial Narrow
    scale_fill_ipsum() +
    theme(legend.position = "top") +
    theme(plot.title=element_text(hjust=0.5),
          plot.subtitle=element_text(hjust=0.5))
  
  

#Fig 4. Correlation matrix
  
  df_cor <- df_2021 %>% 
    select(corruption = Perceptions.of.corruption,
           generosity = Generosity,
           freedom = Freedom.to.make.life.choices, 
           life_expectancy = Healthy.life.expectancy, 
           social_support = Social.support,
           GDP_per_capita = Logged.GDP.per.capita, 
           happiness = Ladder.score
    )
  
  fig(9,8)
 ggcorr(df_cor, 
         method = c("everything", "pearson"), 
         size = 6, hjust = 0.77,
         low = '#edae52', mid = 'white', high = "#7FB185",
         label = TRUE, label_size = 6,
         layout.exp = 1) +
    labs(title = 'Correlation Matrix',
         subtitle = 'Happiness most strongly correlates with 1) wealth (GDP)\n2) health, 3) social support, and 4) freedom') +
    theme_ipsum() +
    #theme_minimal() +
    theme(plot.title = element_text(size=24),
          plot.subtitle = element_text(size = 18),
          legend.text = element_text(size = 18))
  

  ggarrange(fig1, fig3, fig2,fig4,
                      ncol = 2, nrow = 2, common.legend = TRUE)
