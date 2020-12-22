library(skimr)
library(tidyverse)
library(ggplot2)
library(forcats)
library(lubridate)
library(tibbletime)

# Either ISO-8601 date or year/week works!


bigMac <- read.csv("data/big-mac-adjusted-index.csv")
bigMac_raw <- read.csv("data/big-mac-raw-index.csv")

skim(bigMac)

colnames(bigMac)

bigMac$name <- factor(bigMac$name)

class(bigMac$date)

bigMac$date <- ymd(bigMac$date)

summary(bigMac$date)

bigMac %>% 
# transform to date format with lubridate
mutate(loco = ymd(date)) %>% 
  # find min and max
  summarise(min = min(loco),
            max = max(loco)) 
bigMac %>% 
  filter(date == "2020-07-01" | date == "2011-07-01") %>% 
  ggplot(aes(fct_reorder(name, adj_price ), adj_price)) +
  facet_wrap(~date)+
  geom_line(group = 1, color="darkgreen")+
  geom_point(aes(fct_reorder(name, dollar_price), dollar_price), colour = "red")+
  theme(axis.text.x = element_text(angle = 45, hjust=1)) 
  

filtered <- bigMac %>% 
  filter(date == "2020-07-01" | date == "2011-07-01") %>% 
  mutate(currency_valuation = adj_price - dollar_price) %>% 
  select(date, name, currency_valuation) %>% 
  arrange(abs(currency_valuation))


library(reshape2)


# "difference/momentum" of change of either over or under evaluation
# countries near 0 have not changed much in this time period of observation
# countries with high values are currently much more over valued than they were in 2011
# however this does not say anything about where they currently are wrt under/over evaluation
filtered %>% 
  pivot_wider(names_from = date, values_from = currency_valuation) %>% 
  mutate(diff_bw_years = `2020-07-01` - `2011-07-01`) %>% 
  ggplot(aes(fct_reorder(name,diff_bw_years), diff_bw_years)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))


## So now we want to see how this "difference/momentum" of change towards currently over-evaluated 
#countries was over the last 10 years 

#first take the top 10 countries that are currently highly over valued  
top10_2020_overValued <- bigMac %>% 
  filter(date == "2020-07-01") %>% 
  mutate(overValued = dollar_price - adj_price) %>% 
  select(date, name, overValued) %>% 
  arrange(desc(overValued)) %>% 
  top_n(10) 

overValued_2020 <- bigMac %>% 
  filter(date == "2020-07-01") %>% 
  mutate(overValued = dollar_price - adj_price) %>% 
  select(date, name, overValued) %>% 
  arrange(desc(overValued)) 


#Thailand, New Zealand, Hong Kong

overValued_2020$quartile <- ntile(overValued_2020$overValued, 3)  
#now see the rolling difference b/w years for these countries 
rollingDiff <- rollify(.f = diff, window = 2)
normalit<-function(m){
  (m - mean(m))/(sd(m))
}


cur <- bigMac %>% 
  filter(name %in% top10_2020_overValued$name) %>% 
  mutate(overValued = dollar_price - adj_price) %>% 
  #rolling b/w between the overvalued column by 2 years 
  group_by(name) %>% 
  mutate(roll.diff =lag(rollingDiff(overValued)),
         currencyFlucs = normalit(dollar_ex)) %>% 
  select(name, date, roll.diff, currencyFlucs) %>% 
  ggplot(aes(date, currencyFlucs)) +
  geom_line(group=1)+
  facet_grid(~name) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) 

rolDif <- bigMac %>% 
  filter(name %in% top10_2020_overValued$name) %>% 
  mutate(overValued = dollar_price - adj_price) %>% 
  #rolling b/w between the overvalued column by 2 years 
  group_by(name) %>% 
  mutate(roll.diff =lag(rollingDiff(overValued)),
         currencyFlucs = scale(dollar_ex)) %>% 
  select(name, date, roll.diff, roll.diff) %>% 
  ggplot(aes(date, roll.diff)) +
  geom_line(group=1)+
  facet_grid(~name) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) 

cur


library(patchwork)


rolDif / cur




#



rolDif_B <- bigMac %>% 
  filter(name == "Brazil") %>% 
  mutate(overValued = dollar_price - adj_price) %>% 
  select(name, date, overValued) %>% 
  ggplot(aes(date, overValued)) +
  ggtitle("Brazil")+
  geom_line(group=1, colour="#8b0000")+
  theme_bw()+
  ylab("")+
  ylim(0,3)+
  theme(axis.title.x=element_blank(),
                          axis.text.x=element_blank(),
                          axis.ticks.x=element_blank())
## for one currency

cur_B <- bigMac %>% 
 filter(name == "Brazil") %>% 
  mutate(overValued = dollar_price - adj_price) %>% 
  #rolling b/w between the overvalued column by 2 years 
  group_by(name) %>% 
  # mutate(roll.diff =lag(rollingDiff(overValued)),
  #        currencyFlucs = normalit(dollar_ex)) %>% 
  select(name, date, dollar_ex) %>% 
  mutate(exRate = 1/dollar_ex) %>% 
  ggplot(aes(date, exRate)) +
  geom_line(group=1, colour="#8b0000")+
  theme_bw()+
  ylim(0, 1)+
  ylab("")+
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        axis.title.x=element_blank()) 
rolDif_B / cur_B

rolDif_NZ <- bigMac %>% 
  filter(name == "New Zealand") %>% 
  mutate(overValued = dollar_price - adj_price) %>% 
  select(name, date, overValued) %>% 
  ggplot(aes(date, overValued)) +
  geom_line(group=1, colour="#009456")+
  ggtitle("New Zealand")+
  theme_bw()+
  ylab("")+
  ylim(-1,3)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
## for one currency

cur_NZ <- bigMac %>% 
  filter(name == "New Zealand") %>% 
  mutate(overValued = dollar_price - adj_price) %>% 
  #rolling b/w between the overvalued column by 2 years 
  group_by(name) %>% 
  # mutate(roll.diff =lag(rollingDiff(overValued)),
  #        currencyFlucs = normalit(dollar_ex)) %>% 
  select(name, date, dollar_ex) %>% 
  mutate(exRate = 1/dollar_ex) %>% 
  ggplot(aes(date, exRate)) +
  geom_line(group=1, colour="#009456")+
  theme_bw()+
  ylim(0, 1)+
  ylab("")+
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        axis.title.x=element_blank()) 
rolDif_NZ / cur_NZ


rolDif_Sw <- bigMac %>% 
  filter(name == "Switzerland") %>% 
  mutate(overValued = dollar_price - adj_price) %>% 
  select(name, date, overValued) %>% 
  ggplot(aes(date, overValued)) +
  geom_line(group=1, colour = "#000067")+
  theme_bw()+
  ylab("`Over` or `Under` Valued")+
  ggtitle("Switzerland")+
  #ylim(-3,0)+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
## for one currency

cur_Sw <- bigMac %>% 
  filter(name == "Switzerland") %>% 
  mutate(overValued = dollar_price - adj_price) %>% 
  #rolling b/w between the overvalued column by 2 years 
  group_by(name) %>% 
  # mutate(roll.diff =lag(rollingDiff(overValued)),
  #        currencyFlucs = normalit(dollar_ex)) %>% 
  select(name, date, dollar_ex) %>% 
  mutate(exRate = 1/dollar_ex) %>% 
  ggplot(aes(date, exRate)) +
  geom_line(group=1, colour = "#000067")+
  theme_bw()+
 # ylim(0, 1)+
  ylab("How many dollars you need
       for 1 unit of this currency?")+
  theme(axis.text.x = element_text(angle = 45, hjust=1),
        axis.title.x=element_blank()) 
  theme(panel.background =element_rect(fill = "yellow", colour = "grey50"))


p1 <- rolDif_Sw / cur_Sw 


p2 <- rolDif_NZ / cur_NZ

p3 <- rolDif_B / cur_B



(p1 + plot_spacer() | p2) / (plot_spacer() + p3 + plot_spacer())

install.packages("ggtext")
library(ggtext)

ggplot() + geom_text() +
  annotate(label = "plot mpg vs. wt")
  


patchwork <- (((rolDif_Sw / cur_Sw ) | plot_spacer()  | (rolDif_NZ / cur_NZ)) /
  (plot_spacer() + (rolDif_B / cur_B) + plot_spacer())) 



patchwork <- (rolDif_Sw / cur_Sw ) | (rolDif_NZ / cur_NZ) | (rolDif_B / cur_B) 


patchwork <- patchwork & theme(panel.background =element_rect(fill = "#F5DFD5", colour = "grey50"),
                  plot.background = element_rect(fill = "#F2F2F2", colour = "#F2F2F2"),
                  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  text = element_text('mono')) 
  
patchwork + plot_annotation(title = 'Big Mac Index',
                            subtitle = "The Economist says that `The difference between the predicted and the market price of Big Mac 
                            is an alternative measure of currency valuation`, 
                            so I plotted this measure of three countries alongside their exchange rate to dollars 
                            over the years, and well well well! It does look like Big Mac Index holds as a (quasi)
                            representative the country's current economic status! ",
                  theme = theme(plot.title = element_text(size = 18))) 
ggsave("bigMacIndex_2020-12-21.png", path="output/", dpi=300, width=13, height=9)

?ggsave

