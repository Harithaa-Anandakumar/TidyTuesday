library(readr)
library(tidyverse)

library(rnaturalearth)
transit_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv')

continents <- countrycode::codelist %>%
  select(ecb, country.name.en, continent) %>%
  rename(country = country.name.en)

city_locs<- readr::read_csv("data/worldcities.csv") %>%
  select(city, country, lat, lng) %>%
  mutate(country = if_else(country == "Korea, South", "South Korea", country))


world <-  ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(!continent %in% c("Antarctica", "Seven seas (open ocean)"))

dat <- transit_cost %>% 
  filter(!is.na(city)) %>%
  rename(ecb = country) %>%
  mutate(ecb = if_else(ecb == "UK", "GB", ecb)) %>%
  left_join(continents, by = "ecb") %>%
  left_join(city_locs, by = c("city", "country")) %>%
  filter(!is.na(lat)) %>%
  mutate(start_year = as.numeric(start_year),
         end_year = as.numeric(end_year))


last_ten_years <- c(2010:2019)
lt <- dat %>% 
  select(end_year, cost_km_millions,continent, country, lat, lng) %>% 
  group_by(end_year, country) %>% 
  filter(end_year %in% last_ten_years) %>% 
  mutate(country = as.factor(country)) %>% 
  group_by(country) %>% 
  summarise(n_built = n()) %>% 
  ungroup() %>% 
  mutate(time_period = "2010 to 2019") %>% 
  arrange(desc(n_built))

early_ten_years <- c(2000:2009)

et <- dat %>% 
  select(end_year, cost_km_millions,continent, country, lat, lng) %>% 
  group_by(end_year, country) %>% 
  filter(end_year %in% early_ten_years) %>% 
  mutate(country = as.factor(country)) %>% 
  group_by(country) %>% 
  summarise(n_built = n()) %>% 
  ungroup() %>% 
 mutate(time_period = "2000 to 2009") %>%
  arrange(desc(n_built))

merged <- rbind(lt, et)



library(extrafont)

library(ggbump)

library(cowplot)
library(wesanderson)


# Colors
color.background = "#F2E6D8"
color.text = "#22211d"

# Begin construction of chart




hhh <- merged %>% 
  group_by(time_period) %>% 
  mutate(rank = rank(time_period, ties.method = "last"))%>% 
  ungroup()



p <- ggplot(hhh, aes(time_period, rank)) +
  geom_bump(size = 2,smooth=8, show.legend = T, alpha=0.2) +
  geom_point(size = 2, alpha=0.3) +
 theme_minimal_grid(font_size = 10, line_size = 0) +
  theme(panel.grid.major = element_blank(),
        axis.ticks = element_blank())   

library(glue)

p <- p + geom_bump(data = hhh %>% filter(country %in% c("Germany","China","Japan","Italy")), 
            aes(time_period, rank, color = country),
            smooth = 8, size = 2, inherit.aes = F) +
  geom_point(data = hhh %>% filter(country %in% c("Germany","China","Japan","Italy")),
             size = 2, alpha=0.7)+
  geom_text(data = hhh %>% 
              filter(country %in% c("Italy","Germany","Japan")) %>% 
              filter(time_period == "2010 to 2019"),
            aes(label = glue("{country}, {n_built}")) , hjust =-0.15, 
             color = "#888888", size = 4)+
  geom_text(data = hhh %>% 
              filter(country %in% c("China")) %>% 
              filter(time_period == "2010 to 2019"),
            aes(label = glue("{country}, {n_built}")), hjust =-0.15,
           color = "#E05A46", size = 4)+
  geom_segment(data = hhh %>% 
              filter(country %in% c("Turkey","Spain","India")) %>% 
              filter(time_period == "2010 to 2019"),
            aes(x=2 , xend=2.165, y=rank, yend= rank),
            size = 1,
            lineend = "round", color = "grey")+
  geom_text(data = hhh %>% 
              filter(country %in% c("Turkey","Spain","India")) %>% 
              filter(time_period == "2010 to 2019"),
            aes(label = glue("{country}, {n_built}"), x = 2.3) ,  
             color = "#7E7293", size = 4)+
  geom_text(data = hhh %>% 
              filter(country %in% c("Germany","China","Japan","Italy")) %>% 
              filter(time_period == "2000 to 2009"),
            aes(label = glue("{country}, {n_built}"), x = 0.75) , hjust =0.5, 
            color = "#888888", size = 4) +
  geom_point(data = hhh %>% filter(country %in% c("Turkey","China","Spain","Italy","India")),
             size = 2, alpha=0.7)+
  scale_color_manual(values = wes_palette(n = 4, name = "IsleofDogs1")) +
  labs(title = "Changes over decades",
       
       subtitle = "      As per the dataset which looked at transport projects in different countries, 
       I grouped and ranked countries in two groups: between 2000 to 2009 and from 2010 to 2019 
       and looked at which countries completed the most number of projects in that time span. 
       
       We see that China leads the world in transit projects in both the decades, while Japan
       and Germany which ranked high previously have now fallen through. 
       
       And in the past decade other countries like India, Turkey and Spain 
       have climbed the charts! 
       
       Read: Country, number of projects completed.")



p + theme_bw() +
  
  # Format background colors
  theme(panel.background = element_rect(fill=color.background, color=color.background)) +
  theme(plot.background  = element_rect(fill=color.background, color=color.background)) +
  theme(panel.border     = element_rect(color=color.background)) +
  theme(strip.background = element_rect(fill=color.background, color=color.background)) +
  
  # Format the grid
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.y = element_blank()) +
  theme(axis.ticks       = element_blank()) +
  theme(axis.title     = element_blank())+
  theme(text=element_text( family="Verdana"))+
  # Format the legend
  theme(legend.position = "none") +
  
  # Format title and axis labels
  theme(plot.title       = element_text(color=color.text, size=20, face = "bold")) +
  #theme(axis.title.x     = element_text(size=14, color="black", face = "bold")) +
  #theme(axis.title.y     = element_text(size=14, color="black", face = "bold", vjust=1.25)) +
  theme(axis.text.x      = element_text(size=10, vjust=0.5, hjust=0.5, color = color.text)) +
 # theme(axis.text.y      = element_text(size=10, color = color.text)) +
  theme(strip.text       = element_text(face = "bold")) +
  
  # Plot margins
  theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))

ggsave("output/transit-costs.png", width = 8.5, height = 11)


