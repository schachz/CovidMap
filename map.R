# Load Data
geocode <- read.csv("https://raw.githubusercontent.com/schachz/CovidMap/main/geocode.csv")
geo <- geocode
owid.covid.data <- read.csv("https://raw.githubusercontent.com/schachz/CovidMap/main/owid.covid.data.csv")
full <- owid.covid.data


# Load Packages
library(tidyverse)
library(ggmap)
library(maps)
library(sf)

# Data manipulation
colnames(full)[2] <- "region"
colnames(geo)[4] <- "region"
covid <- left_join(full,geo, by="region")
colnames(covid)[58:59] <- c("lat","long")


covid <- covid %>% 
  select(date,region, total_cases,total_deaths,lat,long) %>%
  filter(date == "3/16/2021")


world <- map_data("world")
covid <- na.omit(covid)


covid$region <- ifelse(covid$region == "United States","USA", covid$region)

world <- left_join(world, covid, by="region")

full$region <- ifelse(full$region == "United States", "USA", full$region)
covid2 <- filter(full, date %in% c("3/29/2020","3/29/2021"))

covid3 <- left_join(world,covid2, by="region")
covid3 <- covid3 %>% filter(date.x != "NA")


##############
# Creating plot

ggplot() +
  geom_polygon(data=covid3,
               aes(x=long.x, y=lat.x,
                   group=group,
                   fill=total_deaths_per_million)) +
  facet_grid(date.y~.)+
  theme_void()+
  theme(legend.position = "bottom",
        legend.key.size = unit(1.5, "cm"),
        legend.text=element_text(size=10))+
  labs(title = "Total Deaths per Million by Country", 
       subtitle = "Data as of March 29th, 2021 vs 1 year prior",
       caption = "Source: OurWorldInData.org",
       x = NULL, 
       y = NULL, 
       fill = "Total Deaths\nper Million")+
  scale_fill_distiller(palette = "RdYlBu")

