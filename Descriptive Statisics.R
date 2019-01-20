########### Installing and Loading Libraries ##################

install.packages('here')
install.packages('gghighlight')
library(tidyverse)
library(readxl)
library(lubridate)
library(here)
library(ggrepel)
library(gghighlight)
library(scales)

########### Reading in Polity IV Data ##########################

p4v_2017 <- read_excel(here("data","Merged_Data","p4v_2017.xlsx")) %>%
    mutate(region = as.factor(region)) %>%
    mutate(region = fct_collapse(region,
                        'Europe and North America' = '0',
                        'Africa' = c('1','3','4','2','12','14','23','25','41'),
                        'Asia' = c('5','6','7','50','53','56','67'),
                        'Central and South America' = c('8','9','89','90'),
                        'Oceania (Island States)' = '99')) %>%
    filter(!is.na(region)) %>%
    mutate(population = ifelse(pop >0 & pop < 10000000, "< 10 Million", 
                             ifelse( pop > 10000000 & pop < 30000000, " 10M - 30M",
                                     ifelse(pop > 30000000 & pop < 50000000, "30M - 50M",
                                            ifelse(pop > 50000000 & pop < 70000000, "50M - 70M", 
                                                   ifelse(pop > 70000000 & pop < 100000000, "70M - 100M",
                                                          ifelse(pop > 100000000 & pop < 300000000, "100M - 300M",
                                                                 ifelse(pop > 300000000 & pop < 1000000000,"300M - 1B",
                                                                        ifelse(pop > 1000000000,"> 1B", NA )))))))))

p4v_history <- read_excel(here("data","Merged_Data","p4v_history.xlsx")) %>%
  mutate(region = as.factor(region)) %>%
  mutate(region = fct_collapse(region,
                               'Europe and North America' = '0',
                               'Africa' = c('1','3','4','2','12','14','23','25','41'),
                               'Asia' = c('5','6','7','50','53','56','67'),
                               'Central and South America' = c('8','9','89','90'),
                               'Oceania (Island States)' = '99')) %>%
  filter(!is.na(region))

p4v_conflict <- read_excel(here("data", "Merged_Data", "p4v_conflict.xlsx")) %>%
  mutate(region = as.factor(region)) %>%
  mutate(region = fct_collapse(region,
                               'Europe and North America' = '0',
                               'Africa' = c('1','3','4','2','12','14','23','25','41'),
                               'Asia' = c('5','6','7','50','53','56','67'),
                               'Central and South America' = c('8','9','89','90'),
                               'Oceania (Island States)' = '99')) %>%
  filter(!is.na(polity2))

########### Drawing the First Graph #############################

p1 <- p4v_2017 %>%
  ggplot(aes(x = polity2, y = gdp_per_capita,size = pop, label = country)) +
  geom_text(aes(color = region)) + 
  geom_vline(xintercept = 0, color = "red") +
  scale_size("Population", breaks = c(0,10000000, 20000000, 40000000,60000000,
                                      100000000, 300000000, 1000000000), 
                           labels = c("< 10 Million","10 Million - 20 Million",
                                      "20 Million - 40 Million", "40 Million - 60 Million",
                                      "60 Million - 100 Million", "100 Million - 300 Million",
                                      "300 Million - 1 Billion", "> 1 Billion")) + 
  geom_smooth(formula = 'y ~ x', method = 'loess') + 
  labs(title = "Political Status vs. GDP per capita ", 
       subtitle = "Developing countries with successful economy polarize on democratic ratings",
       x = "Political Status (Polity2 Index)", 
       y = "GDP per capita (log2)", 
       caption = "Source: Polity2 Index, World Bank", color = "Regions") + 
  scale_y_continuous(trans = "log2", labels = dollar) +
  theme_classic() 

ggsave(here("output","plot1.pdf"), height = 10, width = 30, units = "cm", p1) 


########### Drawing the Second Graph #############################

year_average <- p4v_history %>% 
  group_by(year) %>% 
  summarise(mean = mean(polity2, na.rm = TRUE))

p2 <- p4v_history %>%
  group_by(year,region) %>%
  summarise(mean = mean(polity2, na.rm = TRUE)) %>% 
  ggplot(aes(x = year, y = mean)) + 
  scale_x_continuous(name = "Year (1800 - 2017)", breaks = c(1800, 1921, 1945, 1989),
                     labels = c(1800, 1921, 1945, 1989), trans = "log") +
  geom_step(alpha = 0.4, aes(color = region)) + 
  geom_line(data = year_average, aes(x = year, y = mean), color = "black") +
  geom_vline(xintercept = c(1800, 1921, 1945, 1989), alpha = 0.5, linetype = "longdash") +
  labs(title = "Democratic Status in Different Years", 
       subtitle = "The world has been becoming more democratic despite several recessions.",
       x = "Year(1800 - 2017)",
       y = "Political Status  (Average Polity2 Scores)", 
       caption = "Source: Polity2 Index (1800 - 2017)",
       color = "Regions") + 
  annotate("text", x = 2025, y = 5, label = "World", size = 3) + 
  annotate("text", x = 1860, y = -7, label = "Industrial Revolution") +
  annotate("text", x = 1967, y = -7, label = "Cold War") 

ggsave(here("output","plot2.pdf"), height = 10, width = 30, units = "cm", p2)

########### Drawing the Third Graph #############################

year_status <- p4v_conflict %>%   
  mutate(period = ifelse(year > 1945 & year < 1991, "During Cold War", 
                                          ifelse(year >= 1991, "After Cold War", NA)),
                          status = ifelse(polity2 > 0, "Democratic Areas", "Autocratic Areas")) %>%
  group_by(year, status) %>% 
  summarise(count = n()) 
  

p3 <- p4v_conflict %>%
  mutate(period = ifelse(year > 1945 & year < 1991, "During Cold War", 
                         ifelse(year >= 1991, "After Cold War", NA)),
         status = ifelse(polity2 > 0, "Democratic Areas", "Autocratic Areas")) %>%
  group_by(year, status, region) %>%
  summarise(total = sum(conflict),
            count = n()) %>%
  ggplot(aes(x = year, y = total)) + 
  geom_area(aes(fill = region), position = 'stack') + 
  geom_vline(xintercept = 1991, alpha = 0.8, color = "black", linetype = "longdash") + 
  geom_smooth(data = year_status, aes(x = year, y = count), 
              se = FALSE, color = "black", alpha = 0.8) + 
  scale_y_continuous(sec.axis = sec_axis(~ . *1 , name = "Number of States")) +
  facet_grid(~status) + 
  labs(title = "Conflict Meganitudes in Autocratic / Democratic Areas", 
       subtitle = "Democratic States tend to have less conflicts.",
       x = "Year (1946 - 2017) ", 
       y = "Total Conflict Magnitudes", 
       caption = "Source: Center for Systemic Peace",
       color = "Regions")

ggsave(here("output","plot3.pdf"), height = 10, width = 30, units = "cm", p3)
