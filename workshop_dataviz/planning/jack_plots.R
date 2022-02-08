
library(tidyverse)

avgdata = read_csv(here::here("workshop_dataviz/data/historicalAverageData.csv"))
tsdata = read_csv(here::here("workshop_dataviz/data/timeSeriesData.csv"))

# Timeseries with average line

tsdata %>% 
  filter(country == "united_kingdom") %>% 
  group_by(date, name) %>% 
  mutate(avg = mean(mean, na.rm = T)) %>% 
  ggplot(aes(x = date, y = mean)) +
  geom_line(aes(group = code), color = "grey75") +
  geom_line(aes(y = avg), color = "red", size = 1) +
  facet_wrap(~name, ncol = 1, scales = "free_y") +
  expand_limits(y = 0) +
  theme_bw() +
  labs(y = "Mean Concentration", x = NULL)

# density functions

avgdata %>% 
  ggplot(aes(x = mean)) +
  geom_density(aes(lty = theYear, color = name)) +
  scale_x_continuous(limits = c(0,100)) +
  scale_linetype_manual(values = c("2015 - 2019" = "dashed", "2020" = "solid")) +
  theme_classic()

# boxplots

avgdata %>% 
  ggplot(aes(y = country, x = mean)) +
  geom_boxplot(aes(color = theYear)) +
  facet_wrap(~name, scales = "free_x", ncol = 1) +
  theme_minimal() + 
  labs(x = "Mean Concentration", y = NULL) +
  theme(legend.position = "top")

# scatter

avgdata %>% 
  group_by(country, code, name, theYear) %>% 
  summarise(mean = mean(mean)) %>% 
  pivot_wider(names_from = name, values_from = mean) %>% 
  ggplot(aes(x = no2, y = o3)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = openair::quickText("Nitrogen Dioxide (NO2)"), 
       y = openair::quickText("Ozone (O3)")) +
  theme_light()
