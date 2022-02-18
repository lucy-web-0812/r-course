
library(tidyverse)

# read data ---------------------------------------------------------------

avgdata = read_csv(here::here("workshop_dataviz/data/historicalAverageData.csv"))
tsdata = read_csv(here::here("workshop_dataviz/data/timeSeriesData.csv"))

# ggplot, geom_* -----------------------------------------------------

tsdata %>% 
  filter(code == "MY1") %>% 
  select(-median) %>% 
  pivot_wider(names_from = name, values_from = mean) %>% 
  ggplot(aes(x = o3, y = no2)) +
  geom_point() +
  geom_smooth(method = "lm")

avgdata %>% 
  filter(country == "united_kingdom",
         name == "no2") %>% 
  ggplot(aes(x = mean)) +
  geom_density()

avgdata %>% 
  group_by(country, theYear, name) %>% 
  summarise(country_avg = mean(mean)) %>% 
  filter(name == "no2", country == "united_kingdom") %>% 
  ggplot(aes(y = theYear, x = country_avg)) +
  geom_col()

# aes ---------------------------------------------------------------------

tsdata %>% 
  filter(code %in% c("MY1", "KC1")) %>% 
  select(-median) %>% 
  pivot_wider(names_from = name, values_from = mean) %>% 
  ggplot(aes(x = o3, y = no2)) +
  geom_point(aes(color = code)) +
  geom_smooth(method = "lm", aes(group = code))

avgdata %>% 
  filter(country == "united_kingdom",
         name == "no2") %>% 
  ggplot(aes(x = mean, color = theYear, fill = theYear)) +
  geom_density(alpha = .3)

avgdata %>% 
  group_by(country, theYear, name) %>% 
  summarise(country_avg = mean(mean)) %>% 
  filter(name == "no2") %>% 
  ggplot(aes(y = country, x = country_avg, fill = theYear)) +
  geom_col(position = position_dodge())

# scales & labels ---------------------------------------------------------

tsdata %>% 
  filter(code %in% c("MY1", "KC1")) %>% 
  select(-median) %>% 
  pivot_wider(names_from = name, values_from = mean) %>% 
  ggplot(aes(x = o3, y = no2)) +
  geom_point(aes(color = code)) +
  geom_smooth(method = "lm", aes(group = code)) +
  scale_x_continuous(name = "Ozone", limits = c(0, NA)) +
  scale_y_continuous(name = "Nitrogen Dioxide", limits = c(0, NA))

avgdata %>% 
  filter(country == "united_kingdom",
         name == "no2") %>% 
  ggplot(aes(x = mean, color = theYear, fill = theYear)) +
  geom_density(alpha = .3, size = 1) +
  scale_color_manual(values = c("darkgreen", "darkblue")) +
  scale_fill_manual(values = c("darkgreen", "darkblue"))

avgdata %>% 
  group_by(country, theYear, name) %>% 
  summarise(country_avg = mean(mean)) %>% 
  filter(name == "no2") %>% 
  ggplot(aes(y = country, x = country_avg, fill = theYear)) +
  geom_col(position = position_dodge()) +
  scale_fill_brewer(palette = "Set1")

# facets ------------------------------------------------------------------

codes = c("lu0101a", "lu0102a", "ch0010a", "ch0011a", "MY1", "KC1")

tsdata %>% 
  filter(code %in% codes) %>%
  pivot_longer(mean:median, names_to = "stat") %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  ggplot(aes(x = o3, y = no2)) +
  geom_point(aes(color = code)) +
  geom_smooth(method = "lm", aes(group = code)) +
  scale_x_continuous(name = "Ozone", limits = c(0, NA)) +
  scale_y_continuous(name = "Nitrogen Dioxide", limits = c(0, NA)) +
  facet_wrap(~country)

avgdata %>% 
  filter(country %in% c("united_kingdom", "france", "switzerland")) %>% 
  ggplot(aes(x = mean, color = theYear, fill = theYear)) +
  geom_density(alpha = .3, size = 1) +
  scale_color_manual(values = c("darkgreen", "darkblue")) +
  scale_fill_manual(values = c("darkgreen", "darkblue")) +
  facet_grid(country~name, scales = "free")

avgdata %>% 
  group_by(country, theYear, name) %>% 
  summarise(country_avg = mean(mean)) %>% 
  ggplot(aes(y = country, x = country_avg, fill = theYear)) +
  geom_col(position = position_dodge()) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(name = NULL) +
  theme(strip.placement = "outside") +
  facet_wrap(~name, scales = "free_x", strip.position = "bottom") +
  NULL

# theme -------------------------------------------------------------------

plt1 = tsdata %>% 
  filter(code %in% codes) %>%
  pivot_longer(mean:median, names_to = "stat") %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  ggplot(aes(x = o3, y = no2)) +
  geom_point(aes(color = code)) +
  geom_smooth(method = "lm", aes(group = code)) +
  scale_x_continuous(name = "Ozone", limits = c(0, NA)) +
  scale_y_continuous(name = "Nitrogen Dioxide", limits = c(0, NA)) +
  facet_wrap(~country) +
  theme_light() +
  theme(legend.position = "none")

plt2 = avgdata %>% 
  filter(country %in% c("united_kingdom", "france", "switzerland")) %>% 
  ggplot(aes(x = mean, color = theYear, fill = theYear)) +
  geom_density(alpha = .3, size = 1) +
  scale_color_manual(values = c("darkgreen", "darkblue")) +
  scale_fill_manual(values = c("darkgreen", "darkblue")) +
  facet_grid(country~name, scales = "free") +
  theme_light() +
  theme(legend.position = "top")

plt3 = avgdata %>% 
  group_by(country, theYear, name) %>% 
  summarise(country_avg = mean(mean)) %>% 
  ggplot(aes(y = country, x = country_avg, fill = theYear)) +
  geom_col(position = position_dodge()) +
  scale_fill_brewer(palette = "Set1") +
  scale_x_continuous(name = NULL) +
  facet_wrap(~name, scales = "free_x", strip.position = "bottom") +
  theme_light() +
  theme(legend.position = "top", 
        strip.placement = "outside",
        strip.text = element_text(color = "black"),
        strip.background = element_blank())
