
library(tidyverse)
library(patchwork)

# read data ---------------------------------------------------------------

avgdata = read_csv(here::here("workshop_dataviz/data/historicalAverageData.csv"))
tsdata = read_csv(here::here("workshop_dataviz/data/timeSeriesData.csv"))

# ggplot, geom_* -----------------------------------------------------

s1 = tsdata %>% 
  filter(code == "MY1") %>% 
  select(-median) %>% 
  pivot_wider(names_from = name, values_from = mean) %>% 
  ggplot(aes(x = o3, y = no2)) +
  geom_point() +
  geom_smooth(method = "lm")

d1 = avgdata %>% 
  filter(country == "united_kingdom",
         name == "no2") %>% 
  ggplot(aes(x = mean)) +
  geom_density()

b1 = avgdata %>% 
  group_by(country, theYear, name) %>% 
  summarise(country_avg = mean(mean)) %>% 
  filter(name == "no2", country == "united_kingdom") %>% 
  ggplot(aes(y = theYear, x = country_avg)) +
  geom_col()

# aes ---------------------------------------------------------------------

s2 = tsdata %>% 
  filter(code %in% c("MY1", "KC1")) %>% 
  select(-median) %>% 
  pivot_wider(names_from = name, values_from = mean) %>% 
  ggplot(aes(x = o3, y = no2)) +
  geom_point(aes(color = code)) +
  geom_smooth(method = "lm", aes(group = code))

d2 = avgdata %>% 
  filter(country == "united_kingdom",
         name == "no2") %>% 
  ggplot(aes(x = mean, color = theYear, fill = theYear)) +
  geom_density(alpha = .3)

b2 = avgdata %>% 
  group_by(country, theYear, name) %>% 
  summarise(country_avg = mean(mean)) %>% 
  filter(name == "no2") %>% 
  ggplot(aes(y = country, x = country_avg, fill = theYear)) +
  geom_col(position = position_dodge())

# scales & labels ---------------------------------------------------------

s3 = tsdata %>% 
  filter(code %in% c("MY1", "KC1")) %>% 
  select(-median) %>% 
  pivot_wider(names_from = name, values_from = mean) %>% 
  ggplot(aes(x = o3, y = no2)) +
  geom_point(aes(color = code)) +
  geom_smooth(method = "lm", aes(group = code)) +
  scale_x_continuous(name = "Ozone", limits = c(0, NA)) +
  scale_y_continuous(name = "Nitrogen Dioxide", limits = c(0, NA))

d3 = avgdata %>% 
  filter(country == "united_kingdom",
         name == "no2") %>% 
  ggplot(aes(x = mean, color = theYear, fill = theYear)) +
  geom_density(alpha = .3, size = 1) +
  scale_color_manual(values = c("darkgreen", "darkblue")) +
  scale_fill_manual(values = c("darkgreen", "darkblue"))

b3 = avgdata %>% 
  group_by(country, theYear, name) %>% 
  summarise(country_avg = mean(mean)) %>% 
  filter(name == "no2") %>% 
  ggplot(aes(y = country, x = country_avg, fill = theYear)) +
  geom_col(position = position_dodge()) +
  scale_fill_brewer(palette = "Set1")

# facets ------------------------------------------------------------------

codes = c("lu0101a", "lu0102a", "ch0010a", "ch0011a", "MY1", "KC1")

s4 = tsdata %>% 
  filter(code %in% codes) %>%
  pivot_longer(mean:median, names_to = "stat") %>% 
  pivot_wider(names_from = name, values_from = value) %>% 
  ggplot(aes(x = o3, y = no2)) +
  geom_point(aes(color = code)) +
  geom_smooth(method = "lm", aes(group = code)) +
  scale_x_continuous(name = "Ozone", limits = c(0, NA)) +
  scale_y_continuous(name = "Nitrogen Dioxide", limits = c(0, NA)) +
  facet_wrap(~country)

d4 = avgdata %>% 
  filter(country %in% c("united_kingdom", "france", "switzerland")) %>% 
  ggplot(aes(x = mean, color = theYear, fill = theYear)) +
  geom_density(alpha = .3, size = 1) +
  scale_color_manual(values = c("darkgreen", "darkblue")) +
  scale_fill_manual(values = c("darkgreen", "darkblue")) +
  facet_grid(country~name, scales = "free")

b4 = avgdata %>% 
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

s5 = tsdata %>% 
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

d5 = avgdata %>% 
  filter(country %in% c("united_kingdom", "france", "switzerland")) %>% 
  ggplot(aes(x = mean, color = theYear, fill = theYear)) +
  geom_density(alpha = .3, size = 1) +
  scale_color_manual(values = c("darkgreen", "darkblue")) +
  scale_fill_manual(values = c("darkgreen", "darkblue")) +
  facet_grid(country~toupper(name), scales = "free") +
  theme_light() +
  theme(legend.position = "top")

b5 = avgdata %>% 
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

(s1 / s2 / s3 / s4 / s5) | (d1 / d2 / d3 / d4 / d5) | (b1 / b2 / b3 / b4 / b5)
