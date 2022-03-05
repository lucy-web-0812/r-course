

library(tidyverse)

# Spheroids ---------------------------------------------------------------

spheroids = read_csv("~/Phase_function_spheroids.txt") %>%
  pivot_longer(-Degrees)

spheroids360 <-
  bind_rows(spheroids,
            mutate(spheroids, Degrees = rev(Degrees) + 180))

lenses = tibble(lens = c("Lens 1", "Lens 2", "Lens 3"),
                center = c(60, 90, 120))

spheroids360 %>%
  ggplot(aes(x = Degrees, y = value, color = name)) +
  geom_line() +
  geom_rect(data = lenses, inherit.aes = F,
            aes(xmin = center-25, xmax = center+25, 
                ymin = 0, ymax = Inf, fill = lens),
            alpha = .2) +
  coord_polar(start = pi / 2) +
  scale_x_continuous(limits = c(0, 360), breaks = seq(0, 325, 45)) +
  theme_minimal() +
  theme(axis.text.y = element_blank()) +
  labs(x = NULL, y = NULL,
       color = "Aspect Ratio")

# Other -------------------------------------------------------------------

ratios = read_csv("~/Spheroid_ratios.txt")

