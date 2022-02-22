
library(tidyverse)

# Seth --------------------------------------------------------------------

dat <- tibble(
  x = 0:180,
  y = sample(1:100, 181, T)
) %>%
  mutate(y = smooth.spline(y, df = 100)$y)

bind_rows(dat, mutate(dat, x = rev(x) + 180)) %>%
  ggplot(aes(x = x, y = y)) +
  geom_line(color = "red", size = 1) +
  geom_vline(xintercept = c(0, 180), size = 1) +
  coord_polar(start = pi / 2) +
  scale_x_continuous(
    breaks = c(0, 90, 180, 270),
    limits = c(0, 360), labels = c(0, 90, 180, 90)
  ) +
  theme_minimal()


# Gaussian ----------------------------------------------------------------

hist <- tibble(y = rnorm(10000)) %>%
  ggplot(aes(y)) +
  geom_density()

dat <- ggplot_build(hist)$data[[1]] %>%
  tibble() %>%
  select(x, y) %>%
  bind_rows(tibble(x = seq(to = -2, from = -10, by = .01), y = 0)) %>%
  mutate(y = jitter(y, 1000))

dat %>%
  ggplot(aes(x, y)) +
  geom_point(alpha = .25) +
  geom_smooth(method = "gam", size = 2)
