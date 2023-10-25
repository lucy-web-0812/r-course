
library(tidyverse)

# Wes (multi-axes) --------------------------------------------------------

set.seed(123)
dat = tibble(
  x = 1:100,
  c = rep(c(rep(0, 20), rep(1, 5)),4)
) %>% 
  mutate(y = jitter(c*10) %>% abs())

dat %>%
  ggplot(aes(x, y)) +
  geom_point(aes(color = "points")) +
  geom_step(aes(x,c*12, color = "steps")) +
  scale_y_continuous(sec.axis = sec_axis(~ ./12))

## but broadly a bad idea...
## https://blog.datawrapper.de/dualaxis/

dat %>% 
  ggplot(aes(x,y)) +
  geom_line() +
  geom_point(size = 2, aes(color = factor(c)))

dat %>% 
  pivot_longer(c:y) %>% 
  ggplot(aes(x, value)) +
  geom_line() +
  facet_grid(rows = vars(name), scales = "free_y", space = "free") +
  scale_y_continuous(breaks = scales::pretty_breaks(3))

# Seth (polar coords) -----------------------------------------------------

set.seed(123)
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



# Lizzie (line of best fit-ish) -------------------------------------------

## generally--

mtcars %>% 
  ggplot(aes(disp, mpg)) +
  geom_point() +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 3))

## gaussians--

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
