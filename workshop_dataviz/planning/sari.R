
# Light-Absorbing Brown Carbon Aerosol Constituents 
# from Combustion of Indonesian Peat and Biomass
# https://pubs.acs.org/doi/full/10.1021/acs.est.7b00397

library(tidyverse)

# Figure 1 ----------------------------------------------------------------

# A chromatograph - would probably need some data to help with that!

# Figure 2 ----------------------------------------------------------------

# Scatter plot

# mangle some data
tibble(mtcars) %>%
  mutate(vs = if_else(vs == 1, "near-UV", "visible"),
         am = if_else(am == 1, "A", "B")) %>%
  # plot
  ggplot(aes(disp, hp)) +
  geom_point(aes(size = drat, color = factor(vs)), 
             shape = 1, stroke = 1) +
  guides(size = guide_none()) +
  facet_grid(am~.) +
  theme_bw() +
  labs(x = "m/z", y = "DBE", color = NULL) +
  theme(legend.position = c(.1, .9),
        legend.background = element_blank()) +
  scale_color_manual(values = c("black", "red"))

# Figure 3 ----------------------------------------------------------------

# Bar Chart

# make some dummy data
dummy1 <- tibble(y = c(100, 50, 25, 10, 5, 2, 1, .5, 1, .9),
                 c = c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J"))

dummy2 <- bind_rows(
  dummy1 %>% mutate(x = "1"),
  dummy1 %>% mutate(x = "2"),
  dummy1 %>% mutate(x = "3")
)

# plot...

ggplot(dummy2, aes(x = x, y = y, fill = c)) +
  geom_col()

# however, some categories can't be read from this plot! Try lumping?

dummy3 = dummy2 %>% 
  mutate(c2 = if_else(y > 2, c, "Other"))

others = dummy3 %>% 
  filter(c2 == "Other") %>% 
  pull(c) %>% 
  unique() %>% 
  paste(collapse = ", ")

ggplot(dummy3, aes(x = x, y = y, fill = c2)) +
  geom_col() +
  labs(caption = paste("* 'Other' includes", others)) +
  theme_bw() +
  labs(y = openair::quickText("Concentration (ug/m3)"),
       x = "Experiment",
       fill = "NULL") +
  scale_y_continuous(expand = expansion(mult = c(0,.1)))


# Figure 4 ----------------------------------------------------------------

# Multiple Axes
# NB: Multiple y axes are broadly a bad idea, but this is how you'd do it:

ggplot(iris, aes(x = Petal.Length)) +
  geom_point(aes(y = Sepal.Length, color = "Length")) +
  geom_point(aes(y = Sepal.Width*2, color = "Width")) +
  scale_y_continuous(sec.axis = sec_axis(~./2, name = "Sepal.Width")) +
  theme_bw() +
  theme(axis.line.y.left = element_line(color = "salmon"),
        axis.line.y.right = element_line(color = "cadetblue"),
        legend.position = "top")

# a better alternative is probably just faceting...

iris %>% 
  pivot_longer(contains("Sepal")) %>% 
  ggplot(aes(x = Petal.Length, y = value)) +
  geom_point(aes(color = name)) +
  facet_grid(name~., scales = "free") +
  theme_bw() +
  theme(legend.position = "none") +
  labs(y = "Dimension (cm)", x = "Petal Length (cm)")
