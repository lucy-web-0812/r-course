
plotDat = sumData %>% 
  left_join(dffit, by = "pixel", suffix = c("_meas","_mod")) %>% 
  tibble() %>% 
  select(pixel, intensity_meas, intensity_mod) %>% 
  mutate(resid= intensity_mod-intensity_meas)

plotDat %>% 
  mutate(resid = resid+2e5) %>% 
  pivot_longer(-pixel) %>% 
  ggplot()+
  geom_line(aes(pixel, value, colour = name))+
  scale_y_continuous(sec.axis = sec_axis(trans = function(x){x-2e5}))

plotDat %>% 
  pivot_longer(-pixel) %>%
  mutate(grp = ifelse(name == "resid", "Residuals", "Data")) %>% 
  ggplot()+
  geom_line(aes(pixel, value, colour = name))+
  scale_y_continuous(breaks = scales::pretty_breaks(3))+
  scale_colour_manual(values = c("intensity_meas" = "black",
                                 "intensity_mod" = "red",
                                 "resid" = "grey50"))+
  facet_grid(grp~., scales = "free_y", space = "free_y")+
  theme_bw()
