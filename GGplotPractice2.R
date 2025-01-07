library(tidyverse)
# (1 categorical and one numeric)
# Histogram
msleep %>% drop_na(vore) %>%
  ggplot(aes(sleep_total))+
  geom_histogram()+
  facet_wrap(~vore)

# Density plot (basically a smooth histogram)

msleep %>% 
  drop_na(vore) %>%
  ggplot(aes(sleep_total, fill = vore))+
  geom_density()+
  facet_wrap(~vore)+
# OR USING COLOUR INSTEAD:
msleep %>% 
  drop_na(vore) %>%
  ggplot(aes(sleep_total))+
  geom_density(alpha=0.5)
  