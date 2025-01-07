library(tidyverse)
library(gapminder)
?msleep
glimpse(msleep)
# rename(new name = old name)
msleep %>% rename("conserv" = "conservation")

msleep %>% select(vore,name,everything())
#to a factor (base R)
msleep$vore <- as.factor(msleep$vore)
class(msleep$vore)
# back to a char using tidyverse
msleep %>% 
  mutate(vore=as.character(vore)) %>%
  glimpse()

names(msleep)

# 2:4 selects the 2nd, 3rd and 4th column, others are self explanatory
msleep %>%
  select(2:4, awake, starts_with("sleep"),contains("wt"))

# gives the unique outputs possible
unique(msleep$order)

msleep %>%
  filter((order == "Carnivora" | order == "Primates") & sleep_total > 8) %>%
  select(name,order,sleep_total) %>%
  arrange(-sleep_total) %>% # the negative goes from highest to lowest instead
  view()

# alternate to above

msleep %>%
  filter(order %in% c("Carnivora","Primates") & sleep_total > 8) %>%
  select(name,order,sleep_total) %>%
  arrange(order) %>% # Arranged in alphabetical order
  View

msleep %>%
  mutate(brainwt_grams = brainwt * 1000) %>%
  view # could have also overwritten brainwt instead of making a new variable

# conditional changes using if_else
# logical vector based on a conditon (vector of T & F's or NA)

msleep$brainwt > 0.01

size_of_brain <- msleep %>%
  select(name, brainwt) %>%
  drop_na(brainwt) %>%
  mutate(brainsize = if_else(brainwt>0.01,"large","small"))

# Recoding data

size_of_brain %>% 
  mutate(brainsize = recode(brain_size,"large"=1,"small"=2))
view(size_of_brain)

# Long data set instead of wide data set (wide is where there would be a 
# column for each year with the life expectancy as the row value)
data <- select(gapminder, country, year, lifeExp)

# To make wide

wide_data <- data %>%
  pivot_wider(names_from = year, values_from = lifeExp)
wide_data

long_data <- wide_data %>%
  pivot_longer(2:13, names_to = "year", values_to = "lifeExp") %>%
  view()



x <- "aaaabbbbb"
sub("a","c",x) # changes first appearance
gsub("a|b",'c',x) # changes all appearances and allows REGEX
