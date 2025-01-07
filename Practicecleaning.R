library(tidyverse)
dim(starwars)
str(starwars)
names(starwars)
class(starwars$hair_color)
?length(starwars$hair_color)
unique(starwars$hair_color)
view(sort(table(starwars$hair_color),decreasing = T))
barplot(table(starwars$hair_color))

# df[rows you want to select, columns you want to select]
# leaving blank is equivalent to all
starwars[is.na(starwars$hair_color), ]

is.na(starwars$hair_color)

summary(starwars$height)
boxplot(starwars$height)
hist(starwars$height)


starwars$gender <- as.factor(starwars$gender)
#levels(starwars$gender)
# In levels you have feminine as 1 and masculine of level 2, to change this:
starwars$gender <- factor((starwars$gender), levels=c("masculine","feminine"))
levels(starwars$gender)

starwars %>% select(name, height, ends_with("color")) %>%
              filter(hair_color %in% c("blond","brown") & height < 180) 

mean(starwars$height, na.rm = T)

starwars %>%
  select(name,gender,hair_color,height) %>%
  filter(!complete.cases(.)) %>%
  drop_na(height) %>% # removes cases with NA in height since probably wasn't able to get
  muatate(hair_color = replace_na(hair_color, "none")) # shown to be NA where no hair

Names <- c("Peter","John","Andrew","Peter")
Age <- c(22,33,44,22)



friends <- data.frame(Names, Age)
duplicated(friends)
# [Rows to select, columns to select] 
# Give rows which are not duplicated, the next uses tidy verse's distinct function
# to do the same thing
friends[!duplicated(friends), ]
friends %>% distinct() %>% view()

starwars %>% 
  select(name,gender) %>% 
  mutate(gender = recode(gender,"masculine" = 1, "feminine" = 2))



  
