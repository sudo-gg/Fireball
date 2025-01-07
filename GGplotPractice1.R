library(tidyverse)
BOD
# Single categorical variable = bar chart
# Single numeric = histogram
# Two numeric = Scatter plot (could also add colour for more)
# Categorical and numeric = Box plot
# Categorical and numerical = density plot


ggplot(data=BOD, mapping=aes(x=Time, y=demand))+ # Create the axis
         geom_point(size = 5)+ # Add points
  geom_line(colour="red") # Connect points with line

ggplot(BOD, aes(Time,demand))+ # same as above, ggplot assumes this order as above
  geom_point()+
  geom_line()

?CO2
summary(CO2)
names(CO2)
CO2 %>% ggplot(aes(conc, uptake, colour=Treatment))+
  geom_point()+
  geom_smooth(method=lm,se=F)+ # adds smooth linear model with standard error added but since method=lm now linear and se=F removes standard error
  facet_wrap(~Type)+ # makes multiple graphs depending on Type
  labs(title="CO2 uptake (𝜇mol/m^2 sec) from grass at each concentration")+
  theme_dark()

CO2 %>% ggplot(aes(Treatment,uptake))+
  geom_boxplot(aes(colour=Type)) +
  geom_point(aes(size = conc, colour=Plant))+
  coord_flip()+
  theme_bw()+
  labs(title="graph showing uptake of co2 from plants based on treatment and Type (country)")

view(mpg)
summary(mpg)
mpg %>% filter(cty<25) %>% # easy to filter when piping
  ggplot(aes(displ,cty)) +
  geom_point(aes(colour=drv,size=trans))+
  geom_smooth(method=lm)+
  facet_wrap(~year, nrow=2)+ # 2 rows of graphs
  labs(x="Engine size",y="MPG in the city", title="Fuel efficiency")+
  theme_minimal()

?fct_infreq
summary(msleep)
msleep %>% drop_na(vore) %>% # exclude NA from vore only as to not skew data
  ggplot(aes(fct_infreq(vore)))+ # orders factor types by number of observations
  # could also order by level of factor (inseq)/ order in they first appear (inorder)
  geom_bar(fill="#97B3C6")+ # bar chart is best for single categorical data
  theme_bw()+
  labs(x="Vore",y=NULL,title="Number of observations per order")

msleep %>% ggplot(aes(awake))+
  geom_histogram(binwidth = 2, aes(colour=vore))+ # binwidth groups more together
  theme_bw()+
  labs(x= "Total sleep", y=NULL, title="Histogram of total sleep")

# Scatter plot
msleep %>% filter(bodywt < 2) %>%
  ggplot(aes(bodywt, brainwt))+
  geom_point(aes(colour=sleep_total,size=awake))+
  geom_smooth()+
  labs(x="Body Weight", y="Brain Weight",title="Brain and body weight")

# Line graph (two numeric one category)

Orange %>% #filter(Tree != "2") %>%
  ggplot(aes(age,circumference, colour=Tree))+ # Better to do colour instead of facet wrap
  geom_point()+
  geom_smooth(se=F)+
  #facet_wrap(~Tree)+ # This graph is harder to compare in this case
  theme_bw()+
  labs(title="Tree age and circumference")

