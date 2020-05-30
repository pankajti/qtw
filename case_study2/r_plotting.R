

library(tidyverse)
library(ggplot2)

female_table = read_csv('/Users/pankaj/dev/git/smu/qtw/case_study2/transformation/processed_female.csv')

ggplot(female_table, mapping = aes(y= AGE, x= YEAR))+
  geom_point()

ggplot(female_table, mapping = aes(x= AGE, y= YEAR))+
  geom_count()


female_table %>%   mutate (YEAR_R = as.factor(YEAR))   %>%
ggplot(mapping = aes(x= YEAR_R, y= AGE))+
  geom_boxplot()


female_table %>%   mutate (YEAR_R = as.factor(YEAR))   %>%
ggplot(  mapping = aes(x= AGE , fill = YEAR_R))+
 geom_bar()


female_table %>%   mutate (YEAR_R = as.factor(YEAR))   %>%
  ggplot(  mapping = aes(x= AGE , color = YEAR_R ))+
  geom_density()

p <- ggplot(female_table, aes(sample = AGE))
p + stat_qq() + stat_qq_line()

p <-  female_table %>%   mutate (YEAR_R = as.factor(YEAR)) %>%
  filter(AGE>10 ) %>% 
  ggplot(aes(sample = AGE, color = YEAR_R))
p + stat_qq() + stat_qq_line()

female_table %>%   mutate (YEAR_R = as.factor(YEAR))   %>%
  ggplot(  mapping = aes(x= AGE , color = YEAR_R ))+
  geom_qq_line()


female_table %>%   mutate (YEAR_R = as.factor(YEAR))   %>%
  ggplot(  mapping = aes(x= AGE , fill = YEAR_R))+
  geom_bar()

female_table %>%   mutate (YEAR_R = as.factor(YEAR))   %>%
  ggplot(  mapping = aes(x = YEAR_R, fill = AGE))+
  geom_bar()



df = melt(female_table %>% select(YEAR, AGE))
 
ggplot(df, aes(x = ind, y = values)) +
  geom_boxplot()


ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()


ggplot(mpg, aes(displ, fill = drv)) +
  geom_histogram(binwidth = 0.5) +
  facet_wrap(~drv, ncol = 1)

