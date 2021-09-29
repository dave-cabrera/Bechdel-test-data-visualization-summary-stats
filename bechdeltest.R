bech <- read.csv('bechdel87(3).csv')
View(bech)
library(ggplot2)

# Pass/Fail by decade
ggplot(data = bech,
       mapping = aes(x = Decade,
                     fill = Btest)) +
  geom_bar(position = 'fill')

# Pass/Fail by decade and reason why the movie failed
ggplot(data = bech,
       mapping = aes(x = Decade,
                     fill = Btest5)) +
  geom_bar(position = 'fill')


ggplot(data = bech,
       mapping = aes(x = Btest)) +
        geom_bar(fill = 'turquoise')

bech %>% count(Btest)


# Histogram showing why movies failed the Bechdel test
ggplot(data = bech,
       mapping = aes(x = Btest5)) +
  geom_bar(fill = 'blue')

# Histogram showing budgets of the movies
ggplot(data = bech,
       mapping = aes(x = budget13)) +
  geom_histogram(fill = 'purple')

mean(bech$budget13)
# Average budget is 55.46 million (adjusted, 2013)





library(dplyr)

# The average, median, and SD of ratings for movies that passed vs failed
bech %>%
  filter(!is.na(averating)) %>%
  group_by(Btest) %>%
  summarise(meanrating = mean(averating),
            medianrating = median(averating),
            sdrate = sd(averating))

# Average, media, and SD of budget
bech %>%
  group_by(Btest) %>%
  summarise(meanbudget = mean(budget13),
            medianbudget = median(budget13),
            sdbudget = sd(budget13))

# Domestic grossing mean, median, and sd 
bech %>%
  filter(!is.na(domgross13)) %>%
  group_by(Btest) %>%
  summarise(meangrossing = mean(domgross13),
            mediangrossing = median(domgross13),
            sdgrossing = sd(domgross13))

# Budget vs. domestic grossing scatterplot
ggplot(data = bech,
       mapping = aes(x = budget13,
                     y = domgross13,
                     color = Btest)) +
  geom_point()

b2 <- bech %>%
  mutate(domprofit13 = 100 * (domgross13 - budget13) / budget13,
         intprofit13 = 100 * (intgross13 - budget13) / budget13)

View(b2)

# Mean, median, and sd for domestic profit
b2 %>%
  filter(!is.na(domprofit13)) %>%
  group_by(Btest) %>%
  summarise(mean_domprofit = mean(domprofit13),
            median_domprofit = median(domprofit13),
            sd_domprofit = sd(domprofit13))

# International profit mean, median, and sd
b2 %>%
  filter(!is.na(intprofit13)) %>%
  group_by(Btest) %>%
  summarise(mean_intprofit = mean(intprofit13),
            median_intprofit = median(intprofit13),
            sd_intprofit = sd(intprofit13))
