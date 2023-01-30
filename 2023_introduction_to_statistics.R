#PACKAGES----

library(tidyverse)
library(here)
library(kableExtra)

darwin <- read_csv(here("data", "darwin.csv"))

#__________________----


#CHECK DATA ----

colnames(darwin)

glimpse(darwin)

##check data is in a tidy format----

head(darwin)

##check variable names----

colnames(darwin)

##check for duplicates----

darwin %>%
  duplicated()
sum()

## use summarise to make calculations----

darwin %>%
  summarise(min=min(height, na.rm=TRUE),
            max=max(height, na.rm=TRUE))

##check for typos----

darwin %>%
  distinct(type)

##check for missing values----

darwin %>%
  is.na() %>%
  sum()

## quick summary----

summary(darwin)

#__________________----


#VISUALISATION----

darwin %>%
  ggplot(aes(x=type,
             y=height)) +
  geom_point()

darwin %>%
  ggplot(aes(x=type,
             y=height)) +
  geom_boxplot()

#__________________----


#COMPARING GROUPS----

darwin %>%
  group_by(type) %>%
  summarise(mean=mean(height),
            sd=sd(height))

##make a new object----

darwin_summary <-darwin %>% 
  group_by(type) %>% 
  summarise(mean=mean(height),
            sd=sd(height))

##make a summary plot----

darwin_summary %>% 
  ggplot(aes(x=type,
             y=mean))+
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd))+
  theme_bw()

##use kabelExtra----

darwin_summary %>% 
  kbl(caption="Summary statistics of crossed and selfed maize plants") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

#__________________----


#ESTIMATION----

##pivot data----

darwin_wide <- darwin %>%
  pivot_wider(names_from = type, values_from = height) %>%
  mutate(difference = Cross - Self)

difference_summary <- darwin_wide %>%
  summarise(mean=mean(difference),
          sd=sd(difference),
          n=n())
difference_summary

##standard error----

difference_summary %>%
  mutate(se = sd/sqrt(n))

#__________________----


#UNCERTAINTY----

##Create a sequence of 100 equally spaced numbers between -4 and 4----
x <- seq(-4, 4, length=100)

#create a vector of values that shows the height of the probability distribution
#for each value in x
y <- dnorm(x)

##plot x and y as a scatterplot with connected lines (type = "l") and add----
#an x-axis with custom labels
plot(x,y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
axis(1, at = -3:3, labels = c(" ", "0.18", " ", "2.62", " ", "5.06", " "))
# axis title "difference in height(inches)"

##confidence intervals----

lowerCI <- 2.62-(2*1.22)

upperCI <- 2.62+(2*1.22)

lowerCI
upperCI

#__________________----