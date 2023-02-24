#__________________----

#PACKAGES ----

library(tidyverse)
library(rstatix)
library(performance)
library(patchwork)

#__________________----


#IMPORT DATA----

janka <- read_csv(here("data", "janka.csv"))

#__________________----


#ACTIVITY1: EXPLORATORY ANALYSIS----

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()

#__________________----


#ACTIVITY 2: CORRELATION - GENERATE PERSON'S R----

with(janka, cor(dens, hardness))

#__________________-----


#REGRESSION IN R----

janka_ls1 <- lm(hardness ~ dens, data = janka) 

# specify linear model method for line fitting

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm")

##SUMMARY----

summary(janka_ls1)

#__________________----


#ACTIVITY 3: MEAN CENTERED REGRESSION----

dens_mean <- janka %>%
  summarise(mean_dens=mean(dens))

janka %>%
  mutate(centered_dens = dens-pull(dens_mean)) %>%
  lm(hardness ~ centered_dens, data = .) %>%
  broom::tidy()

##CONFIDENCE INTERVALS----

confint(janka_ls1)

summary(janka_ls1)

#__________________----


#ASSUMPTIONS----

predict(janka_ls1)

resid(janka_ls1)

augmented_ls1 <- janka_ls1 %>%
  broom::augment()

augmented_ls1 %>%
  ggplot(aes(x=dens,
             y=.fitted)) +
  geom_line() +
  geom_point(aes(x=dens,
                 y=hardness)) + 
  geom_segment(aes(x=dens,
                   xend=dens,
                   y=.fitted,
                   yend=hardness),
               linetype='dashed', colour='red')

p1 <- augmented_ls1 %>%
  ggplot(aes(x=dens, y=hardness)) +
  geom_line() +
  ggtitle("Full Data")
# a line connecting all the data points in order

p2 <- augmented_ls1 %>%
  ggplot(aes(x=dens, y=.fitted)) +
  geom_line() + 
  ggtitle("Linear trend")
# plotting the fitted values against the independent e.g. our regression line

p3 <- augmented_ls1 %>%
  ggplot(aes(x=.fitted, y=.resid)) +
  geom_hline(yintercept=0, colour="white", size=5) +
  geom_line() +
  ggtitle("Remaining \npattern")
# plotting the residuals against the fitted values e.g. remaining variance

p1+p2+p3

model_plot <- function(data=augmented_ls1,
                       x="dens",
                       y="hardness",
                       title="Full data") {
  ggplot(aes(x=.data[[x]],
             y=.data[[y]]),
         data=data) +
    geom_line() +
    theme_bw() +
    ggtitle(title)
}

p1 <- model_plot()
p2 <- model_plot(y=".fitted", title="Linear prediction")
p3 <- model_plot(y=".resid", title="Remaining pattern")

model_plot(data=augmented_ls1,
           x="dens",
           y="hardness",
           title="Full data")

##NORMAL DISTRIBUTION----

plot(janka_ls1, which=c(2,2))

##EQUAL VARIANCE----

plot(janka_ls1, which=c(1,3))

##OUTLIERS----

plot(janka_ls1, which=c(4,5))

#__________________----


#REDICTION----

coef(janka_ls1)

# a + bx
-1160.49970 + 57.50667 * 65
# 2577.434

coef(janka_ls1)[1] +coef(janka_ls1) [2] * 65
# 2577.434

predict(janka_ls1, newdata=list(dens=c(22,35,65)))

##ADDING CONFIDENCE INTERVALS----

###STANDARD ERROR----

broom::augment(janka_ls1, newdata = tibble(dens=c(22,35,65)), se=TRUE)

###95% CONFIDENCE INTERVALS----

broom::augment(janka_ls1, newdata=tibble(dens=c(22,35,65)), interval="confidence")

emmeans::emmeans(janka_ls1, 
                 specs = "dens", 
                 at = list(dens = c(22, 35, 65)))

#__________________----


#ACTIVITY 4: PREDICTION----

pred_newdata <- broom::augment(janka_ls1, 
                               newdata=tibble(dens=c(22,35,65)))

janka %>% 
  ggplot(aes(x=dens, y=hardness))+
  geom_point()+
  geom_smooth(method="lm")+
  geom_point(data=pred_newdata, aes(y=.fitted, x=dens), colour="red")+
  geom_label(data=pred_newdata, (aes(y=(.fitted+10), x=(dens+3), label=round(.fitted, digits=0))))+
  theme_bw()+
  labs(x="Density", y="Timber Hardness")+
  scale_x_continuous(limits=c(20,80), expand=expansion(add=c(0,5)))

#__________________----
