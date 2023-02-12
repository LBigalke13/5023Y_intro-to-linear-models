#PACKAGES----

library(tidyverse)
library(GGally)
library(emmeans)
library(performance)
library(broom)
install.packages("broom.helpers")

#__________________----


#LINEAR MODEL ANALYSIS----

lsmodel0 <- lm(formula = height ~ 1, data = darwin)

#__________________----


#SUMMARIES FOR MODELS----

##broom----

# broom::tidy()  summarizes information about model components
# broom::glance()  reports information about the entire model
# broom::augment()  adds informations about individual observations to a dataset and it can be used to model predictions onto a new dataset

##model summary----

summary(lsmodel0)

mean(darwin$height)

##compare means----

lsmodel1 <- lm(height ~ type, data = darwin)

# note that the following is identical
# lsmodel1 <- lm(height ~ 1 + type, data = darwin)

broom::tidy(lsmodel1)

darwin %>%
  group_by(type) %>%
  summarise(mean = mean(height))

summary(lsmodel1)

darwin %>%
  ggplot(aes(x = type,
             y = height,
             colour = type)) +
  geom_jitter(alpha = 0.5,
              width = 0.1) +
  stat_summary(fun = mean,
               size = 1.2) +
  theme_bw()

#__________________----


#CONFIDENCE INTERVALS----

confint(lsmodel1)

#__________________----


#ANSWERING THE QUESTIONS----

GGally::ggcoef_model(lsmodel1,
                     show_p_values=FALSE, 
                     conf.level=0.95)

broom::tidy(lsmodel1, conf.int=T, conf.level=0.99)

##getting the other treatment mean and standard error----

darwin %>% 
  mutate(type=factor(type)) %>% 
  mutate(type=fct_relevel(type, c("Self", "Cross"))) %>% 
  lm(height~type, data=.) %>% 
  broom::tidy()

##emmeans----

means <- emmeans::emmeans(lsmodel1, specs = ~ type)

means

means %>% 
  as_tibble() %>% 
  ggplot(aes(x=type, 
             y=emmean))+
  geom_pointrange(aes(
    ymin=lower.CL, 
    ymax=upper.CL))

#__________________----


#ASSUMPTION CHECKING----

performance::check_model(lsmodel1)

##normal distribution----

performance::check_model(lsmodel1, check=c("normality","qq"))

plot(lsmodel1, which=c(2,2))

##equal variance----

performance::check_model(lsmodel1, check="homogeneity")

plot(lsmodel1, which=c(1,3))

##outliers----

performance::check_model(lsmodel1, check="outliers")

plot(lsmodel1, which=c(4,4))

#__________________----


#SUMMARY----

darwin %>% 
  ggplot(aes(x=type, 
             y=height))+
  geom_jitter(width=0.1, 
              pch=21, 
              aes(fill=type))+
  theme_classic()+
  geom_segment(aes(x=1, xend=2, y=20.192, yend=20.192-2.617), linetype="dashed")+
  stat_summary(fun.y=mean, geom="crossbar", width=0.2)

