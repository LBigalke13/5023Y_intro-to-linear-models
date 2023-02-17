# STUDENT'S T-TEST ----

lm(y ~ 1, data = darwin)

lm (height ~ 1, data = darwin)


x <- seq(-4, 4, length=100)
hx <- dnorm(x)

degf <- c(1, 3, 8, 30)
colors <- c("red", "blue", "darkgreen", "gold", "black")
labels <- c("df=1", "df=3", "df=8", "df=30", "normal")

plot(x, hx, type="l", lty=2, xlab="x value",
     ylab="Density", main="Comparison of t Distributions")

for (i in 1:4){
  lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
}

legend("topright", inset=.05, title="Distributions",
       labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)


df <- c(1:30)

# map_dbl forces returned values to be a single vector of numbers (rather than a list)
critical_t <- map_dbl(df, ~qt(p=0.05/2, df=.x, lower.tail=FALSE))

tibble(df,critical_t) %>% 
  ggplot(aes(x=df, y=critical_t))+
  geom_point()+
  geom_line()+
  geom_hline(aes(yintercept=1.96), linetype="dashed", colour="red")+
  labs(x= "Degrees of Freedom",
       y= expression(paste("Critical value of ", italic("t"))))

lsmodel1 <- lm(height ~ type, data = darwin)

summary(lsmodel1)


tidy_model1 <- broom::tidy(lsmodel1)

tidy_model1[[2,2]] / tidy_model1[[2,3]]

#__________________----

#PAIRED T----

lsmodel_darwin <- lm(height ~ type + factor(pair), data = darwin)
summary(lsmodel_darwin)

lm(height ~ type + factor(pair), data = darwin) %>%
  broom::tidy(.,conf.int=T) %>%
  slice(1:2) #just show first two rows

m1 <- lm(height ~ type, data = darwin) %>%
  broom::tidy(., conf.int=T) %>%
  slice(2:2) %>%
  mutate(model = "unpaired")

m2 <- lm(height ~ type + factor(pair), data = darwin) %>%
  broom::tidy(., conf.int=T) %>%
  slice(2:2) %>%
  mutate(model = "paired")

rbind(m1, m2) %>%
  ggplot(aes(model, estimate)) +
  geom_pointrange(aes(ymin = conf.high, ymax = conf.low)) +
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  theme_minimal() +
  coord_flip()

#__________________----

#REPEATABILITY----

set.seed(1234)

myList <- vector("list",20)
y <- tibble()

for (i in 1:length(myList)) {
  x <- rnorm(n = 12, mean = 2.6, sd = 2.83)
  data <- tibble(x)
  temp <- lm(x ~ 1, data = data) %>%
    broom::tidy(conf.int=T)
  y <- rbind(y, temp)
  
}

y$`experiment number` <- rep(1:20)

#the new dataframe y contains the results of 20 new experiments

#__________________----

#EXPERIMENTAL REPEATABILITY----

y %>% 
  mutate(`p value < 0.05` = if_else(p.value > 0.049, "non-significant", "significant")) %>% 
  group_by(`p value < 0.05`) %>% 
  summarise(`number of experiments`=n())

y %>% 
  ggplot(aes(x=`experiment number`, y=estimate))+
  geom_pointrange(aes(ymin = conf.low, ymax=conf.high))+
  labs(y = "Estimated mean effect of outcrossing")+
  geom_hline(linetype="dashed", yintercept=0.05)+
  theme_minimal()

#__________________----