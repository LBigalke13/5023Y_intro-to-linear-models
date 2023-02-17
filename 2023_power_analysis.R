#PACKAGES----

library(effectsize)
library(pwr)
library(simr)

#__________________----

#STANDARDISED EFFECT SIZE----

##COHEN'S D----

lm(height ~ type, data = darwin)

summary_darwin <- darwin %>%
  group_by(type) %>%
  summarise(mean = mean(height),
            sd = sd(height),
            n = n())
summary_darwin

summary_darwin %>%
  mutate(variance = sd^2) %>%
  mutate(per_sample_var = variance * (n-1)) %>%
  summarise(sd_pooled = sqrt(sum(per_sample_var)/sum(n-2)),
            mean_diff = diff(mean))

2.62/3.05

##COHEN'S D FROM A LINEAR MODEL----

#simple t-test
lsmodel1 <- lm(height ~ type, data = darwin)

t_to_d(2.437, df_error = 28, paired = F)

#paired t-test
lsmodel2 <- lm(height ~ type + factor(pair), data = darwin)

t_to_d(2.456, df_error = 27, paired = T)

#__________________----

#POWER----

#calculate the power of our model
test.power <- pwr.t.test(n = 15, d = 0.92, sig.level = 0.05)

#calculate the sample size we need to get power of 0.8 for a medium effect size (d = 0.5)
samp.size <- pwr.t.test(d = 0.5, sig.level = 0.05, power = 0.8)


sample_size <- seq(0,1000, by=10)

output <- list(length(sample_size))

for (i in 1:length(sample_size)) {
  
  sample <- pwr.t.test(n=sample_size[i], d=0.2, sig.level=0.05)
  output[[i]] <- sample
  
  #     if(i %% 1==0){    # The %% operator is the remainder, this handy if line prints a number every time it completes a loop
  #   print(i)
  #  }
}

sample_list <- as.list(sample_size)

names(output) <- sample_size

#  now you should be able to call any sample size and check statistical power!

# output$`30`
