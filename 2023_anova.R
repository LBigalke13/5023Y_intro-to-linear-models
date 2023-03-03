#__________________----

#MAIZE DATA----

lsmodel1 <- lm(height ~ type, data = darwin)

##THE ANOVE TABLE----

anova(lsmodel1)

pf(5.9395, 1, 28, lower.tail=FALSE)
#[1] 0.02141466

#__________________----

#TWO-WAY ANOVA----

lsmodel2 <- lm(height ~ type + as.factor(pair), data = darwin)

anova(lsmodel2)

#__________________----
