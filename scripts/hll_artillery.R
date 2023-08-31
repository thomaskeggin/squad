# set ----
library(tidyverse)

# load data ----
# american artillery
arty <- data.frame(m = c(1600,1500,1400,1300,1200,1100,1000,900,800,700,600,
                         500,400,300,200,100),
                   mil = c(622,646,670,693,717,741,764,788,812,836,859,883,
                           907,930,954,978))


# model ----
fit <- lm(mil ~
            m,
    data = arty)

# estimate range accuracy
acc <- mean(sqrt(fit$residuals^2))

# plot relationship with accuracy margin
arty <-
  arty %>% 
  mutate(prediction = predict(fit),
         upper      = mil+acc,
         lower      = mil-acc)


         ggplot(arty, aes(x = m)) +
  geom_point(aes(y = mil), size = 3) +
  geom_line(aes(y = prediction), linewidth = 1) +
  #geom_ribbon(aes(ymin=lower,ymax=upper),fill = "lightblue",alpha = 0.5) +
  theme_classic()
  

# calculate ----
target <- 625

x <- data.frame(m=c(target))
y <- predict(fit, newdata = x)
round(mean(c(y-acc,y+acc)))


