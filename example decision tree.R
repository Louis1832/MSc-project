install.packages("rpart")
install.packages("rpart.plot")
install.packages("ggplot2")

library(ggplot2)
library(rpart)
library(rpart.plot)

set.seed(52)
sales_df <- data.frame(
  Price = round(rlnorm(100), 2)
)

sales_df$Sales <- (12 * sales_df$Price^(2/3) + 4) + round(rlnorm(100, sdlog = 2))

model <- rpart(Sales ~ Price, data = sales_df, method = "anova")

rpart.plot(model, type = 2, extra = 101, fallen.leaves = FALSE)

ggplot(aes(Price, Sales), data = sales_df) + 
  geom_point() +
  geom_vline(xintercept = 3.4) +
  geom_vline(xintercept = 1.1) + 
  geom_vline(xintercept = 0.41) +
  geom_vline(xintercept = 0.46)

abline(v=1)
