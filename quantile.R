library(tidyverse)
d <- tibble(
  predictor = c(1, 2, 3, 4, 5, 6, 7),
  outcome = c(1.5, 2.3, 2.8, 4.1, 5.3, 0, 6.8)
)

ggplot(d, aes(x = predictor, y = outcome)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_quantile(quantiles = 0.5) +
  annotate("text", x = 6, y = 4, label = "OLS", color = "red", hjust = 0) +
  annotate("text", x = 6, y = 5.8, label = "Median Regression", 
           color = "blue", hjust = 0)

lm <- lm(outcome ~ predictor, data = d)
library(quantreg)
rm <- rq(outcome ~ predictor, data = d, tau = 0.5)

AIC(lm, rm)

library(gtsummary)
tbl_merge(
  tbls = list(
    tbl_regression(lm),
    tbl_regression(rm)
  ),
  tab_spanner = c("**OLS**", "**Median Regression**")
)

library(sjPlot)
plot_models(lm, rm, 
            show.values = TRUE)

data(engel)

ggplot(engel, aes(income, foodexp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_quantile(quantiles = 0.5, color = "blue") +
  annotate("text", x = 4000, y = 2000, label = "OLS", color = "red", hjust = 0) +
  annotate("text", x = 2800, y = 2500, label = "Median Regression", 
           color = "blue", hjust = 0)

lm <- lm(foodexp ~ income, data = engel)
library(quantreg)
rm <- rq(foodexp ~ income, data = engel, tau = 0.5)

AIC(lm, rm)

library(gtsummary)
tbl_merge(
  tbls = list(
    tbl_regression(lm) ,
    tbl_regression(rm)
  ),
  tab_spanner = c("**OLS**", "**Median Regression**")
)


ggplot(engel, aes(income, foodexp)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_quantile(quantiles = 0.5, color = "blue") +
  geom_quantile(quantiles = seq(0.1, 0.9, 0.1), 
                color = "grey", alpha = 0.3) +
  annotate("text", x = 4000, y = 2000, label = "OLS", color = "red", hjust = 0) +
  annotate("text", x = 3200, y = 2500, label = "Median Regression", 
           color = "blue", hjust = 0)
  
    