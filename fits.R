fit_data <- df_clean %>% 
  select_if(is.numeric) %>% 
  drop_na()

fit <- lm(ft_per_g ~ ., fit_data)
summary(fit)


library(glmnet)

split <- sample(nrow(fit_data), size = round(nrow(fit_data) * .7))

train <- model.matrix(ft_per_g ~ ., data = fit_data[split, ])[, -1]
y_train <- fit_data$ft_per_g[split]

test <- model.matrix(ft_per_g ~ ., data = fit_data[-split, ])[, -1]
y_test <- fit_data$ft_per_g[-split]



lasso <- cv.glmnet(train, y_train, alpha = 1)
plot(lasso)

bestlam <- lasso$lambda.min

preds <- predict(lasso, s = bestlam, newx = test)

rmse <- sqrt(mean((y_test - preds)^2))
rmse

full_matrix <- model.matrix(ft_per_g ~ ., data = fit_data)[, -1]
full_lasso <- glmnet(full_matrix, fit_data$ft_per_g, alpha = 1, lambda = bestlam)
predict(full_lasso, s = bestlam, type = "coefficients") 
