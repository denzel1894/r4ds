library(tidyverse)
x = data.frame(a = rnorm(50,50,10))
ggplot(x, aes(x = a)) + geom_dotplot()

# ChatGPT----
set.seed(123)
n <- 1000
x <- rnorm(n)
epsilon <- rnorm(n, mean=0, sd = 1)
beta0 <- 1
beta1 <- 2
y <- beta0 + beta1 * x + epsilon

likelihood <- function(params) {
  beta0 <- params[1]
  beta1 <- params[2]
  sigma <- params[3]
  
  log_likelihood <- -(n/2) * log(2 * pi * sigma^2) - (1/(2 * sigma^2)) * sum((y - beta0 - beta1 * x)^2)
  return(-log_likelihood)  # 返回对数似然函数的相反数，因为optim函数默认进行最小化
}

# 最大似然估计
initial_params <- c(0, 0, 1)  # 初始参数值
result <- optim(initial_params, likelihood, method = "BFGS")  # 使用BFGS算法进行优化
estimated_params <- result$par  # 估计的参数值
sigma_hat <- sqrt(1 / estimated_params[3])  # 估计的标准差

# 打印结果
cat("真实参数：beta0 =", beta0, "beta1 =", beta1, "\n")
cat("估计参数：beta0_hat =", estimated_params[1], "beta1_hat =", estimated_params[2], "\n")
cat("估计的标准差：sigma_hat =", sigma_hat, "\n")


#计算残差
residuals <- y - estimated_params[1] - estimated_params[2] * x

# 绘制残差图
plot(x, residuals, xlab = "x", ylab = "Residuals", main = "Residual Plot")

# 绘制QQ图
qqnorm(residuals)
qqline(residuals)

# 进行假设检验
shapiro.test(residuals)


# 计算协方差矩阵
hessian <- numDeriv::hessian(likelihood, estimated_params)
cov_matrix <- solve(hessian)

# 提取参数的标准差
std_errors <- sqrt(diag(cov_matrix))

# 提取参数间的协方差
cov_beta0_beta1 <- cov_matrix[1, 2]
cov_beta0_sigma <- cov_matrix[1, 3]
cov_beta1_sigma <- cov_matrix[2, 3]

# 打印参数估计和标准差
cat("Parameter Estimates:\n")
cat("beta0:", estimated_params[1], "\n")
cat("beta1:", estimated_params[2], "\n")
cat("sigma:", estimated_params[3], "\n")
cat("\nStandard Errors:\n")
cat("beta0:", std_errors[1], "\n")
cat("beta1:", std_errors[2], "\n")
cat("sigma:", std_errors[3], "\n")
cat("\nCovariances:\n")
cat("Cov(beta0, beta1):", cov_beta0_beta1, "\n")
cat("Cov(beta0, sigma):", cov_beta0_sigma, "\n")
cat("Cov(beta1, sigma):", cov_beta1_sigma, "\n")
