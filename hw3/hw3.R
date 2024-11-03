library(tidyverse)

# 讀取 CSV 數據
dt <- read.csv("./Equity_Premium.csv")

# 將 Time 轉換為日期格式
dt$Time <- as.Date(paste0(dt$Time, "01"), format="%Y%m%d")

# 使用 (X'X)^(-1)*(X'Y) 計算 beta 的函數
calculate_beta_direct <- function(X, y) {
  X <- as.matrix(cbind(1, X))  # 添加截距
  solve(t(X) %*% X) %*% t(X) %*% y
}

# 使用 FWL 定理計算 beta 的函數（修正版）
calculate_beta_fwl <- function(X, y) {
  X <- as.matrix(X)  # 確保 X 是矩陣
  n <- ncol(X)
  betas <- numeric(n + 1)
  betas[1] <- mean(y)  # 截距
  
  for (i in 1:n) {
    X_i <- X[, i]
    X_not_i <- X[, -i, drop = FALSE]  # 確保 X_not_i 是矩陣
    
    # 將 X_i 對其他 X 進行回歸
    res_X <- lm(X_i ~ X_not_i)$residuals
    
    # 將 y 對其他 X 進行回歸
    res_y <- lm(y ~ X_not_i)$residuals
    
    # 將 res_y 對 res_X 進行回歸
    betas[i + 1] <- coef(lm(res_y ~ res_X))[2]
  }
  
  betas
}

# 準備數據
X <- dt[, c("x_dfy", "x_infl", "x_svar", "x_tms", "x_tbl", "x_dfr")]
y <- dt$y

# 計算 beta
beta_direct <- calculate_beta_direct(X, y)
beta_fwl <- calculate_beta_fwl(X, y)

# 使用 lm() 進行線性回歸以作比較
model <- lm(y ~ x_dfy + x_infl + x_svar + x_tms + x_tbl + x_dfr, data = dt)

# 獲取係數估計值
coef_estimates <- coef(model)

# 計算 T*（樣本大小）
T_star <- nrow(dt)

# 計算 y 的樣本均值
y_bar <- mean(y)

# 計算 R 平方
R_squared <- summary(model)$r.squared

# 計算均方誤差（MSE）
mse <- mean(residuals(model)^2)

# 打印結果
cat("係數估計值 (lm)：\n")
print(coef_estimates)

cat("\n係數估計值（直接方法）：\n")
print(beta_direct)

cat("\n係數估計值（FWL 定理）：\n")
print(beta_fwl)

cat("\n樣本大小 (T*):", T_star)
cat("\ny 的樣本均值:", y_bar)
cat("\nR 平方:", R_squared)
cat("\n均方誤差:", mse)

# 創建比較表
comparison_table <- data.frame(
  Variable = c("Intercept", colnames(X)),
  lm = coef_estimates,
  Direct = beta_direct,
  FWL = beta_fwl
)

# 打印比較表
print(comparison_table)

# 繪製殘差圖
ggplot(data.frame(residuals = residuals(model)), aes(x = residuals)) +
  geom_histogram(binwidth = 0.01, fill = "blue", alpha = 0.7) +
  theme_minimal() +
  labs(title = "殘差直方圖", x = "殘差", y = "計數")

# 保存圖片（如果需要保存，請取消注釋）
# ggsave("residuals_histogram.png", width = 8, height = 6)