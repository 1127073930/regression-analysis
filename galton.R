# ===============================
# 应用回归分析 - 一元线性回归建模
# ===============================

# 加载必要的包
library(tidyverse)  # 数据操作与可视化

# -------------------------------
# 1. 读取数据
# -------------------------------
# 数据文件位于当前工作目录下
setwd("C:\\Users\\Lenovo\\Desktop\\regression-analysis")
galton_raw = read_csv("galtonfamiliesall.csv")

# 查看数据结构
head(galton_raw)

# -------------------------------
# 2. 处理数据
# -------------------------------
# 计算每个孩子的父母平均身高，并筛选需要的变量
galton_processed = galton_raw %>%
  mutate(
    MidParent = (Father + Mother) / 2,   # 父母平均身高
    Child = as.factor(Child)             # 性别转为因子（可选，用于后续多元）
  )

# 查看处理后的数据
summary(galton_processed)

# -------------------------------
# 3. 探索数据
# -------------------------------
# 计算相关系数
cor_xy = cor(galton_processed$MidParent, galton_processed$Height)
cat("父母平均身高与子女身高的相关系数：", round(cor_xy, 4), "\n")

# 绘制散点图 + 回归线
scatter_plot = ggplot(galton_processed, aes(x = MidParent, y = Height)) +
  geom_point(alpha = 0.5, color = "steelblue") +
   labs(
    title = "父母平均身高与子女身高的关系",
    x = "父母平均身高（英寸）",
    y = "子女身高（英寸）") 

print(scatter_plot)

# -------------------------------
# 4. 拟合一元线性回归模型
# -------------------------------
model = lm(Height ~ MidParent, data = galton_processed)

# -------------------------------
# 5. 查看模型结果，提取重要信息
# -------------------------------
# 模型概要
model_summary = summary(model)
print(model_summary)

# 重要指标
model$coefficients # 系数

model$residuals # 残差
rstudent(model) # 计算学生化残差

model$fitted.values # 因变量的拟合值

# -------------------------------
# 6. 输出诊断图像
# -------------------------------
plot(model)

# -------------------------------
# 7. 预测
# -------------------------------
# 假设我们想预测父母平均身高分别为 65, 68, 70, 72 英寸时子女的平均身高
new_data = tibble(MidParent = c(65, 68, 70, 72))
predictions = predict(model, newdata = new_data, interval = "confidence", level = 0.95)
print(predictions)

# 输出预测结果
result = new_data %>%
  bind_cols(as_tibble(predictions)) %>%
  rename(Pred_Height = fit, Lower_CI = lwr, Upper_CI = upr)

cat("\n===== 预测结果（置信水平95%） =====\n")
print(result)

# -------------------------------
# 8. 如何提高模型效果？
# -------------------------------

# 多元回归
multi_model = lm(Height ~ Father + Mother + Child, data = galton_processed)
summary(multi_model)

