# ==================================================
# 应用回归分析：一元线性回归案例
# 高尔顿家族身高数据：父母平均身高预测所有孩子身高
# ==================================================

# 加载所需包
library(tidyverse)   # 包含 dplyr, ggplot2, tidyr 等

# 1. 读取数据
galton = read_csv("galtonfamiliesall.csv") 
head(galton) # 查看部分数据，了解数据

# 2. 数据清洗与准备
#    - 计算父母平均身高 MidParent
#    - 重命名部分变量
galton_aug = galton %>%
  mutate(MidParent = (Father + Mother) / 2) %>%   # 计算父母平均身高
  rename(Gender = Child)
head(galton_aug)

# 查看数据摘要
galton_aug %>%
  summarise(
    n = n(),
    mean_height = mean(Height),
    sd_height = sd(Height),
    mean_midparent = mean(MidParent),
    sd_midparent = sd(MidParent)
  )

# 3. 绘制散点图 + 回归线
ggplot(galton_aug, aes(x = MidParent, y = Height)) +
  geom_point(alpha = 0.5, color = "darkgreen") +          # 散点（半透明避免重叠）
  labs(
    title = "父母平均身高与孩子身高的关系",
    x = "父母平均身高 (英寸)",
    y = "孩子身高 (英寸)"
  ) 

# 4. 计算相关系数
cor(galton_aug$MidParent, galton_aug$Height)

# 5. 拟合一元线性回归模型
model = lm(Height ~ MidParent, data = galton_aug)

# 6. 查看模型结果
summary(model)

model$coefficients

model$residuals

model$fitted.values

# 7. 输出诊断图像


# 8. 预测


# 9. 思考：如何提高模型效果

# 10. 多元线性回归模型
