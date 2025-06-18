heart_data=read.csv("C:\\Users\\HP\\Downloads\\heart.csv")
library(ggplot2)
ggplot(heart_data, aes(x=age)) + 
  geom_histogram(fill="#0072BD", bins=15) +
  labs(title="Age Distribution of Patients")
library(corrplot)
# Assuming heart_data is your dataframe
numeric_cols <- sapply(heart_data, is.numeric)
corrplot(cor(heart_data[, numeric_cols]), method="color", type="upper")
ggplot(heart_data, aes(x=target, y=chol, fill=target)) + 
  geom_boxplot() + 
  scale_fill_manual(values=c("#4E84C4", "#D16103"))
plot(heart_data$age, heart_data$thalach, col=ifelse(heart_data$thalach<90, "red", "blue"))