#logit regression only numbers
df1 <- read.csv('/Users/Kartozianstvo/Desktop/вышечка/cxg_2017/data.csv', sep=',')
fit <- glm(Target ~ AdvTime+AdvLoc+V_pres+V_past+V_fut+V_inf+V_predicative+V_subj+V_converb+V_imper+V_ptcp+V_modal+Neg+Adj+Phrasal, data = df1, family = "binomial") 
summary(fit)

#logit regression with categorial variable
df2 <- read.csv('/Users/Kartozianstvo/Desktop/вышечка/cxg_2017/data_viz.csv', sep=',')
fit <- glm(Target ~ AdvTime+AdvLoc+VGram+Neg+Adj+Phrasal, data = df2, family = "binomial") 
summary(fit)

df3 <- read.csv('/Users/Kartozianstvo/Desktop/вышечка/cxg_2017/data_sign.csv', sep=',')
fit <- glm(Target ~ AdvLoc+Adj+Phrasal, data = df3, family = "binomial") 
summary(fit)

#vizualization
library(ggplot2)
library(magrittr)

viz <- read.csv('/Users/Kartozianstvo/Desktop/вышечка/cxg_2017/data_viz.csv', sep=',')
viz %>% 
  ggplot(as.data.frame(table(df2$VGram), aes(x=tense, y=count), fill = Target)) + geom_bar(stat = "identity", position = "dodge")

