#logit regression only numbers
df1 <- read.csv('/Users/Kartozianstvo/Desktop/вышечка/cxg_2017/data.csv', sep=',')
fit <- glm(Target ~ AdvTime+AdvLoc+V_pres+V_past+V_fut+V_inf+V_predicative+V_subj+V_converb+V_imper+V_ptcp+V_modal+Neg+Adj+Phrasal, data = df1, family = "binomial") 
summary(fit)

#logit regression with categorial variable
df2 <- read.csv('/Users/Kartozianstvo/Desktop/вышечка/cxg_2017/data_viz.csv', sep=',')
fit1 <- glm(Target ~ AdvTime+AdvLoc+VGram+Neg+Adj+Phrasal, data = df2, family = "binomial") 
summary(fit1)

#final
df3 <- read.csv('/Users/Kartozianstvo/Desktop/вышечка/cxg_2017/data_sign.csv', sep=',')
fit2 <- glm(Target ~ AdvLoc+Adj+Phrasal, data = df3, family = "binomial") 
summary(fit2)

1-logLik(fit)/logLik(fit1)
# Predict and vizualize
df3$preds <- plogis( predict(fit , newdata=df3))
model <- glm(Sample ~ Temp + Age, data = dataset, family = binomial)
newdata <- expand.grid(
  AdvLoc = pretty(dataset$AdvLoc, 20), 
  Adj = pretty(dataset$Adj, 5))
newdata$Target <- predict(model, newdata = newdata, type = "response")
library(ggplot2)
ggplot(newdata, aes(x = AdvLoc, y = Target)) + geom_line() + facet_wrap(~Adj)
#evaluate
library(lmtest)
lrtest(fit1,fit2)

#vizualization
library(magrittr)
pl <- ggplot(df3, aes( Adj, preds, color=Target))
pl

#analytics viz
viz <- read.csv('/Users/Kartozianstvo/Desktop/вышечка/cxg_2017/data_viz.csv', sep=',')
table(viz$VGram, viz$Target)
dt <- as.data.frame(table(viz$VGram, viz$Target))
dt
dt %>% 
  ggplot(aes(x=Var1, y=Freq, fill=Var2)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Freq, Freq = Freq + 0.1), position = position_dodge(0.9), vjust = 0)

#correlation
install.packages("corrplot")
library('corrplot')
loaddata <- read.csv('/Users/Kartozianstvo/Desktop/вышечка/cxg_2017/encode.csv', sep=',')
df = subset(loaddata, select = -c(X) )
corrmatr <- cor(df)
res1 <- cor.mtest(df, conf.level = .99)
corrplot(corrmatr, p.mat = res1$p, insig = "blank")