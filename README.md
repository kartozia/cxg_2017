# Финальный проект по НИСу "Грамматика конструкций"
### Картозия Инга, БКЛ141


### Материалы
* [Данные](https://docs.google.com/spreadsheets/d/1ZZ1vR18t06Hndg9rw2yg-Zq1cj4ARs92kGAvCdYkFgU/edit?usp=sharing)  

* [Код](./cxg_final.R)

## Рабочая гипотеза
С утра vs. утром
В русском языке есть две конструкции, которые на первый взгляд кажутся взаимозаменяймыми. В этом небольшом исследовании я решила проверить действительно ли это так. 

На мой взгляд, на выбор конструкции могли влиять следующие параметры:

1. Наличие прилагательного

*солнечным снежным утром*

2. Наличие наречия и его тип

*завтра утром vs. завтра с утра*

3. Наличие отрицания в предложение

*не везёт с утра*

4. Форма глагола. Кажется, что обе конструкции хорошо сочетаются с глаголами в форме прошедшего времени

*Утром он вышел из дома*


## Данные

### Материал исследования
Материалы для каждой конструкции были скачены из НКРЯ. На каждую конструкции приводится около 1000 с лишним примеров из корпуса.

### Факторы выбора конструкции
Зависимая переменая: утром/с утра (колонка Target)
Предположительно зависит:

* наличие/отсутсвие наречия времени
* наличие/отсутсвие наречия места
* наличие/отсутсвие прилагательного
* наличие/отсутсвие отрицания в предложение
* форма глагола
* входит ли конструкция в состав фразеологизма

Примеры были выгружены из НКРЯ и размечались вручную.Изначально предполагалось учитывать позицию конструкции в предложении, но составить однозначную классификацию для разметки этого параметра довольно трудно. 

## Анализ: дескриптивная статистика
Все графики можно найти в папке [vizualization](./vizualization/)
Голубым на графике отмечен — *утром*
Красным на графике отмечен — *с утра*

**Прилагательные:** около половины всех случаев с *с утра* имеют при себе прилагательное. Большинство из этих конструкций это *с раннего утра*. Хотя, на первый взгляд казалось, что *утром* будет чаще встречаться с прилагательными

**Наречия места:** Большинство примеров не имеют при себе наречий места и *с утра* имеет всего лишь в 1,5 раза больше примеров с наречие, нежели *утром* (95 и 65, соответственно)

**Наречия времени:** *С утра* в половине случаев встречается с наречием времени, тогда как только 1/4 примеров с *утром* имеют при себе эти наречия. 

**Отрицание:** Обе конструкции почти одинаково редко встречаются в предложениях с отрицанием (50 — *с утра*, 55 — *утром*)

**Глагол:** Самым популярным временем глагола оказалось прошедшее время. Будущее время чаще встречается с конструкцией *с утра* (*С утра будут пить опять.*), а настоящее — с *утром* (*Утром работает, днём спит.*)

**Фразеологизмы:** Здесь учитывались различные устройчивые выражения. Для *утром* это безусловно фраза *С Добрым утром!*. Для *с утра* это *с утра до вечера* и её различные варианты (*с утра до ночи, с раннего утра до позднего вечера и т.п.*).

## Мультифакторный анализ 
Для построения модели использовалась логистическая регрессия. 
В [код](./cxg_final.R) можно найти несколько вариантом:

* с бинарными признаками
* с категориальными признаками

После запуска последней статистически незначимые факторы были убраны из анализа. Финальный вывод можно найти ниже.

**Model output:**

```
Call:
glm(formula = Target ~ AdvLoc + Adj + Phrasal, family = "binomial", 
    data = df3)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.6067  -0.5341  -0.0804   0.8021   3.3874  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  0.96908    0.06417  15.101   <2e-16 ***
AdvLoc      -0.45362    0.20127  -2.254   0.0242 *  
Adj         -2.84451    0.14836 -19.172   <2e-16 ***
Phrasal     -3.85871    0.39235  -9.835   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 2785  on 2008  degrees of freedom
Residual deviance: 1968  on 2005  degrees of freedom
AIC: 1976

Number of Fisher Scoring iterations: 6
```


## Результаты

Наиболее значимыми перемеными в итоге оказались прилагательные, фразеологичность и наличие наречия места. При наличие наречия места и/или прилагательного скорее встретится *с утра*. Возможно, конструкция *с утра* имеет более широкий спект употребления.

## Обсуждение использованных квантитативных методов

### Goodness of fit

```
Likelihood ratio test

Model 1: Target ~ AdvLoc + Adj + Phrasal
Model 2: Target ~ AdvTime + AdvLoc + VGram + Neg + Adj + Phrasal
  #Df  LogLik Df  Chisq Pr(>Chisq)    
1   4 -983.98                         
2  15 -957.82 11 52.322  2.384e-07 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
```

