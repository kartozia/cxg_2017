# Финальный проект по НИСу "Грамматика конструкций"
### Картозия Инга, БКЛ141


### Материалы
[Данные](https://docs.google.com/spreadsheets/d/1ZZ1vR18t06Hndg9rw2yg-Zq1cj4ARs92kGAvCdYkFgU/edit?usp=sharing)  

[Ссылка на код, здесь целиком с комментарием](https://github.com/...) (TBA)

## Рабочая гипотеза
С утра vs. утром
В языке ... конструкция "..." чаще встречается при таких-то условиях, чем альтернативная ей конструкция "...". На выбор конструкции могут также оказывать влияние такие-то социолингвистические / лингвистические факторы.

## Данные

### Материал исследования
Материалы для каждой конструкции были скачены из НКРЯ. На каждую конструкции приводится около 1000 с лишним примеров из корпуса.

### Факторы выбора конструкции
Какая переменная является зависимой в дальнейшем анализе, от каких других переменных она предположительно зависит. Рекомендуется также обсудить независимость факторов, их корреляцию и т.д. Кроме того, можно обсудить, разметка каких параметров вызывала трудности.

## Анализ: дескриптивная статистика
В этом разделе располагаются таблицы, графики и обсуждение дистрибуции значений в отдельных переменных и взаимной дистрибуции пар переменных.
Рекомендуются гистограммы, density plots, боксплоты (или violin plots), метрики корреляции и статистической значимости. Если вы используете метрику хи-квадрат, не забывайте посчитать к ней effect size.

## Мультифакторный анализ 
* логистическая регрессия (привести примеры нескольких моделей, закончив оптимальной, в которой остаются только значимые факторы)  
**Out model**
```glm(formula = Target ~ AdvLoc + Adj + Phrasal, family = "binomial", 
    data = df3)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-1.6085  -0.5960   0.8007   0.8007   3.3211  

Coefficients:
            Estimate Std. Error z value Pr(>|z|)    
(Intercept)  0.97305    0.06416  15.166   <2e-16 ***
AdvLoc      -0.49636    0.19798  -2.507   0.0122 *  
Adj         -2.61096    0.13860 -18.837   <2e-16 ***
Phrasal     -3.87278    0.39208  -9.878   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 2784.9  on 2008  degrees of freedom
Residual deviance: 2017.0  on 2005  degrees of freedom
AIC: 2025

Number of Fisher Scoring iterations: 6
```

Требуется показать output модели и привести графики

## Содержательный лингвистический анализ результатов статистического анализа
Без содержательного анализа факторов выбора конструкции (взаимодействия выделенных вами переменных, их значимости/важности) проект не будет считаться выполненным.   

## Обсуждение использованных квантитативных методов
Тут или ранее по ходу анализа нужно обсудить аккуратность (точность) классифицирующих моделей, classification power, adjusted R2, goodness of fit и т.п. -- стандартные показатели качества моделирования, а также важность / значимость факторов. 
В заключение вы можете предложить идеи по дальнейшей работе с данными и их статистическим анализом. 
