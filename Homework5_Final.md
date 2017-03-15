# N741: Homework 5
Craig Alder  
March 15, 2017  

## Load dataset in from `car` package


```r
library(car)
data(Wong)

library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following object is masked from 'package:car':
## 
##     recode
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
# add an age group variable
Wong$agegrp <- case_when(
  (Wong$age > 0 & Wong$age <= 10) ~ 1,
  (Wong$age > 10 & Wong$age <= 20) ~ 2,
  (Wong$age > 20 & Wong$age <= 30) ~ 3,
  (Wong$age > 30 & Wong$age <= 40) ~ 4,
  (Wong$age > 40 & Wong$age <= 50) ~ 5,
  (Wong$age > 50 & Wong$age <= 60) ~ 6,
  (Wong$age > 60 & Wong$age <= 70) ~ 7,
  (Wong$age > 70 & Wong$age <= 100) ~ 8)

# convert to factor, add code levels and labels
Wong$agegrp <- factor(Wong$agegrp,
                  levels = c(1,2,3,4,5,6,7,8),
                  labels = c("Ages 1-10",
                             "Ages 11-10",
                             "Ages 21-10",
                             "Ages 31-10",
                             "Ages 41-10",
                             "Ages 51-10",
                             "Ages 61-70",
                             "Ages 71-100"))
```

1. Make a table of non-parametric statistics (median and IQR) for the number of days and duration grouped by `sex`. You'll be using `summarise()` from the `dplyr` package. For a given variable `x` you'll use `median(x, na.rm=TRUE)`, `quantile(x, 0.25, na.rm=TRUE)`, and `quantile(x, 0.75, na.rm=TRUE)`. Give the table a title using the `caption=` option and update the column names with something nice using the `col.names=` option in the `knitr::kable()` command. 


```r
tbl1 <- Wong %>%
group_by(sex) %>%
  summarise(Q1days = quantile(days, 0.25, na.rm=TRUE),
            mediandays = median(days, na.rm=TRUE),
            Q3days = quantile(days, 0.75, na.rm=TRUE),
            Q1duration = quantile(duration, 0.25, na.rm=TRUE),
            medianduration = median(duration, na.rm=TRUE),
            Q3duration = quantile(duration, 0.75, na.rm=TRUE))
  
knitr::kable(tbl1,
             col.names=c("Sex",
                         ".25Q Days","Median Days",".75Q Days",
                         ".25Q Duration","Median Duration",
                         ".75Q Duration"),
             caption="Days and Duration by Sex")
```



Table: Days and Duration by Sex

Sex       .25Q Days   Median Days   .75Q Days   .25Q Duration   Median Duration   .75Q Duration
-------  ----------  ------------  ----------  --------------  ----------------  --------------
Female        58.50           135      361.00               1                 4              11
Male          59.75           163      431.25               1                 7              18

2.  Make a table of parametric statistics (mean and SD) for the performance outcomes `piq` and `viq` grouped by `sex`. Like the table above, you'll be using `summarise()` from the `dplyr` package. Now you'll use `mean(x, na.rm=TRUE)` and `sd(x, na.rm=TRUE)`. Give the table a title using the `caption=` option and update the column names with something nice using the `col.names=` option in the `knitr::kable()` command. 


```r
tbl2 <- Wong %>%
group_by(sex) %>%
  summarise(piqmean = mean(piq, na.rm=TRUE),
            piqsd   = sd(piq, na.rm=TRUE),
            viqmean   = mean(viq,na.rm=TRUE),
            viqsd  = sd(viq, na.rm=TRUE))
  
knitr::kable(tbl2,
             col.names=c("Sex",
                         "Math IQ, Mean","Math IQ SD",
                         "Verbal IQ, Mean","Verbal IQ,SD G2"),
             caption="IQ Scores by Sex")
```



Table: IQ Scores by Sex

Sex       Math IQ, Mean   Math IQ SD   Verbal IQ, Mean   Verbal IQ,SD G2
-------  --------------  -----------  ----------------  ----------------
Female         89.18310     17.99866          94.35211          14.24690
Male           87.11154     14.25658          95.13077          14.02281

3. Make a table containing the frequencies and relative percentages for `agegrp`. Use the example we did in class to help guide you.


```r
# find the sample size or number of rows in dataset
ss <- length(Wong$agegrp)

# group by each level of Medu
# find the raw counts using n()
# then compute the relative % by dividing by ss
tbl3 <- Wong %>%
  group_by(agegrp) %>%
  summarise(freq = n(),
            pct = n()*100/ss)

# make a summary table
knitr::kable(tbl3,
             col.names = c("Age Group",
                           "Frequency",
                           "Percent"),
             caption="Frequency Table for Age Group")
```



Table: Frequency Table for Age Group

Age Group      Frequency      Percent
------------  ----------  -----------
Ages 1-10              1    0.3021148
Ages 11-10            53   16.0120846
Ages 21-10           144   43.5045317
Ages 31-10            48   14.5015106
Ages 41-10            42   12.6888218
Ages 51-10            27    8.1570997
Ages 61-70            14    4.2296073
Ages 71-100            2    0.6042296

4. Make a regression model (Model 1) for the performance IQ (`piq`) using `age` and `sex`. Put the regression model results into a table.


```r
m1 <- lm(piq~age+sex, data=Wong)
sm1 <- summary(m1)
knitr::kable(sm1$coefficients,
             caption="Model 1: Math IQ regressed on age and sex")
```



Table: Model 1: Math IQ regressed on age and sex

                 Estimate   Std. Error     t value    Pr(>|t|)
------------  -----------  -----------  ----------  ----------
(Intercept)    86.8868114    2.5796740   33.681314   0.0000000
age             0.0743977    0.0600527    1.238874   0.2162781
sexMale        -2.1651531    2.0258169   -1.068780   0.2859546

5. Make a second regression model (Model 2) for performance IQ (`piq`) using `age` and `sex` plus `days` and `duration`. Put the regression model results into a table.


```r
m2 <- lm(piq~age+sex+days+duration, data=Wong)
sm2 <- summary(m1)
knitr::kable(sm2$coefficients,
             caption="Model 1: Math IQ regressed on age, sex, days, duration")
```



Table: Model 1: Math IQ regressed on age, sex, days, duration

                 Estimate   Std. Error     t value    Pr(>|t|)
------------  -----------  -----------  ----------  ----------
(Intercept)    86.8868114    2.5796740   33.681314   0.0000000
age             0.0743977    0.0600527    1.238874   0.2162781
sexMale        -2.1651531    2.0258169   -1.068780   0.2859546

6. Finally, make a table showing the results from the `anova()` command comparing Model 1 and Model 2 you made above using the example we did in class as a guide. 


```r
anova(m1,m2)
```

```
## Analysis of Variance Table
## 
## Model 1: piq ~ age + sex
## Model 2: piq ~ age + sex + days + duration
##   Res.Df   RSS Df Sum of Sq      F  Pr(>F)   
## 1    328 74968                               
## 2    326 72586  2    2381.9 5.3489 0.00518 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

7. STUDENT CHOICE - pick either a `htmlwidget` from [http://gallery.htmlwidgets.org/](http://gallery.htmlwidgets.org/) or do a "flexdashboard" using the templates at [http://rmarkdown.rstudio.com/flexdashboard/](http://rmarkdown.rstudio.com/flexdashboard/) as a guide.



Code I tried

{r setup, include=FALSE}
library(ggplot2)
library(plotly)
# Build the plot
mathIQ_plot <- ggplot(data = Wong, aes(x="Verbal IQ", y="Math IQ")) + 
  geom_point(aes(color=sex), size=2)
mathIQ_plot
ggplotly(mathIQ_plot)


### References

Wong, P. P., Monette, G., and Weiner, N. I. (2001) Mathematical models of cognitive recovery. Brain Injury, 15, 519–530.

Fox, J. (2016) Applied Regression Analysis and Generalized Linear Models, Third Edition. Sage.


The link to my Github account is [https://github.com/craigalder](https://github.com/craigalder). The link to my repository for this assignment is [https://github.com/craigalder/N741gapminder1.git](https://github.com/craigalder/Regression-1).
