---
title: "PortfolioProjects.R markdown. From MA 425 LINEAR REGRESSION"
author: ' Hanming Fan '
data: https://raw.githubusercontent.com/sylvadon5/data-files/main/life_expectancy_un.csv
---

__Data__

* Health care decisions are made at the individual, corporate and government levels. Virtually every person, corporation and government have their own perspective on health care; these different perspectives result in a wide variety of systems for managing health care. Comparing different health care systems help us learn about
approaches other than our own, which in turn help us make better decisions in designing
improved systems.

* __Read data into R.__  Select the following variables: "LIFEEXP", "ILLITERATE", "POP", "FERTILITY", "PRIVATEHEALTH", "PUBLICEDUCATION", "HEALTHEXPEND", "BIRTHATTEND", and "GDP". Omit all the NAs before proceeding to perform analysis. 

```{r echo=TRUE, message=FALSE, warning=FALSE}

life_data <- read.csv("https://raw.githubusercontent.com/sylvadon5/data-files/main/life_expectancy_un.csv", header = TRUE, stringsAsFactors = TRUE)

library(tidyverse)

life_0 <- life_data %>% select(LIFEEXP, ILLITERATE, POP, FERTILITY, PRIVATEHEALTH, PUBLICEDUCATION, HEALTHEXPEND, BIRTHATTEND, GDP)

life <- dplyr::filter(life_0) %>% 
  na.omit(life_0)

summary(life)

```


Fit a regression model on **LIFEEXP** using the following explanatory variables: **ILLITERATE, POP, FERTILITY, PUBLICEDUCATION, GDP**, and **lnHEALTH**.

```{r echo=TRUE, message=FALSE, warning=FALSE}

lnHEALTH <- log(life$PRIVATEHEALTH)

first_model <- lm(LIFEEXP ~ ILLITERATE + POP + FERTILITY + PUBLICEDUCATION + GDP + lnHEALTH, data = life)

summary(first_model)

```

  a. Interpret the regression coefficient associated with **PUBLICEDUCATION**.
  
- _The average number of PUBLIC EDUCATION as a percentage is expected to decrease by 0.4237181 per unit increase in the average number of LIFE EXPECTANCY._
      
  b.  Based on the model fit, is **PUBLICEDUCATION** a statistically significant variable? To respond to this question, use a formal test of hypothesis. Assume $\alpha = 0.05$. 
  
$$ H_0: \beta_4 = 0, $$
$$ H_a: \beta_4 \neq 0 $$

- The p-value for $\beta_4$ is 0.179

- Fail to reject $H_0$ since the p-value > $\alpha = 0.05$

- Since we fail to reject $H_0$, we have enough evidence to support the null hypothesis \newline
We therefore conclude that, PUBLIC EDUCATION is not a significant predictor of LIFE EXPECTANCY.

  c. Is there a relation between the response variable and the explanatory variables? Perform a formal test to check the overall significance of the estimated model. Assume $\alpha = 0.05$. 

$$ H_0: \beta_1 = \beta_2 = \beta_3 = \beta_4 = \beta_5 = \beta_6 = 0, $$

$$ H_a: at\ least\ one\ of\ the\ \beta_i \neq 0 $$

The p-value: 6.762847e-05

- Reject $H_0$ since the p-value < $\alpha = 0.05$ in favor of $H_a$
- Since $H_0$ is rejected, we have enough evidence to conclude that at least one of the $\beta_i$ is non-zero. \newline 
Hence there is a relationship between the response and at least one of the explanatory variables.
  
```{r echo=TRUE, message=FALSE, warning=FALSE}

anova(first_model)

pf(6.7, 6, 38, lower.tail = FALSE)

```
  d. To obtain a more parsimonious model, perform a formal test to check the following hypotheses (Assume $\alpha = 0.05)$:
  $$H_0: \beta_1=\beta_2=\beta_4=\beta_6=0,$$
  $$H_a: H_0\text{ is not true}.$$
  The p-value: 5.299e-13

- Reject $H_0$ since the p-value < $\alpha = 0.05$ 
- Since $H_0$ is rejected, we have enough evidence to conclude that at least one of the $\beta_i$ is non-zero. \newline 
Hence there is a relationship between the response and all the explanatory variables.
  
```{r echo=TRUE, message=FALSE, warning=FALSE}

first_model <- lm(LIFEEXP ~ ILLITERATE + POP + FERTILITY + PUBLICEDUCATION + GDP + lnHEALTH, data = life)

second_model <- lm(LIFEEXP ~ ILLITERATE + POP + PUBLICEDUCATION + lnHEALTH, data = life)

summary(second_model)

anova(first_model, second_model)

```

  e. Based on your results from part d, predict LIFEEXP for a new point with the following values: ILLITERATE = 17, POP = 1000, FERTILITY  = 4, PUBLICEDUCATION = 7, GDP = 1000, lnHEALTH = 10. Construct and interpret a 99% prediction interval.
   
- _The LIFEEXP for the new point with the values is 45.51 and a prediction interval for LIFEEXP at 99% prediction interval is between 20.31 and 70.71_

```{r}

new <- data.frame(ILLITERATE = 17, POP = 1000, FERTILITY  = 4, PUBLICEDUCATION = 7, GDP = 1000, lnHEALTH = 10)

new_point <- predict(first_model, new, interval = "prediction", levels = 0.99)

round(new_point, digits = 2)

new_point

```

 