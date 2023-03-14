International Exposure and English Ability
================
Raj Mehta - rajm

-----

### Introduction

The data set given is a subset of the which English data, which records
multiple variables about a participant in an English test.

The questions I seek to answer regard international exposure and English
language ability. By international exposure I specifically want to look
at factors such as total known languages and places lived by a person,
and their effect on English ability. Ability in English language will
always be measured by the percentage of correct answers on the test.

To answer the first part of my question, I will be looking at if people
who have a spent a portion of their lives outside an English speaking
country are better at English than those who have spent their entire
life in an English speaking country. The people that I will be looking
at are those that have English as their native language. The variable
that I will use to help me with this analysis is
‘english\_country\_percent.’

Answering a similar question, I will take my analysis one step further
to see if there is a correlation between the number of countries someone
has lived (do not necessarily have to be English speaking countries) and
their English ability. For that I will edit my tibble so that I can get
a numerical value for the variable ‘all\_countries.’

To answer the second part of my research question, I will be performing
statistical tests to see whether people that speak only English have
better English ability than those that are multilingual (with English
being one of their known languages).

-----

### Exploratory data analysis

``` r
#new tibble created with number of primary languages and number of countries lived.

langNum <- lang_data %>%
  filter(native_english) %>%
  mutate(langs = str_count(primary_languages, ",") + 1) %>%
  mutate(countries = str_count(all_countries, ",") + 1) %>%
  mutate(one_plus = langs > 1) %>%
  mutate(id = row_number()) 


#tibble to answer question one, tells us if someone has lived only in English speaking countries or not

other_country <- lang_data %>%
  filter(native_english) %>%
  mutate(stayed = english_country_percent >= 1) %>%
  mutate(id = row_number()) 



other_country %>% 
  group_by(stayed)%>%
  summarise(mean_score = mean(correct))
```

    ## # A tibble: 2 x 2
    ##   stayed mean_score
    ## * <lgl>       <dbl>
    ## 1 FALSE       0.891
    ## 2 TRUE        0.959

``` r
#ot is the same tibble as other_country just helped with graphing. 

ot <- other_country %>%
  group_by(stayed)%>%
  summarise(mean_score = mean(correct))

#This column graph compares people that have moved (lived in a non-english speaking country) versus those that have stayed in only English speaking countries
ggplot(ot, aes(x = stayed, y = mean_score)) +
  geom_col() 
```

![](project_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

``` r
#Mean scores3 compares the number of known languages with the mean score.
meanScores3 <- langNum %>%
  group_by(countries) %>%
  summarize(mean_score = mean(correct))
 
ggplot(meanScores3, aes(x = countries, y = mean_score)) +
  geom_point() 
```

![](project_files/figure-gfm/unnamed-chunk-1-2.png)<!-- -->

``` r
#meanScores is mean scores based on how many languages someone knows.
meanScores <- langNum %>%
  group_by(langs) %>%
  summarize(mean_score = mean(correct))

meanScoresL <- langNum %>%
  group_by(langs) %>%
  summarize(mean_score = mean(correct)) %>%
  pull(mean_score)


meanScores3L <- langNum %>%
  group_by(countries) %>%
  summarize(mean_score = mean(correct)) %>%
  pull(mean_score)

combine <- tibble(n = 1:10, y = meanScoresL, z = meanScores3L[1:10])




ggplot(meanScores, aes(x = langs, y = mean_score)) +
  geom_point() 
```

![](project_files/figure-gfm/unnamed-chunk-1-3.png)<!-- -->

``` r
ggplot(langNum, aes(x = countries, y = )) +
  geom_bar() 
```

![](project_files/figure-gfm/unnamed-chunk-1-4.png)<!-- -->

``` r
ggplot(langNum, aes(x = langs, y = )) +
  geom_bar() 
```

![](project_files/figure-gfm/unnamed-chunk-1-5.png)<!-- -->

Description of the graphs.

1)  The first column graph shows the mean score of those that have lived
    in a non-English speaking country (stayed = FALSE) versus those that
    have stayed in an English speaking country their whole life (stayed
    = TRUE). The graph shows that people who have lived in an English
    speaking country their whole life tend to perform better on the
    test.

2)  The second graph is a scatter plot showing the mean score of a
    person versus how many different countries they live in. Despite the
    results of the first graph, there seems to be a weak positive
    correlation between the two variables.

3)  The last graph is a scatter plot showing the number of languages a
    person knows and their English ability. I have grouped by known
    languages and used the mean, so this is not an entirely accurate
    description between the two variables, as there are many more people
    that know only one language as opposed to 5 languages. There does
    not seem to be a strong correlation between the variables, but we
    can find out if there is one through our inference tests and when
    building our model.

4, 5) Graphs 4 and 5 are histograms showing the distribution of number
of countries lived and the distribution of number of languages known. As
we can see, when we perform our inferences it is best to use samples
because the data is very right skewed.

Moving on, we will check if our results are significant by performing
various inference tests.

-----

### Inference

The first inference test I perform is a t-test. I perform this t-test to
see if there is a significant difference in English ability between
English speakers that have lived only in English speaking countries
versus those that have moved to a non-English speaking country. I do
this since the bar graph suggests that there is a difference. Sampling
is used because the histogram above shows that there are many more
people who lived in only one country than those that have moved around.

``` r
# count the number of people that have moved internationally.
# count number of people lived only in English speaking country

n_inter <- other_country %>%
  filter(!stayed) %>%
  pull(id)


n_stayed <- other_country %>%
  filter(stayed) %>%
  pull(id)

#take a sample here
inter.id <- sample(n_inter, 1000)

inter.sample <- other_country %>%
  filter(id %in% inter.id)%>%
  pull(correct)

stayed.id <- sample(n_stayed, 1000)

stayed.sample <- other_country %>%
  filter(id %in% stayed.id) %>%
  pull(correct)

t.test(inter.sample, stayed.sample)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  inter.sample and stayed.sample
    ## t = -25.412, df = 1546.2, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  -0.07413312 -0.06350898
    ## sample estimates:
    ## mean of x mean of y 
    ## 0.8895684 0.9583895

The results of this t-test show that there is a significant difference
between those who have lived in only English speaking places and those
that have moved around. Moreover, it tells us that people that have
lived their whole life in an English speaking country have a better
score than those that have moved out of an English speaking country.

Next I perform an anova test to see if there is a relationship between
the number of countries someone has lived in and scores.

``` r
oneway.test(formula = correct ~ countries, data = langNum)
```

    ## 
    ##  One-way analysis of means (not assuming equal variances)
    ## 
    ## data:  correct and countries
    ## F = 92.937, num df = 10.000, denom df = 15.966, p-value = 3.283e-12

As the p-value is very close to 0, I can conclude that the number of
countries someone has lived in has an effect on average score. I will
proceed to see what that relationship is in my modeling section.

The last inference test I perform is a t-test to see if there is a
difference between people that speak only one language versus those that
are multilingual. Again both groups of people are native English
speakers and samples are taken because there are far more monolingual
people than multilingual ones.

``` r
#repeat t - test for one language or more than one language
one_plus <- langNum %>%
  filter(one_plus) %>%
  nrow()

one_only <- langNum %>%
  filter(!one_plus) %>%
  nrow()

one_plus.id <- sample(one_plus, 1000)

one_plus.sample <- langNum %>%
  filter(id %in% one_plus.id)%>%
  pull(correct)

one_only.id <- sample(one_only, 1000)

one_only.sample <- langNum %>%
  filter(id %in% one_only.id) %>%
  pull(correct)

t.test(one_plus.sample, one_only.sample)
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  one_plus.sample and one_only.sample
    ## t = 6.8103, df = 1934.2, p-value = 1.295e-11
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  0.01552969 0.02809136
    ## sample estimates:
    ## mean of x mean of y 
    ## 0.9266421 0.9048316

The second t-test shows us that there is a significant difference and
that people who speak more than one language (with English being one of
them) have a higher average score than those that speak only English.

-----

### Modeling

In my inferences I was able to conclude that the number of countries
someone has lived in has an effect on the average score. I also found
out that people who know multiple languages have higher scores than
those who speak only English. For that reason I will create a linear
model with score as a function of number of countries and languages
known. As the data is heavily skewed, I will use samples as opposed to
the data points from the entire data set.

``` r
samp <- langNum %>%
  pull(id)

#take a sample here
samp.id <- sample(samp, 1000)

samp.sample <- langNum %>%
  filter(id %in% inter.id)
  

m1 <- lm(correct ~ countries + langs, data = samp.sample)

summary(m1)
```

    ## 
    ## Call:
    ## lm(formula = correct ~ countries + langs, data = samp.sample)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.296826 -0.044195  0.008752  0.050542  0.124226 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 0.833984   0.006500 128.306  < 2e-16 ***
    ## countries   0.019464   0.002458   7.919 6.35e-15 ***
    ## langs       0.022326   0.004540   4.918 1.02e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.07169 on 997 degrees of freedom
    ## Multiple R-squared:  0.09211,    Adjusted R-squared:  0.09028 
    ## F-statistic: 50.57 on 2 and 997 DF,  p-value: < 2.2e-16

``` r
m2 <- lm(correct ~ countries, data = samp.sample)

summary(m2)
```

    ## 
    ## Call:
    ## lm(formula = correct ~ countries, data = samp.sample)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.30063 -0.04800  0.01491  0.05701  0.12043 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 0.858266   0.004276 200.703   <2e-16 ***
    ## countries   0.021309   0.002457   8.672   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.07252 on 998 degrees of freedom
    ## Multiple R-squared:  0.07008,    Adjusted R-squared:  0.06915 
    ## F-statistic: 75.21 on 1 and 998 DF,  p-value: < 2.2e-16

``` r
m3 <- lm(correct ~ langs, data = samp.sample)

summary(m3)
```

    ## 
    ## Call:
    ## lm(formula = correct ~ langs, data = samp.sample)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.30481 -0.04464  0.01098  0.05309  0.11625 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 0.855942   0.006058 141.293  < 2e-16 ***
    ## langs       0.027813   0.004623   6.016 2.51e-09 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.07387 on 998 degrees of freedom
    ## Multiple R-squared:  0.035,  Adjusted R-squared:  0.03403 
    ## F-statistic: 36.19 on 1 and 998 DF,  p-value: 2.505e-09

The model (m1) shows how English ability is affected by the number of
countries a person has lived in and number of languages known. The
p-value is under 0.05 for both variables and the r-squared value is 0.1.
This suggests that their is a significant but weak correlation between
the variables. As the gradient is positive for both variables this
correlation is positive.

I then performed linear analysis on the variables alone (backward
analysis, shown by summaries of m2 and m3). When removing either one of
the variables, the resulting model has a much lower r-squared and
adjusted r-squared value. This suggests that the best model we should
use is the one that has score as a function of both number of languages
known and countries lived.

Based on our model we can use it to make some predictions about our
data.

Our model says that score = 0.0204(countries lived) + 0.02545(languages
known) + 0.8324. Thus if someone knows 3 languages and has lived in 3
countries we can assume that their score should be around 0.97. In
comparison, one of the data points from our dataset that has 3 languages
known and 3 countries lived has a score of 0.93. The difference between
what our model predicts and what the actual data says is 0.97 - 0.93
which is 0.04.

-----

### Conclusion

To conclude, I will attempt to provide explanations for our observed
data and highlight any shortcomings in the data set and in my research.
From the inferences we learned that people who have stayed in English
speaking countries their whole life have higher scores than those that
have not. This makes sense as people who live in an English speaking
country will be exposed to English more often.

I then learned that native English speakers that know more than one
language are better at English than those who know only one language.
This may be because multilingual people have a better understanding of
language in general and are therefore able to better perform on the
test.

I then created a model to see how both number of countries lived in and
number of languages known influences score. We found that there was a
weak positive correlation between both variables. This is quite an
interesting result and it makes sense as people who live in multiple
countries probably speak multiple languages and thus both variables
would correlate similarly with test scores. The accuracy of our model
was quite low as the R-squared value was very low and if we were to make
predictions about a person that knows many languages or has lived in
many places, our model would tell us that the score should be above 1
which is not possible.

The findings I have collected have helped me answer my research
questions and tell us that different types of international exposure
affect English ability in different ways.

A shortcoming in our data is that we had very few people that lived in
many countries or spoke many languages (8 or more). This meant our
dataset was not quite complete and the statistical tests became
inaccurate as there were certain data points where we were using data of
thousands of people and others where we had data of only a handful of
people. For example, there were more than ten thousand people that knew
1 language but less than a 100 people that knew 4 languages. If we had
data from more of these kind of people (outliers) it may help us.
Furthermore there were many variables that we did not control for. For
example, level of education could have been an important factor in
judging English ability, and the samples could have had an uneven
distribution of education levels. Age and Age of starting to learn
English could have also had a similar effect.
