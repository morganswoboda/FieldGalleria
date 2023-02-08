Fall 2022 Galleria field infection data
================
Morgan Swoboda
2023-02-08

# Package install and upload data sheet

``` r
#install.packages("emmeans")
#install.packages("ggthemes")

library(ggplot2)
library(emmeans)
library(tidyr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggthemes)
library(RColorBrewer)

#import data
Fall22EPF <- read.csv("Fall 22 epf bioassay - Sheet1.csv")

#make treatments and varieties into factors
Fall22EPF$treatment = factor(Fall22EPF$treatment, levels = c("Control", "10Met", "10Bb"))
Fall22EPF$variety = factor(Fall22EPF$variety, levels = c("Cochise", "Armani"))
```

\#Prelim data check

``` r
ggplot(data = Fall22EPF, aes(x = treatment, y = perc.inf.total, color = variety)) + geom_boxplot() + ggtitle("Total infection")
```

![](Fall2022GalleriaField_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
#everything looks ok

ggplot(data = Fall22EPF, aes(x = treatment, y = perc.met.total, color = variety)) + geom_boxplot() + ggtitle("Metarhizium infection")
```

![](Fall2022GalleriaField_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->
\# Stats for Metarhizium - normal ANOVA way?

``` r
attach(Fall22EPF)

##two way ANOVA
two.way.Met <- aov(perc.met.total~treatment*variety)
summary(two.way.Met) #no signifcant differences, no interaction, change to one way model instead
```

    ##                   Df Sum Sq Mean Sq F value Pr(>F)
    ## treatment          2     40    19.8   0.055  0.947
    ## variety            1    196   196.2   0.543  0.467
    ## treatment:variety  2    150    74.8   0.207  0.814
    ## Residuals         30  10831   361.0

``` r
summary.lm(two.way.Met)
```

    ## 
    ## Call:
    ## aov(formula = perc.met.total ~ treatment * variety)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -24.073 -14.260  -7.458  13.864  41.300 
    ## 
    ## Coefficients:
    ##                              Estimate Std. Error t value Pr(>|t|)  
    ## (Intercept)                   20.1583     7.7571   2.599   0.0144 *
    ## treatment10Met                -5.8983    10.9701  -0.538   0.5948  
    ## treatment10Bb                 -4.7883    10.9701  -0.436   0.6656  
    ## varietyArmani                 -0.1583    10.9701  -0.014   0.9886  
    ## treatment10Met:varietyArmani   9.9717    15.5141   0.643   0.5253  
    ## treatment10Bb:varietyArmani    4.5100    15.5141   0.291   0.7733  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 19 on 30 degrees of freedom
    ## Multiple R-squared:  0.03435,    Adjusted R-squared:  -0.1266 
    ## F-statistic: 0.2134 on 5 and 30 DF,  p-value: 0.9541

``` r
##one way anova
one.way.Met <- aov(perc.met.total~treatment)
summary(one.way.Met) #not significant
```

    ##             Df Sum Sq Mean Sq F value Pr(>F)
    ## treatment    2     40    19.8   0.058  0.943
    ## Residuals   33  11177   338.7

``` r
summary.lm(one.way.Met)
```

    ## 
    ## Call:
    ## aov(formula = perc.met.total ~ treatment)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -20.079 -17.546  -7.546  13.287  36.393 
    ## 
    ## Coefficients:
    ##                Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)     20.0792     5.3126   3.780 0.000626 ***
    ## treatment10Met  -0.9125     7.5132  -0.121 0.904069    
    ## treatment10Bb   -2.5333     7.5132  -0.337 0.738113    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 18.4 on 33 degrees of freedom
    ## Multiple R-squared:  0.003523,   Adjusted R-squared:  -0.05687 
    ## F-statistic: 0.05833 on 2 and 33 DF,  p-value: 0.9434

``` r
plot(one.way.Met) #probably not normal based on QQ plot, looks like it's exponential?
```

![](Fall2022GalleriaField_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->![](Fall2022GalleriaField_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->![](Fall2022GalleriaField_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->![](Fall2022GalleriaField_files/figure-gfm/unnamed-chunk-3-4.png)<!-- -->

``` r
shapiro.test(resid(one.way.Met)) #residuals NOT normal
```

    ## 
    ##  Shapiro-Wilk normality test
    ## 
    ## data:  resid(one.way.Met)
    ## W = 0.88313, p-value = 0.001218

``` r
fligner.test(perc.met.total~treatment) #normal
```

    ## 
    ##  Fligner-Killeen test of homogeneity of variances
    ## 
    ## data:  perc.met.total by treatment
    ## Fligner-Killeen:med chi-squared = 0.57668, df = 2, p-value = 0.7495

``` r
# KW instead since it's not normal
##KW for met
MetF22KW <- kruskal.test(perc.met.total~treatment)
print(MetF22KW) #not significant
```

    ## 
    ##  Kruskal-Wallis rank sum test
    ## 
    ## data:  perc.met.total by treatment
    ## Kruskal-Wallis chi-squared = 0.35641, df = 2, p-value = 0.8368

``` r
detach(Fall22EPF)
```

# Try a different type of analysis? Could this data be proportions?

## X is categorical, unsure what to do with Y?

``` r
attach(Fall22EPF)

head(Fall22EPF)
```

    ##   pot.ID treatment variety num.dead num.pupae perc.pupated total.out num.infect
    ## 1    C+1   Control Cochise        5         2     28.57143         7          1
    ## 2    C+2   Control Cochise       10         0      0.00000        10          6
    ## 3    C+3   Control Cochise        9         0      0.00000         9          6
    ## 4    C+4   Control Cochise        9         0      0.00000         9          2
    ## 5    C+5   Control Cochise        9         0      0.00000         9          2
    ## 6    C+6   Control Cochise        9         1     10.00000        10          2
    ##   num.bb.infect num.met.infect perc.inf.total perc.bb.total perc.met.total
    ## 1             0              1          14.29             0          14.29
    ## 2             0              2          60.00             0          20.00
    ## 3             0              4          66.67             0          44.44
    ## 4             0              1          22.22             0          11.11
    ## 5             0              1          22.22             0          11.11
    ## 6             0              2          20.00             0          20.00

``` r
par(mfrow = c(1,2))

inf.proportion <- num.met.infect/total.out #looking for the proportion of metarhizium infections out of the number of removed insects
plot(treatment, inf.proportion, ylab = "proportion met infected", xlab("Treatment"))
plot(treatment, log(inf.proportion), ylab = "proportion met infected", xlab("Treatment"))
```

    ## Warning in bplt(at[i], wid = width[i], stats = z$stats[, i], out =
    ## z$out[z$group == : Outlier (-Inf) in boxplot 1 is not drawn

![](Fall2022GalleriaField_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
