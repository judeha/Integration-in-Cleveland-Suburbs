#Gregory Bruich, Ph.D.
#Economics 50
#Harvard University
#Send suggestions and corrections to gbruich@fas.harvard.edu
rm(list=ls()) # removes all objects from the environment
cat('\014') # clears the console

## load packages
if (!require(haven)) install.packages("haven"); library(haven)
if (!require(sandwich)) install.packages("sandwich"); library(sandwich)
if (!require(lmtest)) install.packages("lmtest"); library(lmtest)
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if (!require(statar)) install.packages("statar"); library(statar)

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Harvard, Year 1/Semester 2/ECON 50/E1")

## Read data 
atlas <- read_dta("atlas.dta")

#2 kfr_pooled_pooled_p25 and kfr_black_female_p25 are measured in percentile ranks.
# Higher values correspond to higher upward mobility, since it means that kids from
# the 25th percentile are moving to higher income percentile ranks. The statistic should
# be estimated using a linear statistical model because it assumes a linear one to one
# relationship between parent income percentile ranks and children's income percentile
# ranks. Using this relationship we can calculate a regression to find mobility at the 25th
# percentile or any other percentile.

#3
ggplot(atlas) + geom_histogram(aes(x=kfr_pooled_pooled_p25, y=..density..), bins = 100)

# can also paste into word doc
ggsave("histogram_kfr_pooled_pooled_p25.png")

# It seems that there is an even, symmetrical distribution. Kids from the 25th percentile
# mostly end up between the 25th and 65th percentiles, with the vast majority falling in
# the middle around the 40th percentile. Most of them end up in a higher percentile rank
# than their parents, but very few make it past the 65th percentile.

#4
summary(atlas$kfr_pooled_pooled_p25)
sd(atlas$kfr_pooled_pooled_p25, na.rm=TRUE)

# Mean: 42.858, Min: -3.286, Max: 103.349, Sd: 7.126422

# kfr_pooled_pooled_25 can be below 0 or above 100 for the data because of the limits of
# a linear statistical model. Linear regression works well for data within its bounds
# (0 to 100) but not as well for extrapolating beyond, since it doesn't recognize that
# while the regression line may extend to negative values or values above 100 that these
# values are "nonsensical" for our purposes.

#5
sub1 = subset(atlas, czname=="Columbus")
sub2 = subset(sub1, state==39)
sub3 = subset(atlas, state==39)
mean(sub2$kfr_pooled_pooled_p25, na.rm=TRUE) # Mean: 40.20921
mean(sub3$kfr_pooled_pooled_p25, na.rm=TRUE) # Mean: 40.41933

# The mean for kfr_pooled_pooled_p25 is lower in my census tract than in the U.S. and
# very slightly lower than in my state. Kids have worse chances of climbing the income
# ladder than the average child in America.

#6
sd(sub2$kfr_pooled_pooled_p25, na.rm=TRUE) # Sd: 7.301378
sd(sub3$kfr_pooled_pooled_p25, na.rm=TRUE) # Sd: 7.039618

# Standard deviation in my county is higher than the U.S. standard deviation by 0.2,
# but the standard deviation in my state is lower by 0.1. This shows us that the data for
# my county is a little more spread out than the data for the whole U.S., and the data
# for my state is a little less spread out.

#7a
subA = subset(atlas, HOLC_A > 0.5 & !is.na(HOLC_A))
subB = subset(atlas, HOLC_B > 0.5 & !is.na(HOLC_B))
subC = subset(atlas, HOLC_C > 0.5 & !is.na(HOLC_C))
subD = subset(atlas, HOLC_D > 0.5 & !is.na(HOLC_D))
mean(subA$kfr_pooled_pooled_p25, na.rm=TRUE) # Mean: 44.01404
mean(subB$kfr_pooled_pooled_p25, na.rm=TRUE) # Mean: 42.46768
mean(subC$kfr_pooled_pooled_p25, na.rm=TRUE) # Mean: 39.87268
mean(subD$kfr_pooled_pooled_p25, na.rm=TRUE) # Mean: 36.15839

# It seems that there is a correlation betwen HOLC grade an mobility: hOLC_A neighorhoods
# have higher mobility (the average child from a 25th percentile household ends up in the
# 44th percentile), and each decrease in HOLC grade corresponds with a 2-3 point decrease
# in absolute mobility at the 25th percentile.

#7b
mean(subA$kfr_black_pooled_p25, na.rm=TRUE) # Mean: 34.43756
mean(subB$kfr_black_pooled_p25, na.rm=TRUE) # Mean: 34.50712
mean(subC$kfr_black_pooled_p25, na.rm=TRUE) # Mean: 33.23932
mean(subD$kfr_black_pooled_p25, na.rm=TRUE) # Mean: 31.61861

mean(subA$kfr_white_pooled_p25, na.rm=TRUE) # Mean: 50.36435
mean(subB$kfr_white_pooled_p25, na.rm=TRUE) # Mean: 48.75348
mean(subC$kfr_white_pooled_p25, na.rm=TRUE) # Mean: 46.32723
mean(subD$kfr_white_pooled_p25, na.rm=TRUE) # Mean: 44.11593

# It seems that segregation drives a lot of the effect HOLC grade has on mobility. Whereas
# the overall difference between kfr_pooled_pooled_p25 for a HOLC_A and HOLC_D grade
# neighborhood if 8 percentile ranks, the difference shrinks to only 3 points when looking
# at Black children and 6 points when looking at white children. This indicates HOLC grade
# matters more if you are white. More importantly, we see the difference in mobility
# is driven primarily by race; a white child's absolute mobility at the 25th percentile
# is at least 10 points higher than a Black child, regardless of HOLC grade.

#8a
compare_outcomes <- function(xvar, yvar) {
  ggplot(sub3, aes(x=xvar, y=yvar)) +
    stat_smooth(method="lm", se=FALSE) +
    stat_binmean(n=50, geom="point")
}

compare_outcomes(sub3$kfr_pooled_pooled_p25, sub3$singleparent_share2010)
compare_outcomes(sub3$kfr_pooled_pooled_p25, sub3$mail_return_rate2010)
compare_outcomes(sub3$kfr_pooled_pooled_p25, sub3$gsmn_math_g3_2013)

#8b
correlation_coeff <- function(slope, xvar, yvar) {
  slope * (sd(xvar, na.rm=TRUE) / sd(yvar, na.rm=TRUE))
}

mod1 <- lm(kfr_pooled_pooled_p25 ~ singleparent_share2010, data=sub3) # Slope: -21.98
mod2 <- lm(kfr_pooled_pooled_p25 ~ mail_return_rate2010, data=sub3) # Slope: 0.6705
mod3 <- lm(kfr_pooled_pooled_p25 ~ gsmn_math_g3_2013, data=sub3) # Slope: 4.529

correlation_coeff (-21.98, sub3$singleparent_share2010, sub3$kfr_pooled_pooled_p25) # -0.7083237
correlation_coeff (0.6705, sub3$mail_return_rate2010, sub3$kfr_pooled_pooled_p25) # 0.6949079
correlation_coeff (4.529, sub3$gsmn_math_g3_2013, sub3$kfr_pooled_pooled_p25) # 0.6443461

#9
compare_outcomes(sub3$kfr_black_pooled_p25, sub3$singleparent_share2010)
compare_outcomes(sub3$kfr_black_pooled_p25, sub3$mail_return_rate2010)
compare_outcomes(sub3$kfr_black_pooled_p25, sub3$gsmn_math_g3_2013)
compare_outcomes(sub3$kfr_white_pooled_p25, sub3$singleparent_share2010)
compare_outcomes(sub3$kfr_white_pooled_p25, sub3$mail_return_rate2010)
compare_outcomes(sub3$kfr_white_pooled_p25, sub3$gsmn_math_g3_2013)
compare_outcomes(sub3$kfr_asian_pooled_p25, sub3$gsmn_math_g3_2013)
compare_outcomes(sub3$kfr_hisp_pooled_p25, sub3$singleparent_share2010)

modb1 <- lm(kfr_black_pooled_p25 ~ singleparent_share2010, data=sub3) # Slope: -11.24
modb2 <- lm(kfr_black_pooled_p25 ~ mail_return_rate2010, data=sub3) # Slope: 0.3212
modb3 <- lm(kfr_black_pooled_p25 ~ gsmn_math_g3_2013, data=sub3) # Slope: 2.693

correlation_coeff (-11.24, sub3$singleparent_share2010, sub3$kfr_black_pooled_p25) # -0.395682
correlation_coeff (0.3212, sub3$mail_return_rate2010, sub3$kfr_black_pooled_p25) # 0.3636469
correlation_coeff (2.693, sub3$gsmn_math_g3_2013, sub3$kfr_black_pooled_p25) # 0.4185325

modw1 <- lm(kfr_white_pooled_p25 ~ singleparent_share2010, data=sub3) # Slope: -17.73
modw2 <- lm(kfr_white_pooled_p25 ~ mail_return_rate2010, data=sub3) # Slope: 0.5440
modw3 <- lm(kfr_white_pooled_p25 ~ gsmn_math_g3_2013, data=sub3) # Slope: 3.43

correlation_coeff (-17.73, sub3$singleparent_share2010, sub3$kfr_white_pooled_p25) # -0.6042389
correlation_coeff (0.5440, sub3$mail_return_rate2010, sub3$kfr_white_pooled_p25) # 0.5962429
correlation_coeff (3.43, sub3$gsmn_math_g3_2013, sub3$kfr_white_pooled_p25) # 0.5160679

# In this analysis, we examine the patterns found in #8 by race in Ohio. We will only be
# examining kfr_black_pooled_p25 and kfr_white_pooled_p25 as there is not enough data on Asian
# and Hispanic populations to consTruct a tightly bound regression (see figures below).
# We first map the binned scatter plots for each group by variable.
# Outcomes for both white and Black children are strongly negatively correlated with the
# share of single parent households in the area and strongly positively correlated with
# the rate of mail return on Census forms and 3rd grade math test scores. However,
# correlation coefficients are higher by a factor of up to 2 for white children on all
# metrics. For example, white children are about 50% more likely to be negatively affected
# by the share of single parents in the area. This suggests that race is a large driver of
# economic outcomes for Black children, perhaps more so than the other factors listed.

