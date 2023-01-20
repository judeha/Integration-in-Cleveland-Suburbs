if (!require(haven)) install.packages("haven"); library(haven)
if (!require(sandwich)) install.packages("sandwich"); library(sandwich)
if (!require(lmtest)) install.packages("lmtest"); library(lmtest)
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if (!require(statar)) install.packages("statar"); library(statar)

setwd("~/Library/Mobile Documents/com~apple~CloudDocs/Harvard, Year 1/Semester 2/ECON 50/E1")
atlas <- read_dta("atlas.dta")

ohio_data = subset(atlas, state==39)
cleveland_data = subset(ohio_data, county==35)
mean(cleveland_data$kfr_white_pooled_p25, na.rm=TRUE) #46.52053
mean(cleveland_data$kfr_black_pooled_p25, na.rm=TRUE) #30.82332

calculate_corr_coeff <- function(yvar, xvar) {
  mod <- lm(yvar ~ xvar, data=cleveland_data)
  a <- mod[[1]]
  slope <- a[[2]]
  sdx <- sd(xvar, na.rm=TRUE)
  sdy <- sd(yvar, na.rm=TRUE)
  slope * (sdx / sdy)
}

college = calculate_corr_coeff(cleveland_data$kfr_pooled_pooled_p25, cleveland_data$frac_coll_plus2010)
foreign = calculate_corr_coeff(cleveland_data$kfr_pooled_pooled_p25, cleveland_data$foreign_share2010)
income = calculate_corr_coeff(cleveland_data$kfr_pooled_pooled_p25, cleveland_data$med_hhinc2016)
poverty = calculate_corr_coeff(cleveland_data$kfr_pooled_pooled_p25, cleveland_data$poor_share2010)
white = calculate_corr_coeff(cleveland_data$kfr_pooled_pooled_p25, cleveland_data$share_white2010)
black = calculate_corr_coeff(cleveland_data$kfr_pooled_pooled_p25, cleveland_data$share_black2010)
mathscores = calculate_corr_coeff(cleveland_data$kfr_pooled_pooled_p25, cleveland_data$gsmn_math_g3_2013)
rent = calculate_corr_coeff(cleveland_data$kfr_pooled_pooled_p25, cleveland_data$rent_twobed2015)
singleparent = calculate_corr_coeff(cleveland_data$kfr_pooled_pooled_p25, cleveland_data$singleparent_share2010)
travel = calculate_corr_coeff(cleveland_data$kfr_pooled_pooled_p25, cleveland_data$traveltime15_2010)
employment = calculate_corr_coeff(cleveland_data$kfr_pooled_pooled_p25, cleveland_data$emp2000)
wagegrowth = calculate_corr_coeff(cleveland_data$kfr_pooled_pooled_p25, cleveland_data$ln_wage_growth_hs_grad)
mailreturn = calculate_corr_coeff(cleveland_data$kfr_pooled_pooled_p25, cleveland_data$mail_return_rate2010)
jobstotal = calculate_corr_coeff(cleveland_data$kfr_pooled_pooled_p25, cleveland_data$jobs_total_5mi_2015)
jobshigh = calculate_corr_coeff(cleveland_data$kfr_pooled_pooled_p25, cleveland_data$jobs_highpay_5mi_2015)
popdensity = calculate_corr_coeff(cleveland_data$kfr_pooled_pooled_p25, cleveland_data$popdensity2010)
jobgrowth = calculate_corr_coeff(cleveland_data$kfr_pooled_pooled_p25, cleveland_data$ann_avg_job_growth_2004_2013)
jobdensity = calculate_corr_coeff(cleveland_data$kfr_pooled_pooled_p25, cleveland_data$job_density_2013)
mean(cleveland_data$kfr_pooled_pooled_p25, na.rm=TRUE)
median(cleveland_data$kfr_black_pooled_p25, na.rm=TRUE)
median(cleveland_data$kfr_white_pooled_p25, na.rm=TRUE)
median(atlas$kfr_pooled_pooled_p25, na.rm=TRUE)
median(atlas$kfr_black_pooled_p25, na.rm=TRUE)
median(atlas$kfr_white_pooled_p25, na.rm=TRUE)
median(ohio_data$kfr_pooled_pooled_p25, na.rm=TRUE)
median(ohio_data$kfr_black_pooled_p25, na.rm=TRUE)
median(ohio_data$kfr_white_pooled_p25, na.rm=TRUE)

ggplot(cleveland_data, aes(x=share_white2010, y=kfr_pooled_pooled_p25)) +
  stat_smooth(method="lm", se=FALSE) +
  stat_binmean(n=20, geom="point")

corr_coeffs = c(college, foreign, income, poverty, white, black, mathscores, rent, singleparent,
               travel, employment, wagegrowth, mailreturn, jobstotal, jobshigh, popdensity, jobgrowth, jobdensity)

barplot(corr_coeffs, main = "Covariate Correlation Coefficients",
        ylab = "Correlation Coefficient",
        xlab="Covariate")

sh183604 = subset(cleveland_data, tract==183604)
sh183605 = subset(cleveland_data, tract==183605)
sh183606 = subset(cleveland_data, tract==183606)
sh183502 = subset(cleveland_data, tract==183502)
sh183501 = subset(cleveland_data, tract==183501)
sh183200 = subset(cleveland_data, tract==183200)
sh183300 = subset(cleveland_data, tract==183300)
sh183100 = subset(cleveland_data, tract==183100)
shaker_data <- rbind(rbind(rbind(rbind(rbind(rbind(rbind(sh183604, sh183605), sh183606), sh183502),
                     sh183501), sh183200), sh183300), sh183100)

mean(shaker_data$kfr_pooled_pooled_p25, na.rm=TRUE)
mean(shaker_data$kfr_black_pooled_p25, na.rm=TRUE)
mean(shaker_data$kfr_white_pooled_p25, na.rm=TRUE)

bwb193900 = subset(cleveland_data, tract==193900)
bwg131102 = subset(cleveland_data, tract==131102)
bwg131103 = subset(cleveland_data, tract==131103)
bwg131104 = subset(cleveland_data, tract==131104)
beachwood_good <- rbind(bwg131102, rbind(bwg131103, bwg131104))
beachwood_data <- rbind(bwb193900, beachwood_good)
mean(bwb193900$kfr_pooled_pooled_p25, na.rm=TRUE)
mean(bwb193900$kfr_black_pooled_p25, na.rm=TRUE)
mean(bwb193900$kfr_white_pooled_p25, na.rm=TRUE)

calculate_corr_coeff_shaker <- function(yvar, xvar) {
  mod <- lm(yvar ~ xvar, data=shaker_data)
  a <- mod[[1]]
  slope <- a[[2]]
  sdx <- sd(xvar, na.rm=TRUE)
  sdy <- sd(yvar, na.rm=TRUE)
  slope * (sdx / sdy)
}

calculate_corr_coeff_beachwood <- function(yvar, xvar) {
  mod <- lm(yvar ~ xvar, data=beachwood_data)
  a <- mod[[1]]
  slope <- a[[2]]
  sdx <- sd(xvar, na.rm=TRUE)
  sdy <- sd(yvar, na.rm=TRUE)
  slope * (sdx / sdy)
}

white_shaker_covar = calculate_corr_coeff_shaker(shaker_data$kfr_pooled_pooled_p25, shaker_data$share_white2010)
#0.589
black_shaker_covar = calculate_corr_coeff_shaker(shaker_data$kfr_pooled_pooled_p25, shaker_data$share_black2010)
#-0.49
poverty_shaker_covar = calculate_corr_coeff_shaker(shaker_data$kfr_pooled_pooled_p25, shaker_data$poor_share2010)
#-0.693
singleparent_shaker_covar = calculate_corr_coeff_shaker(shaker_data$kfr_pooled_pooled_p25, shaker_data$singleparent_share2010)
#-0.434
hhinc_shaker_covar = calculate_corr_coeff_shaker(shaker_data$kfr_pooled_pooled_p25, shaker_data$med_hhinc2016)

black_job_shaker_covar = calculate_corr_coeff_shaker(shaker_data$kfr_black_pooled_p25, shaker_data$jobs_highpay_5mi_2015)
mod1 <- lm(kfr_pooled_pooled_p25 ~ share_black2010, data=shaker_data)
mean(shaker_data$med_hhinc2016) #$96,460.75
mean(shaker_data$frac_coll_plus2000)
mean(shaker_data$poor_share2010)
mean(shaker_data$jobs_highpay_5mi_2015)
mean(shaker_data$kfr_pooled_pooled_p25)
mean(shaker_data$kfr_black_pooled_p25)
mean(shaker_data$kfr_white_pooled_p25)

white_beachwood_covar = calculate_corr_coeff_beachwood(beachwood_data$kfr_pooled_pooled_p25, beachwood_data$share_white2010)
#0.98
black_beachwood_covar = calculate_corr_coeff_beachwood(beachwood_data$kfr_pooled_pooled_p25, beachwood_data$share_black2010)
#-0.97'
poverty_beachwood_covar = calculate_corr_coeff_beachwood(beachwood_data$kfr_pooled_pooled_p25, beachwood_data$poor_share2010)
#-0.946
singleparent_beachwood_covar = calculate_corr_coeff_beachwood(beachwood_data$kfr_pooled_pooled_p25, beachwood_data$singleparent_share2010)
#-0.813
hhinc_beachwood_covar = calculate_corr_coeff_beachwood(beachwood_data$kfr_pooled_pooled_p25, beachwood_data$med_hhinc2016)

beachwood_coeffs = c(white_beachwood_covar, black_beachwood_covar, poverty_beachwood_covar, singleparent_beachwood_covar, hhinc_beachwood_covar)
shaker_coeffs = c(white_shaker_covar, black_shaker_covar, poverty_shaker_covar, singleparent_shaker_covar, hhinc_shaker_covar)
table <- rbind(beachwood_coeffs, shaker_coeffs)
barplot(table,
        main = "Grouped barchart",
        xlab = "Transmission type", ylab = "Frequency",
        col = c("mistyrose", "lightcyan"),
        legend.text = rownames(table),
        beside = TRUE) # Grouped bars

mean(beachwood_data$med_hhinc2016) #$73,083.25
mean(beachwood_data$frac_coll_plus2000)
mean(beachwood_data$poor_share2010)
mean(beachwood_data$jobs_highpay_5mi_2015)
mean(beachwood_data$kfr_pooled_pooled_p25)
mean(beachwood_data$kfr_black_pooled_p25)
mean(beachwood_good$share_black2010)
mean(bwb193900$kfr_black_pooled_p25)
mean(beachwood_good$kfr_black_pooled_p25)
mean(beachwood_data$kfr_white_pooled_p25, na.rm=TRUE)


