# Maria Kartika

#clear the workspace
rm(list=ls()) # removes all objects from the environment
cat('\014') # clears the console

#Install and load haven package
if (!require(haven)) install.packages("haven"); library(haven)

#Change working directory and load stata data set
setwd("/Users/mariapkartika/Documents/Ec50")
nls <- read_dta("nls6679.dta")

#1
#Histogram in base R
hist(nls$kid_income, probability = T, main="Histogram of Kid Income", xlab="Kid Income", ylab="Density", border="black", col="green", breaks=10)

#Saving a histogram drawn in base R
png("histogram_yvar.png") 
hist(nls$kid_income, probability = T)
dev.off()

#Histogram using ggplot
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
ggplot(nls) +  geom_histogram(aes(x=kid_income, y=..density..)) + theme_classic()

ggsave("histogram_kid_income.png")

#Use 50 bins, overriding default
ggplot(nls) + geom_histogram(aes(x=kid_income, y=..density..), bins = 50) + theme_bw()

# summary stats, unweighted
summary(nls$kid_income)
mean(nls$kid_income, na.rm=TRUE)
sd(nls$kid_income, na.rm=TRUE)
median(nls$kid_income, na.rm=TRUE)

#Create new indicator variable called below_mean
nls$below_mean <- 0
nls$below_mean[which(nls$kid_income >= 0.5)] <- 1

#Sample mean of below_mean
summary(nls$below_mean)

#Indicator for being between two values

#Step 1: define the upper value and the lower value
upper_bound <- 52.1
lower_bound <- 32.1

#Step 2: use ifelse function to define the indicator variable
nls$dvar <- ifelse(nls$xvar <= upper_bound & nls$xvar >= lower_bound, 1, 0)

mean(nls$below_mean)
fraction_below_mean <- mean(nls$below_mean)

#Report the result
cat("Fraction of observations with kid_income below its mean:", fraction_below_mean, "\n")

mean_income <- mean(nls$kid_income, na.rm = TRUE)
nls$below_mean1 <- ifelse(nls$kid_income < mean_income, 1, 0)

fraction_below_mean <- mean(nls$below_mean1)

median_income <- median(nls$kid_income, na.rm = TRUE)
cat ("Median of kid_income:", median_income,"\n")

sd_income <- sd(nls$kid_income, na.rm = TRUE)
cat ("Median of kid_income:",sd_income,"dollars\n")

nls$within_one_sd <- ifelse(abs(nls$kid_income-mean_income) <= sd_income, 1,0)
nls$within_two_sd <- ifelse(abs(nls$kid_income-mean_income) <= 2*sd_income, 1, 0)

fraction_within_one_sd <- mean(nls$within_one_sd)
fraction_within_two_sd <- mean(nls$within_two_sd)
cat("Fraction of observation within two standard deviations of the mean:", fraction_within_two_sd,"\n")
cat("Fraction of observation within two standard deviations of the mean:", fraction_within_one_sd,"\n")

nls$kid_ink_raw <- rank(nls$kid_income)
nls_sorted <- nls[order(nls$kid_income),]
print (nls_sorted)

max_rank <- max(nls$kid_ink_raw, na.rm=TRUE)
nls$kid_ink_raw <- (nls$kid_ink_raw/max_rank)*100
print (nls)

ggplot(nls, aes(x=kid_ink_raw)) + geom_histogram (binwidth=5, fill = "lightgreen", color = "black", alpha = 0.7) + theme_classic() + ggtitle ("Histogram of Kids Income") + xlab("Percentile Rank") + ylab("Frequency") + theme_classic()
ggsave("kid_inc_rank_histogram.png", width = 8, height = 6)


mean_rank_raw <- mean(nls$kid_ink_raw, na.rm = TRUE)
median_rank_raw <- median(nls$kid_ink_raw, na.rm = TRUE)
cat("The mean of kid_inc_raw:", mean_rank_raw, "\n")
cat("The median of kid_inc_raw:", median_rank_raw, "\n")

ggplot(nls, aes(x=parent_income, y=kid_income)) + geom_point(alpha = 0.5, size =2) + ggtitle("Scatter Plot of Kid Income vs Parent Income") + xlab("Parent Income (Dollar)") + ylab("Kid Income (Dollar)") + theme_minimal()

ggsave("scatter_plot_income_relationship.png", width = 8, height = 6)

ggplot(nls, aes(x=parent_income, y=kid_income)) + geom_bin2d(binwidth = c(5000,5000), bins = 20, alpha = 0.7) + ggtitle ("Binned Scatter Plot of Kid Income vs Parent Outcome") + xlab("Parent Income (Dollar)") + ylab("Kid Income (Dollars)")+ theme_minimal()
ggsave("Binned_scatter_plot_income.png", width=8, height =6)

set.seed(11647820)
nls$random_number <- runif(nrow(nls))
nls$treatment_group <- ifelse(nls$random_number >= 0.5, 1, 0)
num_treatment <- sum(nls$treatment_group==1)
num_control <- sum(nls$treatment_group==0)
cat("Number of observations in the treatment group:", num_treatment, "\n")
cat("Number of observations in the control group:", num_control, "\n")

variables_to_summarize <- c("kid_income", "parent_income")
treatment_group_data <- nls[nls$treatment_group == 1, ]
control_group_data <- nls[nls$treatment_group == 0, ]

summary_treatment <- sapply(treatment_group_data[variables_to_summarize], function(x) c(mean=mean(x, na.rm=TRUE), sd=sd(x, na.rm=TRUE)))
summary_control <- sapply(control_group_data[variables_to_summarize], function(x) c(mean=mean(x, na.rm=TRUE), sd=sd(x, na.rm=TRUE)))
print(summary_treatment)
print(summary_control)



