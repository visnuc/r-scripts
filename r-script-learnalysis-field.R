# ---------------------------------
# libraries
# ---------------------------------
library(arm)
library(car); library(codebook); library(coefplot)
library(datasets); library(devtools); library(dplyr)
library(e1071)
library(foreign)
library(ggplot2); library(ggpubr); library(ggthemes); library(grid); library(gridExtra)
library(haven); library(hrbrthemes)
library(MASS); library(moments)
library(pastecs); library(plotrix); library(plyr); library(psych)
library(RColorBrewer); library(reshape); library(rstatix)
library(tidyverse)
library(utils)
# library(Rcmdr) # do not load unless needed, messes with exporting plots

# ---------------------------------
#   set working directory
# ---------------------------------
setwd("/home/visnu/MEGA/800x/mathematics/statistics/discovering_statistics_using_r_-_field_miles_field/DSUR_field_miles_datafiles"); getwd()

# ---------------------------------
#   list files in dir 
# ---------------------------------
dir()

# ---------------------------------
#   import data
# ---------------------------------
Data <- read.csv(file.choose(), header = T)
Data <- read.delim(file.choose(), header = T)
Data <- read.dta(file.choose())
Data <- read.spss(file.choose(), header = T)

# ---------------------------------
#   others 
# ---------------------------------
attach(Data); names(Data); View(Data)
dim(Data)
str(Data)

# ---------------------------------
# normality check - with histogram
# ---------------------------------
hist.day1 <- ggplot(Data, aes(day1)) + 
  # opts(legend.position = "none") +
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  labs(x = "Hygiene score on day 1", y = "Density") + 
  stat_function(fun = dnorm, args = list(mean = mean(Data$day1, na.rm = TRUE), sd = sd(Data$day1, na.rm = TRUE)), 
                colour = "#ff0000", size = 0.25); hist.day1

# hist.day2 <- ggplot(Data, aes(day2)) + 
#   # opts(legend.position = "none") +
#   geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
#   labs(x = "Hygiene score on day 2", y = "Density") + 
#   stat_function(fun = dnorm, args = list(mean = mean(Data$day2, na.rm = TRUE), sd = sd(Data$day2, na.rm = TRUE)), 
#                 colour = "#ff0000", size = 0.25); hist.day2
# 
# hist.day3 <- ggplot(Data, aes(day3)) + 
#   # opts(legend.position = "none") +
#   geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
#   labs(x = "Hygiene score on day 3", y = "Density") + 
#   stat_function(fun = dnorm, args = list(mean = mean(Data$day3, na.rm = TRUE), sd = sd(Data$day3, na.rm = TRUE)), 
#                 colour = "#ff0000", size = 0.25); hist.day3

# ---------------------------------
# normality check - with q-q plot
# ---------------------------------
qqplot.day1 <- qplot(sample = Data$day1, stat="qq"); qqplot.day1
qqplot.day2 <- qplot(sample = Data$day2, stat="qq"); qqplot.day2
qqplot.day3 <- qplot(sample = Data$day3, stat="qq"); qqplot.day3

# ---------------------------------
# normality check - with skew and kurtosis 
#     values ≈0 for normal distribution
# ---------------------------------
round(describe(cbind(Data$day1, Data$day2, Data$day3)), digits = 3)
round(stat.desc(cbind(Data$day1, Data$day2, Data$day3), basic = F, norm = T), digits = 3)
# describe(Data[,c("day1", "day2", "day3")])
# stat.desc(Data[, c("day1", "day2", "day3")], basic = F, norm = T)



# -------------------------------
#   export data
# ---------------------------------
write.table(Data, file = "later.csv", sep = ",", row.names = F)

# ---------------------------------
#   subsetting 
# ---------------------------------
Data2 <- subset(Data, select = c(var_1, var_2, var_3))

# var_old_name = varNewName

# ---------------------------------
#   conditional variable 
# ---------------------------------
Data$varNewName <- ifelse(Data$var_1 == 1 | Data$var_1 == 1 , 1, 0)
Data$varNewName <- ifelse(Data$var_1 == 1 & Data$var_1 == 1 , 1, 0)

# ---------------------------------
#     removing variables 
# ---------------------------------
Data$var_1 <- NULL

# ---------------------------------
#   recoding
# ---------------------------------
Data$var_1 <- recode(Data$var_1, "Positive" = 1, "Negative" = 0)

# ---------------------------------
# two-way contingency table
# ---------------------------------
continTable <- table(Data$var_dependent, Data$var_independent); continTable

rownames(continTable) <- c("response of dependent var", "response of dependent var")
colnames(continTable) <- c("response of independent var", "response of independent var")

continTable

# relative frequencies percentage 
prop.table(continTable)*100
prop.table(continTable, 1)*100 # conditional, row-wise
prop.table(continTable, 2)*100 # conditional, column-

# -------------------------------
#   Chi-square test of independence
# ---------------------------------
chisq.test(continTable)
chisq.test(continTable)$observed
chisq.test(continTable)$expected

# Monte Carlo simulation as expected freq <5
chisq.test(continTable, simulate.p.value = T, B = 10000) 

# -------------------------------
#   Fisher's Exact test
# -------------------------------
fisher.test(continTable)

# -------------------------------
#   Binary logistic regression - unadjusted
# -------------------------------

# final models - unadjusted 
model_lr <- glm(var_dependent ~ var_independent, family = "binomial", data = Data); summary(model_lr)

# OR & 95% CI
round(exp(cbind(coef(model_lr), confint(model_lr))), 3)

# -------------------------------
#   Binary logistic regression - adjusted
# -------------------------------
model_lr <- glm(var_dependent ~ var_independent_1 + var_independent_2, 
                  family = "binomial" (link="logit"), data = Data); summary(model_lr)

# OR & 95% CI
round(exp(cbind(coef(model_lr), confint(model_lr))), 3)

# -------------------------------
#   Coefficient plot - multiple
# -------------------------------
# Thanks to https://github.com/dsparks

# renaming variables for plot 
set <- lab_set_01
ipgA_icsB <- reg_ipgA_icsB
mxiC <- reg_mxiC
ipgB1_spa15 <- reg_ipgB1_spa15

t3ssRegAdj1 <- glm(ho_bloody_01 ~ set + ipgA_icsB, family = "binomial" (link="logit"), data = t3ss) 
t3ssRegAdj2 <- glm(ho_mucoid_01 ~ set + mxiC, family = "binomial" (link="logit"), data = t3ss)
t3ssRegAdj3 <- glm(ho_re_str_01 ~ set + ipgA_icsB + mxiC + ipgB1_spa15, family = "binomial" (link="logit"), data = t3ss)

# Put model estimates into temporary data.frames 
model1Frame <- data.frame(Variable = rownames(summary(t3ssRegAdj1)$coef),
                          Coefficient = summary(t3ssRegAdj1)$coef[, 1],
                          SE = summary(t3ssRegAdj1)$coef[, 2],
                          modelName = "Bloody stool")
model2Frame <- data.frame(Variable = rownames(summary(t3ssRegAdj2)$coef),
                          Coefficient = summary(t3ssRegAdj2)$coef[, 1],
                          SE = summary(t3ssRegAdj2)$coef[, 2],
                          modelName = "Mucoid stool")
model3Frame <- data.frame(Variable = rownames(summary(t3ssRegAdj3)$coef),
                          Coefficient = summary(t3ssRegAdj3)$coef[, 1],
                          SE = summary(t3ssRegAdj3)$coef[, 2],
                          modelName = "Rectal straining")

# Combine these data.frames 
allModelFrame <- data.frame(rbind(model1Frame, model2Frame, model3Frame))  # etc.

# Widths of your confidence intervals 
interval1 <- -qnorm((1-0.9)/2) # 90% multiplier
interval2 <- -qnorm((1-0.95)/2) # 95% multiplier

# Plot 
mPlots <- ggplot(allModelFrame, aes(colour = modelName)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2) + 
  geom_linerange(aes(x = Variable, 
                     ymin = Coefficient - SE*interval1, 
                     ymax = Coefficient + SE*interval1), 
                 lwd = 1, position = position_dodge(width = 1/2)) + 
  geom_pointrange(aes(x = Variable, 
                      y = Coefficient, 
                      ymin = Coefficient - SE*interval2, 
                      ymax = Coefficient + SE*interval2), 
                  lwd = 1/2, position = position_dodge(width = 1/2),
                  shape = 21, fill = "WHITE") + 
  coord_flip() + 
  theme_bw() 
# + ggtitle("Comparing several models")

print(mPlots)  # The trick is position_dodge()


# -------------------------------
#   coefficient plot - individual 
# -------------------------------

coefplot3 <- coefplot(t3ssRegAdj3, innerCI = 0, outerCI = 1.96, intercept = F,
                      title = "",
                      xlab = "Regression coefficient at 95% CI",
                      ylab = "Predictor genes",
                      decreasing = T,
                      col = "skyblue2",
                      newNames = c(reg_toxin = "set & sen", 
                                   reg_ipgA_icsB = "ipgA & icsB", 
                                   reg_mxiC = "mxiC",
                                   reg_ipgB1_spa15 = "ipgB1 & spa15")) +
  theme(axis.line = element_line(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) + 
  geom_point(pch = 21)



# -------------------------------
#   barplots - 3 in 1
# -------------------------------
seroDf <- data.frame(seroNames = c("1b", "1c", "2a", "2b", "3a", "4", "6a"), 
                     seroCount = c(5, 9, 16, 15, 8, 3, 5), 
                     seroPrcnt = c(8.20, 14.75, 26.23, 24.59, 13.14, 4.92, 8.20)); 
seroDf$seroNames <- factor(seroDf$seroNames, levels = c("6a", "4", "3a", "2b", "2a", "1c", "1b"))

cfDf <- data.frame(cfNames = c("Bloody stool", "Mucoid stool", "Rectal straining", "Fever", "Cough"), 
                   cfCount = c(49, 50, 45, 22, 25), 
                   cfPrcnt = c(80.33, 81.97, 73.77, 36.07, 40.98)); 
# "Severe disease", "Abdominal pain", "Vomiting", "Dehydration", "Convulsion"
# 55, 52, 28, 13, 2
# 90.16, 85.24, 45.90, 21.31, 3.28
cfDf$cfNames <- factor(cfDf$cfNames, levels = c("Cough", "Fever", "Rectal straining", "Mucoid stool", "Bloody stool"))
# "Convulsion", "Dehydration", "Vomiting", "Abdominal pain", "Severe disease"

gnDf <- data.frame(gnNames = c("p140", 
                               "ipaH", "ial", "set", "sen", "virB", "ipaBCD", "ipgC", "ipgB1", "ipgA", "icsB",
                               "ipgD", "ipgE", "ipgF", "mxiH", "mxiI", "mxiK", "mxiE", "mxiC", "spa15", "spa47", 
                               "spa32", "spa24"),
                   gnCount = c(48, 
                               61, 54, 34, 38, 50, 55, 52, 44, 50, 27, 
                               48, 52, 47, 49, 49, 43, 40, 22, 49, 49, 
                               46, 43), 
                   gnPrcnt = c(78.69, 
                               100, 88.52, 55.74, 62.30, 81.97, 90.16, 85.25, 72.13, 81.97, 44.26, 
                               78.69, 85.25, 77.05, 80.33, 80.33, 70.49, 65.57, 36.07, 80.33, 80.33, 
                               75.41, 70.49))
gnDf$gnNames <- factor(gnDf$gnNames, 
                       levels = c("spa24","spa32", "spa47", "spa15", 
                                  "mxiC", "mxiE", "mxiK", "mxiI", "mxiH", 
                                  "ipgF", "ipgE", "ipgD", "icsB", "ipgA", "ipgB1", "ipgC", "ipaBCD", "virB", 
                                  "sen", "set", "ial", "ipaH", "p140"))

bpGn <- ggplot(data = gnDf, aes(x = gnNames, y = gnPrcnt)) +
  geom_bar(stat = "identity", width = 0.65, fill = "#6994c0", color = "#565656") +
  geom_text(aes(label = gnPrcnt), hjust = 1.2, vjust = 0.5, colour = "#ffffff") + 
  # scale_color_grey() +
  theme_minimal() +
  # theme_classic() +
  # scale_fill_grey() +
  # scale_fill_brewer(palette="Blues") +
  # scale_color_manual(values=seroColor) + 
  theme(legend.position="none") +
  # labs(title = "Frequencies of different serotypes") +
  xlab("Genes and Plasmid (p140)") +
  ylab("Percentage (%)") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 100))

bpCf <- ggplot(data = cfDf, aes(x = cfNames, y = cfPrcnt)) +
  geom_bar(stat = "identity", width = 0.35, fill = "#F77262", color = "#565656") +
  geom_text(aes(label = cfPrcnt), hjust = 1.2, vjust = 0.5, colour = "#ffffff") + 
  theme_minimal() + 
  theme(legend.position="none") +
  xlab("Clinical features") +
  ylab("Percentage (%)") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 100, 10), limits = c(0, 82))

bpSero <- ggplot(data = seroDf, aes(x = seroNames, y = seroPrcnt)) +
  geom_bar(stat = "identity", width = 0.5, fill = "#a8a8a8", color = "#565656") +
  geom_text(aes(label = seroPrcnt), hjust = 1.2, vjust = 0.5, colour = "#ffffff") + 
  theme_minimal() +
  theme(legend.position="none") +
  xlab("Serotypes") +
  ylab("Percentage (%)") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 30, 10), limits = c(0, 27))

grid.arrange(bpSero, bpCf, bpGn, layout_matrix = cbind(c(1,2), c(3,3)))
