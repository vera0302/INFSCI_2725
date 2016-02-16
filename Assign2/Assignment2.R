# package rJava, xlsx and xlsxjars are used to tramsform the data frame to xlsx file.
library(rJava)
library(xlsxjars)
library(xlsx)

# package psych contain the "describe" function to generate descriptive statistics
library(psych)


# read txt file "Retention" into a data frame
Retention <- read.table("Retention.txt",header = TRUE)

# create a vector "s" with three specific strings "apret","tstsc","salar" which we used to do some analytics
s <- c("apret","tstsc","salar")

# call describe function to generate descriptive statistics of three specific colunms, and then put the result into a data frame "x"
x <- describe(Retention[s]) #Retention[s] stored three specific colunms of data given by vector"s"

# tramsform data frame "x" to a xlsx file "statistics.xlsx"
write.xlsx(x,"statistics.xlsx",row.names = TRUE,col.names = TRUE)

# generete plot histograms for the following three colunms: apret, tstsc, and salar
# call hist function to draw histograms for a given colunm of data.
# main argument set the head name of histograms
hist(Retention$apret, main="Histogram of apret") 
hist(Retention$tstsc, main="Histogram of tstsc")
hist(Retention$salar, main="Histogram of salar")

# perform linear regression of apret on tstsc
# call the plot function, using the "tstsc" column of countries for the horizontal axis, and the "apret" column for the vertical axis
plot(Retention$apret ~ Retention$tstsc)
# call lm function to fit linear model, given a response variable(apret),and a predictor variable(tstsc),and put the result into a vector Retention.reg
Retention.reg <- lm(Retention$apret ~ Retention$tstsc, data = Retention)
# call abline function to draw a line fitting the data on the plot
abline(Retention.reg , col = 2, lty = 2) 
# call summary function to produce result summaries of the results of various model fitting functions. 
summary(Retention.reg) 
# call anova function to compute an analysis of variance table for the linear model fits.
anova(Retention.reg) 

# perform linear regression of apret on salar
# call the plot function, using the "salar" column of countries for the horizontal axis, and the "apret" column for the vertical axis
plot(Retention$apret ~ Retention$salar)
# call lm function to fit linear model, given a response variable(apret),and a predictor variable(salar)
Retention.reg <- lm(Retention$apret ~ Retention$salar, data = Retention) 
# call abline function to draw a line fitting the data on the plot
abline(Retention.reg, col = 2, lty = 2) 
# call summary function to produce result summaries of the results of various model fitting functions.
summary(Retention.reg) 
# call anova function to compute an analysis of variance table for the linear model fits.
anova(Retention.reg) 


# perform linear regression of apret on tstsc and salar
# call lm function to fit linear mode, given a response variable(apret),and a predictor variable(salar+tstsc)
Retention.reg <- lm(Retention$apret ~ Retention$salar + Retention$tstsc , data = Retention) 
# call summary function to produce result summaries of the results of various model fitting functions.
summary(Retention.reg) 
# call anova function to compute an analysis of variance table for the linear model fits.
anova(Retention.reg)

# clear all data frame and vectors in the environment
rm(list=ls())

