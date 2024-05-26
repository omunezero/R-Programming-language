
# R with chris

x <- c(1,2,3,4,5)
sd(x)

sd2 <- function(mune){sqrt(var(mune))}
sd2(x)
save.sd <- sd2(x)

objects()
ls()
function(mune){cat("my data \n", mune, "\n") 
  
  sqrt(var(mune))}

save.sd <- sd2(x)
pnorm(q=1.96)
pnorm(q=1.96, mean = 0, sd=1)

pnorm(q = c(-1.96, 1.96))

c(-1.96, 1.96)


x <- c(3.68, -3.63, 0.80, 3.03, -9.86, -8.66, 
         -2.38, 8.94, 0.52, 1.25) 
y <- c(0.55, 1.65, 0.98, -0.07, -0.01, -0.31, 
         -0.34, -1.38, -1.32, 0.53)
x[1]+y[2]
pnorm(x)


getwd()

setwd("/Users/oliviermunezero/Desktop/ChrisBilder_R")

library(tidyverse)
library(tidyr)

# Read the dataset in R

GPA <- read_csv("/Users/oliviermunezero/Desktop/ChrisBilder_R/gpa.csv")
names(GPA)
colnames(GPA)
GPA$HSGPA[2]
GPA$CollegeGPA[2]

GPA$CollegeGPA[1]

GPA$CollegeGPA[1:3]

GPA[c(1,2,3), 2]
GPA[2]

GPA["HSGPA"]
GPA[,1] %>% GPA[2,1]
# Some statistics
sum <- summary(GPA)

 plot(x=GPA$HSGPA, y=GPA$CollegeGPA, xlab = "HSGPA", ylab = "(CollegeGPA", 
      main = "CollegeGPAvs. HSGPA", xlim = c(0,4.5), ylim = c(0,4.0), col="green", pch=1, cex=1.0, 
 panel.first = grid(col = "red", lty = 2)) %>% abline()
 
 library(ggplot2)
 
 install.packages("RColorBrewer")
 library(RColorBrewer)

 ggplot(GPA, aes(HSGPA, CollegeGPA, fill="blue")) + geom_point()+labs(x="High School GPA", 
           y="Collge GPA", title = "High School GPA vsCollge GPA")+geom_smooth(method = "lm")
  
install.packages("Rcmdr")
library(Rcmdr)
pnorm(1.96)  

a <- c(1,2,3,4,5)

sd(a)

sd2 <- function(x){sqrt(var(x))}
sd2(a)

regression <- lm(CollegeGPA~HSGPA, data = GPA)

names(regression)
regression$coefficients
regression$fitted.values
regression$rank
regression$

# creating new data.frome from GPA
  
  install.packages("math")

new_GPA <- data.frame(GPA,GPAhat=regression$fitted.values , Residuals=regression$residuals)



?round

library(stats)

