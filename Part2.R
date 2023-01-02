library(ggplot2)
library(sciplot)
library(magrittr) 
library(dplyr)    
library(glmmTMB)
library(plyr)
library(knitr)
library(PerformanceAnalytics)
library(psych)
library(rcompanion)
library(MASS)

################################################################################
################################################################################
#
# DATASET INTRODUCTION AS USUAL
#
################################################################################
################################################################################

dat = read.table("exam2022_part2.txt", header=T) 
head(dat)
names(dat)
View(dat)
str(dat)
dat = na.omit(dat)

################################################################################
# VARIABLES ORGANIZING AND CHECKING PLUS HISTOGRAMS 
################################################################################

sex = as.factor(dat$sex)
density = as.factor(dat$density)
#plot(dat)
horns_difference = dat$hornL - dat$hornR
mean_horns_lenght = (dat$hornL + dat$hornR)/2


oldpar = par(no.readonly = TRUE)


par(mfrow=c(1,2))
par(bg = "ivory")

plotNormalHistogram(horns_difference, prob = FALSE, col="slategray2", border="slategray",
                    main = "Left-Right Horns Difference Histogram", xlab = "Difference Values [mm]",
                    linecol="red", lwd=2 )


plotNormalHistogram(mean_horns_lenght, prob = FALSE, col="slategray2", border="slategray",
                    main = "Left-Right Horns Mean Histogram", xlab = "Mean Values [mm]",
                    linecol="red", lwd=2 )

par(oldpar)


fitdistr(horns_difference, "normal")


fitdistr(mean_horns_lenght, "normal")

# sex as categorical variable

popstats = ddply(dat, .(sex), summarize,
                  hornLm = mean(hornL, na.rm=T),
                  hornLsd = sd(hornL, na.rm=T),
                  hornLse = se(hornL, na.rm=T),
                  hornRm = mean(hornR, na.rm=T),
                  hornRsd = sd(hornR, na.rm=T),
                  hornRse = se(hornR, na.rm=T),
                  massm = mean(mass, na.rm=T),
                  masssd = sd(mass, na.rm=T),
                  massse = se(mass, na.rm=T))
popstats[,-1] = round(popstats[,-1], 2)
kable(popstats)

# population density at birth as categorical variable

popstats2 = ddply(dat, .(density), summarize,
                  hornLm = mean(hornL, na.rm=T),
                  hornLsd = sd(hornL, na.rm=T),
                  hornLse = se(hornL, na.rm=T),
                  hornRm = mean(hornR, na.rm=T),
                  hornRsd = sd(hornR, na.rm=T),
                  hornRse = se(hornR, na.rm=T),
                  massm = mean(mass, na.rm=T),
                  masssd = sd(mass, na.rm=T),
                  massse = se(mass, na.rm=T))
popstats2[,-1] = round(popstats2[,-1], 2)
kable(popstats2)
################################################################################
# TOWARDS 2-WAY ANOVA: LOOKING AT BOXPLOTS
################################################################################

oldpar = par(no.readonly = TRUE)


par(mfrow=c(1,2))
par(bg = "ivory")
boxplot(mean_horns_lenght~sex, xlab="Sex", ylim = c(0, 300),
        ylab="Horns Length (Left-Right Mean) [mm]",boxwex=0.35, main = "Boxplot for the Horn length (mean)", lwd = 2, border= "slategrey", # colour of the box borders
        col = "slategray2", # colour of the inside of the boxes
        col.axis = 'grey20', # colour of the axis numbers 
        col.lab = 'grey20', # colour of the axis labels
        frame = F)
stripchart(mean_horns_lenght~sex,
           method = "jitter",  main = "method = 'jitter', jitter = 0.2",
           pch = 16, # specify the type of point to use
           cex = 1,
           col = c(2,4),
           vertical = TRUE,
           at = c(0.6,1.6),
           add = TRUE)


boxplot(mean_horns_lenght~density, xlab="Population Density at Birth", ylim = c(0, 300),
        ylab="Horns Length (Left-Right Mean) [mm]",boxwex=0.35, main = "Boxplot for the Horn length (mean)", lwd = 2, border= "slategrey", # colour of the box borders
        col = "slategray2", # colour of the inside of the boxes
        col.axis = 'grey20', # colour of the axis numbers 
        col.lab = 'grey20', # colour of the axis labels
        frame = F)
stripchart(mean_horns_lenght~density,
           method = "jitter",  main = "method = 'jitter', jitter = 0.2",
           pch = 16, # specify the type of point to use
           cex = 1,
           col = c(6,7),
           vertical = TRUE, 
           at = c(0.6,1.6),
           add = TRUE)


par(oldpar)



par(mfrow=c(1,2))
par(bg = "ivory")


boxplot(dat$mass~sex, xlab="Sex", ylim = c(0, 50),
        ylab="Body Mass [kg]",boxwex=0.35, main = "Boxplot for the Body Mass", lwd = 2, border= "slategrey", # colour of the box borders
        col = "slategray2", # colour of the inside of the boxes
        col.axis = 'grey20', # colour of the axis numbers 
        col.lab = 'grey20', # colour of the axis labels
        frame = F)
stripchart(dat$mass~sex,
           method = "jitter",  main = "method = 'jitter', jitter = 0.2",
           pch = 16, # specify the type of point to use
           cex = 1,
           col = c(2,4),
           vertical = TRUE,
           at = c(0.6,1.6),
           add = TRUE)


boxplot(dat$mass~density, xlab="Population Density at Birth", ylim = c(0, 50),
        ylab="Body Mass [kg]",boxwex=0.35, main = "Boxplot for the Body Mass", lwd = 2, border= "slategrey", # colour of the box borders
        col = "slategray2", # colour of the inside of the boxes
        col.axis = 'grey20', # colour of the axis numbers 
        col.lab = 'grey20', # colour of the axis labels
        frame = F)
stripchart(dat$mass~density,
           method = "jitter",  main = "method = 'jitter', jitter = 0.2",
           pch = 16, # specify the type of point to use
           cex = 1,
           col = c(6,7),
           vertical = TRUE, 
           at = c(0.6,1.6),
           add = TRUE)
par(oldpar)
################################################################################
# STARTING THE FIRST 2-WAY ANOVA FOR HORNS LENGTH
################################################################################

means = tapply(mean_horns_lenght, list(sex, density), mean)
ses = tapply(mean_horns_lenght,
             list(sex, density),
             function(x) sd(x)/sqrt(sum(!is.na(x))))
means
ses

plot(c(0.97, 1.03), means[,1], ylim=c(160, 200), xlim=c(0.8, 2.2),
     xlab="Population Density at Birth",
     ylab="Horns Length (Left-Right Mean) [mm]",
     xaxt="n", las=1, pch=c(21,16), col="white")
axis(1, 1:2, labels=c("High", "Low"))
arrows(c(0.97,1.03), means[,1]-ses[,1], c(0.97,1.03),
       means[,1]+ses[,1], length=0.05, angle=90, code=3)
arrows(c(1.97,2.03), means[,2]-ses[,2], c(1.97,2.03),
       means[,2]+ses[,2], length=0.05, angle=90, code=3)
segments(0.97, means[1,1], 1.97, means[1,2], lty=2)
segments(1.03, means[2,1], 2.03, means[2,2])
points(c(0.97, 1.03), means[,1], pch=c(21,16), bg="white")
points(c(1.97, 2.03), means[,2], pch=c(21, 16), bg="white")
legend("topleft", c("Sex", "Female", "Male"),
       bty="n", pch=c(NA,21,16))

m = lm(mean_horns_lenght~sex*density)
anova(m)
summary(m)
colMeans(means)
rowMeans(means)

m2 = lm(mean_horns_lenght~sex*density - sex:density)
anova(m2)
summary(m2)
colMeans(means)
rowMeans(means)



################################################################################
# SECOND 2-WAY ANOVA FOR BODY MASS
################################################################################

means2 = tapply(dat$mass, list(sex, density), mean)
ses2 = tapply(dat$mass,
             list(sex, density),
             function(x) sd(x)/sqrt(sum(!is.na(x))))
means2
ses2

plot(c(0.97, 1.03), means2[,1], ylim=c(18, 26), xlim=c(0.8, 2.2),
     xlab="Population Density at Birth",
     ylab="Body Mass [kg]",
     xaxt="n", las=1, pch=c(21,16), col="white")
axis(1, 1:2, labels=c("High", "Low"))
arrows(c(0.97,1.03), means2[,1]-ses2[,1], c(0.97,1.03),
       means2[,1]+ses2[,1], length=0.05, angle=90, code=3)
arrows(c(1.97,2.03), means2[,2]-ses2[,2], c(1.97,2.03),
       means2[,2]+ses2[,2], length=0.05, angle=90, code=3)
segments(0.97, means2[1,1], 1.97, means2[1,2], lty=2)
segments(1.03, means2[2,1], 2.03, means2[2,2])
points(c(0.97, 1.03), means2[,1], pch=c(21,16), bg="white")
points(c(1.97, 2.03), means2[,2], pch=c(21, 16), bg="white")
legend("topleft", c("Sex", "Female", "Male"),
       bty="n", pch=c(NA,21,16))

m3 = lm(dat$mass~sex*density)
anova(m3)
summary(m3)
colMeans(means2)
rowMeans(means2)

m4 = lm(dat$mass~sex*density - sex:density)
anova(m4)
summary(m4)
colMeans(means2)
rowMeans(means2)


################################################################################
# PUTTING PLOTS TOGETHER
################################################################################

par(mfrow=c(1,2))
par(bg = "ivory")

plot(c(0.97, 1.03), means[,1], ylim=c(160, 200), xlim=c(0.8, 2.2),
     xlab="Population Density at Birth",
     ylab="Horns Length (Left-Right Mean) [mm]",
     xaxt="n", las=1, pch=c(21,16), col="white")
axis(1, 1:2, labels=c("High", "Low"))
arrows(c(0.97,1.03), means[,1]-ses[,1], c(0.97,1.03),
       means[,1]+ses[,1], length=0.05, angle=90, code=3)
arrows(c(1.97,2.03), means[,2]-ses[,2], c(1.97,2.03),
       means[,2]+ses[,2], length=0.05, angle=90, code=3)
segments(0.97, means[1,1], 1.97, means[1,2], lty=2)
segments(1.03, means[2,1], 2.03, means[2,2])
points(c(0.97, 1.03), means[,1], pch=c(21,16), bg="white")
points(c(1.97, 2.03), means[,2], pch=c(21, 16), bg="white")
legend("topleft", c("Sex", "Female", "Male"),
       bty="n", pch=c(NA,21,16))

plot(c(0.97, 1.03), means2[,1], ylim=c(18, 26), xlim=c(0.8, 2.2),
     xlab="Population Density at Birth",
     ylab="Body Mass [kg]",
     xaxt="n", las=1, pch=c(21,16), col="white")
axis(1, 1:2, labels=c("High", "Low"))
arrows(c(0.97,1.03), means2[,1]-ses2[,1], c(0.97,1.03),
       means2[,1]+ses2[,1], length=0.05, angle=90, code=3)
arrows(c(1.97,2.03), means2[,2]-ses2[,2], c(1.97,2.03),
       means2[,2]+ses2[,2], length=0.05, angle=90, code=3)
segments(0.97, means2[1,1], 1.97, means2[1,2], lty=2)
segments(1.03, means2[2,1], 2.03, means2[2,2])
points(c(0.97, 1.03), means2[,1], pch=c(21,16), bg="white")
points(c(1.97, 2.03), means2[,2], pch=c(21, 16), bg="white")
legend("topleft", c("Sex", "Female", "Male"),
       bty="n", pch=c(NA,21,16))

par(oldpar)
