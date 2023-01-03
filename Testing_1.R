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

dat = read.csv("exam2022_part1.csv")
head(dat)
names(dat)
View(dat)
str(dat)
dat = na.omit(dat)

############################
# creation of sub-datasets
############################

dat_wet = dat[dat$treat=="W",]
dat_dry = dat[dat$treat=="D",]
dat_S = dat[dat$sp=="S",]
dat_L = dat[dat$sp=="L",]

View(dat_wet)
View(dat_dry)
View(dat_S)
View(dat_L)
################################################################################
# VARIABLES ORGANIZING AND CHECKING PLUS HISTOGRAMS 
################################################################################

populations = as.factor(dat$pop)
species = as.factor(dat$sp)
treatment = as.factor(dat$treat)

################################################################
# checking LBL and UBL with histograms for each dataset
################################################################

########
# dat
########

par(mfrow=c(1,2))
par(bg = "ivory")

plotNormalHistogram(dat$LBL, prob = FALSE, col="slategray2", border="slategray",
                    main = "LBL Values with Normal Distribution Overlay", xlab = "LBL [mm]",
                    linecol="red", lwd=2 )


plotNormalHistogram(dat$UBL, prob = FALSE, col="slategray2", border="slategray",
                    main = "UBL Values with Normal Distribution Overlay", xlab = "UBL [mm]",
                    linecol="red", lwd=2 )

par(oldpar)


fitdistr(dat$LBL, "normal")


fitdistr(dat$UBL, "normal")

########
# dat_wet
########

par(mfrow=c(1,2))
par(bg = "ivory")

plotNormalHistogram(dat_wet$LBL, prob = FALSE, col="slategray2", border="slategray",
                    main = "LBL Values with Normal Distribution Overlay", xlab = "LBL [mm]",
                    linecol="red", lwd=2 )


plotNormalHistogram(dat_wet$UBL, prob = FALSE, col="slategray2", border="slategray",
                    main = "UBL Values with Normal Distribution Overlay", xlab = "UBL [mm]",
                    linecol="red", lwd=2 )

par(oldpar)


fitdistr(dat_wet$LBL, "normal")


fitdistr(dat_wet$UBL, "normal")


########
# dat_dry
########

par(mfrow=c(1,2))
par(bg = "ivory")

plotNormalHistogram(dat_dry$LBL, prob = FALSE, col="slategray2", border="slategray",
                    main = "LBL Values with Normal Distribution Overlay", xlab = "LBL [mm]",
                    linecol="red", lwd=2 )


plotNormalHistogram(dat_dry$UBL, prob = FALSE, col="slategray2", border="slategray",
                    main = "UBL Values with Normal Distribution Overlay", xlab = "UBL [mm]",
                    linecol="red", lwd=2 )

par(oldpar)


fitdistr(dat_dry$LBL, "normal")


fitdistr(dat_dry$UBL, "normal")

########
# dat_S
########

par(mfrow=c(1,2))
par(bg = "ivory")

plotNormalHistogram(dat_S$LBL, prob = FALSE, col="slategray2", border="slategray",
                    main = "LBL Values with Normal Distribution Overlay", xlab = "LBL [mm]",
                    linecol="red", lwd=2 )


plotNormalHistogram(dat_S$UBL, prob = FALSE, col="slategray2", border="slategray",
                    main = "UBL Values with Normal Distribution Overlay", xlab = "UBL [mm]",
                    linecol="red", lwd=2 )

par(oldpar)


fitdistr(dat_S$LBL, "normal")


fitdistr(dat_S$UBL, "normal")

########
# dat_L
########

par(mfrow=c(1,2))
par(bg = "ivory")

plotNormalHistogram(dat_L$LBL, prob = FALSE, col="slategray2", border="slategray",
                    main = "LBL Values with Normal Distribution Overlay", xlab = "LBL [mm]",
                    linecol="red", lwd=2 )


plotNormalHistogram(dat_L$UBL, prob = FALSE, col="slategray2", border="slategray",
                    main = "UBL Values with Normal Distribution Overlay", xlab = "UBL [mm]",
                    linecol="red", lwd=2 )

par(oldpar)


fitdistr(dat_L$LBL, "normal")


fitdistr(dat_L$UBL, "normal")

##############################################################################
##############################################################################
#
# Doing boxplots to visualize data of UBL and LBL
#
##############################################################################
##############################################################################

########
# dat
########

########
# first LBL
########

par(mfrow=c(1,2))
par(bg = "ivory")
boxplot(dat$LBL~species, xlab="Species", ylim = c(9, 35),
        ylab="LBL [mm]",boxwex=0.35, main = "Boxplot of LBL data for species", lwd = 2, border= "slategrey", # colour of the box borders
        col = "slategray2", # colour of the inside of the boxes
        col.axis = 'grey20', # colour of the axis numbers 
        col.lab = 'grey20', # colour of the axis labels
        frame = F)
stripchart(dat$LBL~species,
           method = "jitter",  main = "method = 'jitter', jitter = 0.2",
           pch = 16, # specify the type of point to use
           cex = 1,
           col = c(3,2),
           vertical = TRUE, 
           at = c(0.6,1.6),
           add = TRUE)
# at = c(0.75,1.75)

boxplot(dat$LBL~treatment, xlab="Treatments", ylim = c(9, 35),
        ylab="LBL [mm]",boxwex=0.35, main = "Boxplot of LBL data for treatments", lwd = 2, border= "slategrey", # colour of the box borders
        col = "slategray2", # colour of the inside of the boxes
        col.axis = 'grey20', # colour of the axis numbers 
        col.lab = 'grey20', # colour of the axis labels
        frame = F)
stripchart(dat$LBL~treatment,
           method = "jitter",  main = "method = 'jitter', jitter = 0.2",
           pch = 16, # specify the type of point to use
           cex = 1,
           col = c(7,4),
           vertical = TRUE, 
           at = c(0.6,1.6),
           add = TRUE)

par(oldpar)

########
# now UBL
########


par(mfrow=c(1,2))
par(bg = "ivory")
boxplot(dat$UBL~species, xlab="Species", ylim = c(8, 30),
        ylab="UBL [mm]",boxwex=0.35, main = "Boxplot of UBL data for species", lwd = 2, border= "slategrey", # colour of the box borders
        col = "slategray2", # colour of the inside of the boxes
        col.axis = 'grey20', # colour of the axis numbers 
        col.lab = 'grey20', # colour of the axis labels
        frame = F)
stripchart(dat$UBL~species,
           method = "jitter",  main = "method = 'jitter', jitter = 0.2",
           pch = 16, # specify the type of point to use
           cex = 1,
           col = c(3,2),
           vertical = TRUE, 
           at = c(0.6,1.6),
           add = TRUE)
# at = c(0.75,1.75)

boxplot(dat$UBL~treatment, xlab="Treatments", ylim = c(8, 30),
        ylab="UBL [mm]",boxwex=0.35, main = "Boxplot of UBL data for treatments", lwd = 2, border= "slategrey", # colour of the box borders
        col = "slategray2", # colour of the inside of the boxes
        col.axis = 'grey20', # colour of the axis numbers 
        col.lab = 'grey20', # colour of the axis labels
        frame = F)
stripchart(dat$UBL~treatment,
           method = "jitter",  main = "method = 'jitter', jitter = 0.2",
           pch = 16, # specify the type of point to use
           cex = 1,
           col = c(7,4),
           vertical = TRUE, 
           at = c(0.6,1.6),
           add = TRUE)

par(oldpar)

##############################################################################
##############################################################################
#
# end of boxplots to visualize data
#
##############################################################################
##############################################################################



################################################################################
# 2-WAY ANOVA FOR HORNS LENGTH For UBL and LBL with factors treatment and species
################################################################################


################################################################################
# STARTING THE FIRST 2-WAY ANOVA FOR LBL
################################################################################

means = tapply(dat$LBL, list(species, treatment), mean)
ses = tapply(dat$LBL,
             list(species, treatment),
             function(x) sd(x)/sqrt(sum(!is.na(x))))
means
ses

plot(c(0.97, 1.03), means[,1], ylim=c(15, 25), xlim=c(0.8, 2.2),
     xlab="Treatment",
     ylab="Lower bract Length (LBL) [mm]",
     xaxt="n", las=1, pch=c(21,16), col="white")
axis(1, 1:2, labels=c("Dry", "Wet"))
arrows(c(0.97,1.03), means[,1]-ses[,1], c(0.97,1.03),
       means[,1]+ses[,1], length=0.05, angle=90, code=3)
arrows(c(1.97,2.03), means[,2]-ses[,2], c(1.97,2.03),
       means[,2]+ses[,2], length=0.05, angle=90, code=3)
segments(0.97, means[1,1], 1.97, means[1,2], lty=2)
segments(1.03, means[2,1], 2.03, means[2,2])
points(c(0.97, 1.03), means[,1], pch=c(21,16), bg="white")
points(c(1.97, 2.03), means[,2], pch=c(21, 16), bg="white")
legend("topleft", c("Species", "L", "S"),
       bty="n", pch=c(NA,21,16))

m = lm(dat$LBL~species*treatment)
anova(m)
summary(m)
colMeans(means)
rowMeans(means)

m2 = lm(dat$LBL~species*treatment-species:treatment)
anova(m2)
summary(m2)




################################################################################
# SECOND 2-WAY ANOVA FOR UBL
################################################################################

means2 = tapply(dat$UBL, list(species, treatment), mean)
ses2 = tapply(dat$UBL,
             list(species, treatment),
             function(x) sd(x)/sqrt(sum(!is.na(x))))
means2
ses2

plot(c(0.97, 1.03), means2[,1], ylim=c(14, 22), xlim=c(0.8, 2.2),
     xlab="Treatment",
     ylab="Upper bract Length (UBL) [mm]",
     xaxt="n", las=1, pch=c(21,16), col="white")
axis(1, 1:2, labels=c("Dry", "Wet"))
arrows(c(0.97,1.03), means2[,1]-ses2[,1], c(0.97,1.03),
       means2[,1]+ses2[,1], length=0.05, angle=90, code=3)
arrows(c(1.97,2.03), means2[,2]-ses2[,2], c(1.97,2.03),
       means2[,2]+ses2[,2], length=0.05, angle=90, code=3)
segments(0.97, means2[1,1], 1.97, means2[1,2], lty=2)
segments(1.03, means2[2,1], 2.03, means2[2,2])
points(c(0.97, 1.03), means2[,1], pch=c(21,16), bg="white")
points(c(1.97, 2.03), means2[,2], pch=c(21, 16), bg="white")
legend("topleft", c("Species", "L", "S"),
       bty="n", pch=c(NA,21,16))

m3 = lm(dat$UBL~species*treatment)
anova(m3)
summary(m3)
colMeans(means2)
rowMeans(means2)

m4 = lm(dat$UBL~species*treatment-species:treatment)
anova(m4)
summary(m4)


################################################################################
# PUTTING PLOTS TOGETHER
################################################################################

par(mfrow=c(1,2))
par(bg = "ivory")

plot(c(0.97, 1.03), means[,1], ylim=c(14, 25), xlim=c(0.8, 2.2),
     xlab="Treatment",
     ylab="Lower bract Length (LBL) [mm]",
     xaxt="n", las=1, pch=c(21,16), col="white")
axis(1, 1:2, labels=c("Dry", "Wet"))
arrows(c(0.97,1.03), means[,1]-ses[,1], c(0.97,1.03),
       means[,1]+ses[,1], length=0.05, angle=90, code=3)
arrows(c(1.97,2.03), means[,2]-ses[,2], c(1.97,2.03),
       means[,2]+ses[,2], length=0.05, angle=90, code=3)
segments(0.97, means[1,1], 1.97, means[1,2], lty=2)
segments(1.03, means[2,1], 2.03, means[2,2])
points(c(0.97, 1.03), means[,1], pch=c(21,16), bg="white")
points(c(1.97, 2.03), means[,2], pch=c(21, 16), bg="white")
legend("topleft", c("Species", "L", "S"),
       bty="n", pch=c(NA,21,16))

plot(c(0.97, 1.03), means2[,1], ylim=c(14, 25), xlim=c(0.8, 2.2),
     xlab="Treatment",
     ylab="Upper bract Length (UBL) [mm]",
     xaxt="n", las=1, pch=c(21,16), col="white")
axis(1, 1:2, labels=c("Dry", "Wet"))
arrows(c(0.97,1.03), means2[,1]-ses2[,1], c(0.97,1.03),
       means2[,1]+ses2[,1], length=0.05, angle=90, code=3)
arrows(c(1.97,2.03), means2[,2]-ses2[,2], c(1.97,2.03),
       means2[,2]+ses2[,2], length=0.05, angle=90, code=3)
segments(0.97, means2[1,1], 1.97, means2[1,2], lty=2)
segments(1.03, means2[2,1], 2.03, means2[2,2])
points(c(0.97, 1.03), means2[,1], pch=c(21,16), bg="white")
points(c(1.97, 2.03), means2[,2], pch=c(21, 16), bg="white")
legend("topleft", c("Species", "L", "S"),
       bty="n", pch=c(NA,21,16))


par(oldpar)
