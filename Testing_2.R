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
# VARIABLES ADJUSTING AND CHECKING
################################################################################

sex = as.factor(dat$sex)
density = as.factor(dat$density)
#plot(dat)
horns_difference = dat$hornL - dat$hornR
hist(horns_difference)
mean_horns_lenght = (dat$hornL + dat$hornR)/2
hist(mean_horns_lenght)

################################################################################
# TOWARDS 2-WAY ANOVA: LOOKING AT BOXPLOTS
################################################################################

oldpar = par(no.readonly = TRUE)


par(mfrow=c(1,2))
par(bg = "ivory")
boxplot(mean_horns_lenght~sex, xlab="Sex", ylim = c(0, 300),
        ylab="Left-Right mean Horn Length [mm]",boxwex=0.35, main = "Boxplot of Horn length data for sexes", lwd = 2, border= "slategrey", # colour of the box borders
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
           add = TRUE)
# at = c(0.75,1.75)

boxplot(mean_horns_lenght~density, xlab="Pop. Density at birth", ylim = c(0, 300),
        ylab="Left-Right mean Horn Length [mm]",boxwex=0.35, main = "Boxplot of Horn length data vs Pop. density", lwd = 2, border= "slategrey", # colour of the box borders
        col = "slategray2", # colour of the inside of the boxes
        col.axis = 'grey20', # colour of the axis numbers 
        col.lab = 'grey20', # colour of the axis labels
        frame = F)
stripchart(mean_horns_lenght~density,
           method = "jitter",  main = "method = 'jitter', jitter = 0.2",
           pch = 16, # specify the type of point to use
           cex = 1,
           col = c(2,4),
           vertical = TRUE, 
           add = TRUE)
# at = c(0.75,1.75)

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
     xlab="Population Density at BIrth",
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

################################
# BOXPLOTS
################################

oldpar = par(no.readonly = TRUE)


par(mfrow=c(1,2))
par(bg = "ivory")
boxplot(mean_horns_lenght~sex, data = dat, xlab="Sex", ylim = c(0, 300),
        ylab="Left-Right mean Horn Length [mm]",boxwex=0.35, main = "Boxplot of Horn length data for sexes", lwd = 2, border= "slategrey", # colour of the box borders
        col = "slategray2", # colour of the inside of the boxes
        col.axis = 'grey20', # colour of the axis numbers 
        col.lab = 'grey20', # colour of the axis labels
        frame = F)
stripchart(mean_horns_lenght~sex,
           data = dat,
           method = "jitter",  main = "method = 'jitter', jitter = 0.2",
           pch = 16, # specify the type of point to use
           cex = 1,
           col = c(2,4),
           vertical = TRUE, 
           add = TRUE)
# at = c(0.75,1.75)

boxplot(mean_horns_lenght~density, data = dat, xlab="Pop. Density at birth", ylim = c(0, 300),
        ylab="Left-Right mean Horn Length [mm]",boxwex=0.35, main = "Boxplot of Horn length data vs Pop. density", lwd = 2, border= "slategrey", # colour of the box borders
        col = "slategray2", # colour of the inside of the boxes
        col.axis = 'grey20', # colour of the axis numbers 
        col.lab = 'grey20', # colour of the axis labels
        frame = F)
stripchart(mean_horns_lenght~density,
           data = dat,
           method = "jitter",  main = "method = 'jitter', jitter = 0.2",
           pch = 16, # specify the type of point to use
           cex = 1,
           col = c(2,4),
           vertical = TRUE, 
           add = TRUE)
# at = c(0.75,1.75)

par(oldpar)

################################
# 2-WAY ANOVA 
################################

means = tapply(mean_horns_lenght, list(sex, density), mean)
ses = tapply(mean_horns_lenght,
             list(sex, density),
             function(x) sd(x)/sqrt(sum(!is.na(x))))
means
ses

plot(c(0.97, 1.03), means[,1], ylim=c(160, 200), xlim=c(0.8, 2.2),
     xlab="Population Density at BIrth",
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
