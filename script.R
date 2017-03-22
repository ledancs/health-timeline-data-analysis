setwd("C:/Users/Land/Documents/health-timeline-data-analysis")
dataFrame = read.csv(file="data.csv", head=TRUE, sep=",")
# dataFrame <- subset(dataFrame, value > 0) # remove zero value insights
# write.csv(dataFrame, file = "healthTimelineDataRichNonZero.csv")
summary(dataFrame)

# Mann-Whitney U test
# wilcox.test(value ~ useTimeline, data = dataFrame)

# distribution of the value of the insights
# hist(dataFrame$value)

# Another way to count how many are timeline and how many are table
# nrow(subset(dataFrame, visualization=="timeline"))
# nrow(subset(dataFrame, visualization=="table"))

# total number of insights
table(dataFrame$visualization)

tmp <- subset(dataFrame, value > 0) # remove zero value insights
table(tmp$visualization)

# before we continue we explain the concept of experiment
# we have 10 assessments and 5 participants
# the insights produced by one participant durin an assessment are collected into an experiment
# thus we have a total of 50 experiments
# 25 with timeline and 25 with the table

# distribution of value of all the insights by visualization
tbl <- table(dataFrame$value, dataFrame$visualization)
# chi squared statistical test
chisq.test(tbl)
chisq.test(tbl, simulate.p.value=TRUE)
# p-value = 2.01e-10
# chisq.test(tbl, simulate.p.value=TRUE, B=1e7)$p.value
# fisher.test(tbl) # fails
fisher.test(tbl, simulate.p.value = TRUE) # works

# statisitcal test for all the insights per value
# tmp <- aggregate(insight ~ value+visualization, dataFrame, FUN=length)
# fisher.test(tmp$visualization, tmp$insight)
# fisher.test(tmp$visualization, tmp$insight, simulate.p.value=TRUE)
# chisq.test(tmp$visualization, tmp$insight)

# use the assessment pairs
tbl <- table(dataFrame$pair, dataFrame$visualization)
chisq.test(tbl)

tmp <- subset(dataFrame, value > 2)
tbl <- table(tmp$pair, tmp$visualization)
chisq.test(tbl)
fisher.test(tbl)
fisher.test(tbl, simulate.p.value = TRUE) # works

# to calculate the median
# median of the table
# median(subset(dataFrame, visualization == "table")$value)
# median of the timeline
# median(subset(dataFrame, visualization == "timeline")$value)

minMaxMeanMedianStdFun <- function(x) {
  c(min = min(x), max = max(x), 
    mean = mean(x), median = median(x), 
    std = sd(x))
}

tapply(dataFrame$value, dataFrame$visualization, minMaxMeanMedianStdFun)

# Chi squared test
# chisq.test(table(dataFrame$visualization))

# non-zero insights
table(subset(dataFrame, value > 0)$visualization)

xtabs(value~useTimeline, dataFrame) # total value of the insights for each visualization

# total number of insights by value per visualization
tbl <- with(dataFrame, tapply(visualization, list(visualization, value), FUN=length))
tbl[is.na(tbl)] <- 0

# distribution of the timeline values
hist(dataFrame[dataFrame$visualization=="timeline", "value"])
# mean of the value for timeline
mean(dataFrame[dataFrame$visualization=="timeline", "value"])
# standard deviation
sd(dataFrame[dataFrame$visualization=="timeline", "value"])

# distribution of the table values
hist(dataFrame[dataFrame$visualization=="table", "value"])
# mean of the value for table
mean(dataFrame[dataFrame$visualization=="table", "value"])
# standard deviation
sd(dataFrame[dataFrame$visualization=="table", "value"])

# subset that has insights at least value 3 from table
tableValue3OrMore <- dataFrame[ which( dataFrame$value > 2 & dataFrame$visualization == "table"), ]
# time to insight value 3 or more for table
timeToInsight3Table <- aggregate(time ~ assessment+participant, tableValue3OrMore, FUN=min)
# mean of time to first insight value 3 or higher
mean(timeToInsight3Table$time)
# standard deviation
sd(timeToInsight3Table$time)

# subset that has insights at least value 3 from timeline
timelineValue3OrMore <- dataFrame[ which( dataFrame$value > 2 & dataFrame$visualization == "timeline"), ]
# time to insight value 3 or more for table
timeToInsight3Timeline <- aggregate(time ~ assessment+participant, timelineValue3OrMore, FUN=min)
# mean of time to first insight value 3 or higher
mean(timeToInsight3Timeline$time)
# standard deviation
sd(timeToInsight3Timeline$time)

# some statistics

# total number of insights per assessment per participant
# here we see participant 1 assessment 1 had n number of insights
aggregate(insight ~ assessment+participant, dataFrame, FUN=length)
# the same but with visualization
aggregate(insight ~ assessment+participant+visualization, dataFrame, FUN=length)

# plots

# get the values into a data frame
tableCumulative <- dataFrame[dataFrame$visualization=="table", c("time", "cumulative")]
timelineCumulative <- dataFrame[dataFrame$visualization=="timeline", c("time", "cumulative")]

# Table cumulative value plot
# filled circles are pch = 16 
# for now I guess we can use pch = 1 to make the density look more accurate

cumulativeMean <- aggregate(cumulative ~ time, tableCumulative, FUN=mean)

plot(
  cumulativeMean, 
  col = "red", pch = 1, 
  ylim = c(1, 75), xlim = c(0, 200),
  xlab = "Time (seconds)", ylab = "Cumulative value", main = "Table cumulative alue over time")


# Timeline cumulative value plot

plot(
  timelineCumulative[c("time", "cumulative")], 
  col = "blue", pch = 1, 
  ylim = c(1, 75), xlim = c(0, 200),
  xlab = "Time (seconds)", ylab = "Cumulative value", main = "Timeline cumulative alue over time")

# get the mean of each moment
cumulativeMean <- aggregate(cumulative ~ time, timelineCumulative, FUN=mean)

plot(
  cumulativeMean, 
  col = "blue", pch = 1, 
  ylim = c(1, 75), xlim = c(0, 200),
  xlab = "Time (seconds)", ylab = "Cumulative value", main = "Timeline cumulative alue over time")


#fit first degree polynomial equation:

x <- cumulativeMean$time
y <- cumulativeMean$cumulative
fit  <- lm(y~x)
#second degree
fit2 <- lm(y~poly(x,2,raw=TRUE))
#third degree
fit3 <- lm(y~poly(x,3,raw=TRUE))
#fourth degree
fit4 <- lm(y~poly(x,4,raw=TRUE))

# plot(x,y,pch=19,ylim=c(0,150))
lines(xx, predict(fit, data.frame(x=xx)), col="red")
lines(xx, predict(fit2, data.frame(x=xx)), col="green")
lines(xx, predict(fit3, data.frame(x=xx)), col="blue")
lines(xx, predict(fit4, data.frame(x=xx)), col="purple")

# fit with exponential
exponential.model <- lm(log(cumulativeMean$cumulative)~ cumulativeMean$time)
timevalues <- seq(0, 200, length=length(cumulativeMean$time))
Counts.exponential2 <- exp(predict(exponential.model,list(Time=timevalues)))
lines(timevalues, Counts.exponential2,lwd=2, col = "gold")

# plot the fitting curve
#generate range of 50 numbers starting from 30 and ending at 160
xx <- seq(0,200, length=length(cumulativeMean$time))
lines(xx, predict(fit, data.frame(x=xx)), col="black")


# histograms

h1 <- dataFrame[dataFrame$visualization=="table", "value"]
h2 <- dataFrame[dataFrame$visualization=="timeline", "value"]

hist(
  h1, 
  breaks=rep(0:5,each=2)+c(-.15,.15),
  col=rgb(1,0,0,0.5),
  xlim=c(0,5), 
  # ylim=c(0,150), 
  main="Overlapping Histogram", xlab="Variable"
)

hist(
  h2, 
  breaks=rep(0:5,each=2)+c(-.15,.15),
  col=rgb(0,0,1,0.5), add=T)
box()

# box plots
boxplot(
  time~visualization, 
  # data = subset(dataFrame, value > 2), 
  # data = dataFrame, 
  data = timeToFirstInsight,
  col=(c("darkgreen","gold")),
  notch=FALSE,
  main = "Time to first insight", xlab = "Visualization", ylab="Value"
  # main = "Insight Value 3 or Higher", xlab = "Visualization", ylab="Value"
  # ylim=c(0,5)
)

#########################
# TIME TO FIRST INSIGHT #
#########################

# TIME TO FIRST INSIGHT
# statistical tests for time to first insight
# get the first insight ( of at least value 1 ) for each assessment per participant
tmp <- aggregate(time ~ experiment+visualization, subset(dataFrame, value > 0), FUN=min)
wilcox.test(time~visualization, data = tmp)
kruskal.test(time~visualization, data = tmp)

tmp <- aggregate(time ~ experiment+visualization, subset(dataFrame, value > 1), FUN=min)
wilcox.test(time~visualization, data = tmp)
kruskal.test(time~visualization, data = tmp)

tmp <- aggregate(time ~ experiment+visualization, subset(dataFrame, value > 2), FUN=min)
wilcox.test(time~visualization, data = tmp)
kruskal.test(time~visualization, data = tmp)

tmp <- aggregate(time ~ experiment+visualization, subset(dataFrame, value > 3), FUN=min)
wilcox.test(time~visualization, data = tmp)
kruskal.test(time~visualization, data = tmp)

tmp <- aggregate(time ~ experiment+visualization, subset(dataFrame, value > 4), FUN=min)
wilcox.test(time~visualization, data = tmp)
kruskal.test(time~visualization, data = tmp)

# Fisher and chi-squared are not suitable for numerical data.
fisher.test(tmp$visualization, tmp$time)
fisher.test(tmp$visualization, tmp$time, simulate.p.value=TRUE)
# fisher.test(tmp$visualization, tmp$time, simulate.p.value=TRUE, B=1e7)
chisq.test(tmp$visualization, tmp$time, simulate.p.value = TRUE)
# chisq.test(tmp$visualization, tmp$time, simulate.p.value = TRUE, B=1e7)
chisq.test(tmp$visualization, tmp$time)


# We can try to also use the Na or zero observations of the time to first insight ( value 1 to 5 )
# in this example we check insights value at least 2
# tmp <- aggregate(time ~ experiment+visualization, subset(dataFrame, value > 4), FUN=min)
for(valueToCheck in c( 0, 1, 2, 3, 4 ) ){
  print( paste( "time to first insight with value >", valueToCheck ) )
  base <- aggregate(value ~ case+visualization, dataFrame, FUN = function(z){ length( z[ z > valueToCheck ] ) } )
  base <- setNames( base, c("case", "visualization", "count") )
  tmp <- aggregate(time ~ case+visualization, subset( dataFrame, value > valueToCheck ), FUN = min )
  tmp <- merge(base, tmp, by = c("case", "visualization"), all.x = TRUE)
  tmp[is.na(tmp)] <- 191 # the three-minute limit we had per assessment
  p <- kruskal.test(time~visualization, data = tmp)$p.value
  print( paste("Kruskal ", p) )
  p <- wilcox.test(time~visualization, data = tmp, p.value=TRUE)$p.value
  print( paste("wilcox ", p) )
}

# valueToCheck <- 0
# base <- aggregate(value ~ case+visualization, dataFrame, FUN = function(z){ length( z[ z > valueToCheck ] ) } )
# base <- setNames( base, c("case", "visualization", "count") )
# tmp <- aggregate(time ~ case+visualization, subset( dataFrame, value > valueToCheck ), FUN = min )
# tmp <- merge(base, tmp, by = c("case", "visualization"), all.x = TRUE)
# now test if there is a statistical difference in the p-values
# wilcox.test(time~visualization, data = tmp)
# kruskal.test(time~visualization, data = tmp)
# if we replace the NA values for the 600 assuming to represent that the participant never reached an insight of that level

# test agian with kruskal
# kruskal.test(time~visualization, data = tmp)





#####################################
# INISIGHTS BY VALUE PER ASSESSMENT #
#####################################
# Now we calculate the statistical significance of the number of insights ( value 1 to 5 ) per assessment

# to get the zero counts of insights of a given value use the next instruction
# aggregate(value~experiment+visualization, dataFrame, FUN=function(z){ length(z[z > 2]) })
# to see all the insight values per experiment
aggregate(value~experiment, dataFrame, FUN=function(z){ z }) # z refers to the list of values grouped by experiment

tmp <- aggregate(value~case+visualization, dataFrame, FUN=function(z){ z })

# assign to the tmp data frame the total number of insights value at least valueToCheck
for(valueToCheck in c(0, 1, 2, 3, 4, 5)){
  print( paste("total number of insights value", valueToCheck) )
  tmp <- aggregate(value~case+visualization, dataFrame, FUN=function(z){ length(z[z == valueToCheck]) })
  tmp <- setNames(tmp, c("case", "visualization", "count"))
  p <- kruskal.test(count~visualization, data = tmp)$p.value
  print( paste("Kruskal", p) )
  p <- wilcox.test(count~visualization, data = tmp)$p.value
  print( paste("Wilcox", p) )
}
# valueToCheck <- 4

# makes no sense because "case" is from 1 to 25 and describes the case used nothing else
# wilcox.test(case~visualization, data = dataFrame, p.value=TRUE) 
# use tmp and select count which contains the number of insights per case
# wilcox.test(count~visualization, data = tmp)$p.value
# wilcox.test(case~visualization, data = subset(dataFrame, value > 4), p.value=TRUE)
kruskal.test(count~visualization, data = tmp)$p.value

# Experiment
# ggplot(dataFrame, aes(x=case, fill=visualization)) + geom_density(alpha=.3) + scale_x_continuous(limits = c(-20, 50))
# ggplot(subset(dataFrame, value >3), aes(x=case, fill=visualization)) + geom_density(alpha=.3) + scale_x_continuous(limits = c(-20, 50))
# ggplot(subset(dataFrame, value >0), aes(x=time, fill=visualization)) + geom_density(alpha=.3) + scale_x_continuous(limits = c(-50, 250))
ggplot(dataFrame, aes(x=case, fill=visualization)) + geom_density(alpha=.3) + xlim(-20, 50)
ggplot(subset(dataFrame, value > 0), aes(x=case, fill=visualization)) + geom_density(alpha=.3) + xlim(-20, 50)
ggplot(subset(dataFrame, value > 1), aes(x=case, fill=visualization)) + geom_density(alpha=.3) + xlim(-20, 50)
ggplot(subset(dataFrame, value > 2), aes(x=case, fill=visualization)) + geom_density(alpha=.3) + xlim(-20, 50)
ggplot(subset(dataFrame, value > 3), aes(x=case, fill=visualization)) + geom_density(alpha=.3) + xlim(-20, 50)
ggplot(subset(dataFrame, value > 4), aes(x=case, fill=visualization)) + geom_density(alpha=.3) + xlim(-20, 50)

# use the zero values from assessments/experiments that had no insight of a given value ( 1 to 5 )
tmp <-aggregate(value~experiment+visualization, dataFrame, FUN=function(z){ length(z[z == 5]) })
wilcox.test(value~visualization, data = tmp, p.value=TRUE)
kruskal.test(value~visualization, data = tmp)

# Total number of insights per assessment
# count insights per assessment
tmp <- aggregate(insight ~ experiment+visualization, dataFrame, FUN=length)
wilcox.test(insight~visualization, data = tmp)
kruskal.test(insight~visualization, data = tmp)
tapply(tmp$insight, tmp$visualization, minMaxMeanMedianStdFun) # get the average and SD

tmp <- aggregate(insight ~ assessment+participant+visualization, subset(dataFrame, value == 1), FUN=length)
wilcox.test(insight~visualization, data = tmp)
kruskal.test(insight~visualization, data = tmp)

tmp <- aggregate(insight ~ assessment+participant+visualization, subset(dataFrame, value == 2), FUN=length)
wilcox.test(insight~visualization, data = tmp)
kruskal.test(insight~visualization, data = tmp)

tmp <- aggregate(insight ~ assessment+participant+visualization, subset(dataFrame, value == 3), FUN=length)
wilcox.test(insight~visualization, data = tmp)
kruskal.test(insight~visualization, data = tmp)

tmp <- aggregate(insight ~ assessment+participant+visualization, subset(dataFrame, value == 4), FUN=length)
wilcox.test(insight~visualization, data = tmp)
kruskal.test(insight~visualization, data = tmp)

tmp <- aggregate(insight ~ assessment+participant+visualization, subset(dataFrame, value == 5), FUN=length)
wilcox.test(insight~visualization, data = tmp)
kruskal.test(insight~visualization, data = tmp)

# count the number of hypothesis
# A hypothesis is an insight value 4 or 5
valueToCheck <- 3
# assessments that do not have a value 4 or 5 will get a 0 
# this means no observations occurred in the assessment of this insights
tmp <- aggregate(value ~ experiment+visualization, dataFrame, FUN = function(z){ length( z[ z > valueToCheck ] ) } )
tmp <- setNames(tmp, c("experiment", "visualization", "insight")) # remane the columns
wilcox.test(insight~visualization, data = tmp)
kruskal.test(insight~visualization, data = tmp)
tapply(tmp$insight, tmp$visualization, minMaxMeanMedianStdFun) # get the average and SD

# The distribution of the value per assessment
# we have 25 assessments with timeline and another 25 with the table

# if we use the x-axis as the average insight-value
tmp <- aggregate(value ~ experiment+visualization, dataFrame, FUN = function(z){ mean(z) } )
tmp <- setNames(tmp, c("experiment", "visualization", "meanValue")) # remane the columns

test <- wilcox.test(meanValue~visualization, data = tmp)
p <- test$p.value
print( paste( "Mean value per assessment ", paste("Wilcoxon Mann Whitney ", p) ) )
test <- kruskal.test(meanValue~visualization, data = tmp)
p <- test$p.value
print( paste( "Mean value per assessment ", paste("Kruskal Wallis ", p) ) )
tapply(tmp$meanValue, tmp$visualization, minMaxMeanMedianStdFun) # get the average and SD

# try some boxplots
# dataToPlot <- data.frame(
#   'timeline' = split(tmp, tmp$visualization)$timeline$meanValue,
#   'table' = split(tmp, tmp$visualization)$table$meanValue
# )
# boxplot(dataToPlot)

plot <- ggplot(tmp, aes(x=visualization, y=meanValue)) + geom_boxplot()
plot + geom_boxplot()
plot + geom_boxplot() + geom_jitter()
plot + geom_boxplot() + coord_flip() + geom_jitter()
plot + geom_boxplot(aes(fill = factor(visualization)))

# Recommended
plot <- ggplot(tmp, aes(factor(visualization), meanValue)) 
plot + geom_boxplot(aes(fill = factor(visualization))) + coord_flip() + geom_jitter() + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
plot + geom_boxplot(aes(fill = factor(visualization))) + coord_flip() + scale_fill_brewer(palette="Accent")


# or the cummulative value
tmp <- aggregate(value ~ experiment+visualization, dataFrame, FUN = function(z){ sum(z) } )
tmp <- setNames(tmp, c("experiment", "visualization", "cumulativeValue")) # remane the columns
test <- wilcox.test(cumulativeValue~visualization, data = tmp)
p <- test$p.value
print( paste( "cumulative value per assessment ", paste("Wilcoxon Mann Whitney ", p) ) )
test <- kruskal.test(cumulativeValue~visualization, data = tmp)
p <- test$p.value
print( paste( "cumulative value per assessment ", paste("Kruskal Wallis ", p) ) )
tapply(tmp$cumulativeValue, tmp$visualization, minMaxMeanMedianStdFun) # get the average and SD

plot <- ggplot(tmp, aes(factor(visualization), cumulativeValue)) 
plot + geom_boxplot(aes(fill = factor(visualization))) + coord_flip() + geom_jitter() + scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
plot + geom_boxplot(aes(fill = factor(visualization))) + coord_flip() + scale_fill_brewer(palette="Accent")



###############################
# INSIGHT VALUE BY PATIENT ID #
###############################

# Extract the patient IDs
patientIDs <- unique(dataFrame$patient)

for(patientID in patientIDs ){
  dataOfPatient <- subset(dataFrame, dataFrame$patient == patientID) # get the subset for each patient ID
  tbl <- table(dataOfPatient$value, dataOfPatient$visualization) # extract the count of insights per value
  # chi squared with simulated data passes
  # chisq.test(tbl) # chi squared statistical test fails
  # fisher.test(tbl) # fails
  # fisher.test(tbl, simulate.p.value = TRUE) # works
  p <- chisq.test(tbl, simulate.p.value=TRUE)$p.value
  print( paste( paste( "Patient ID ", patientID ), paste("ChiSQ ", p) ) )
  p <- fisher.test(tbl, simulate.p.value = TRUE)$p.value
  print( paste("Fisher ", p) )
  stats <- tapply(dataOfPatient$value, dataOfPatient$visualization, minMaxMeanMedianStdFun) # get the average and SD
  print(stats)
}

