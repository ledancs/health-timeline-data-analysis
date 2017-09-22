setwd("C:/Users/Land/Documents/health-timeline-data-analysis")
dataFrame = read.csv(file="data.csv", head=TRUE, sep=",")

# remove zero value insights
tmp <- subset(dataFrame, value > 0)

# INSIGHT DISTRIBUTION

# histogram with both distributions by vlaue
tableInsights <- subset(tmp, visualization == "table")
timelineInsights <- subset(tmp, visualization == "timeline")

hist(tableInsights$value, breaks=seq(0, 6, by=0.25), col="lightblue", xlab="Value", main="", ylim = c(0, 200), right = TRUE)
hist(timelineInsights$value, breaks=seq(0, 6, by=0.25), col="lightsalmon", xlab="Value", main="", ylim = c(0, 200), right = FALSE, add = TRUE)
legend(locator(1), c("Table", "Timeline"), fill=c("lightblue", "lightsalmon"), bty ="n")

# METRICS PER ASSESSMENT

# number of insights per assessment
tmp <-aggregate(value~experiment+visualization, dataFrame, FUN=function(z){ length(z) })
tmp <- setNames(tmp, c("experiment", "visualization", "total")) # remane the columns
boxplot(total~visualization,data=tmp, notch=TRUE,
        horizontal=TRUE,
        col=(c("lightblue","lightsalmon")),
        xlab="Number of Insights")

# mean value of insights per assessment
tmp <- aggregate(value ~ experiment+visualization, dataFrame, FUN = function(z){ mean(z) } )
tmp <- setNames(tmp, c("experiment", "visualization", "meanValue")) # remane the columns
boxplot(meanValue~visualization,data=tmp, notch=TRUE,
        horizontal=TRUE,
        col=(c("lightblue","lightsalmon")),
        xlab="Mean Value")

# cumulative value of insights per assessment
tmp <- aggregate(value ~ experiment+visualization, dataFrame, FUN = function(z){ sum(z) } )
tmp <- setNames(tmp, c("experiment", "visualization", "cumulativeValue")) # remane the columns
boxplot(cumulativeValue~visualization,data=tmp, notch=TRUE,
        horizontal=TRUE,
        col=(c("lightblue","lightsalmon")),
        xlab="Cumulative Value")


# TIME TO FIRST INSIGHT

valueToCheck <- 0
# par(mfrow=c(1,4))
for(valueToCheck in c( 0, 2, 3, 4 ) ){
  base <- aggregate(value ~ case+visualization, dataFrame, FUN = function(z){ length( z[ z > valueToCheck ] ) } )
  base <- setNames( base, c("case", "visualization", "count") )
  tmp <- aggregate(time ~ case+visualization, subset( dataFrame, value > valueToCheck ), FUN = min )
  tmp <- merge(base, tmp, by = c("case", "visualization"), all.x = TRUE)
  tmp <- na.omit(tmp)
  # write.csv(tmp[c('visualization', 'time')], file = paste(valueToCheck, "time.csv"))
  # boxplots 
  boxplot(time~visualization,data=tmp, notch=FALSE,
          horizontal=TRUE,
          col=(c("lightblue","lightsalmon")),
          main= paste("Value of at least", valueToCheck + 1),
          xlab="Time (s)")
}