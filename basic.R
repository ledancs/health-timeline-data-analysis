setwd("C:/Users/Land/Documents/health-timeline-data-analysis")
dataFrame = read.csv(file="data.csv", head=TRUE, sep=",")
# dataFrame <- subset(dataFrame, value > 0) # remove zero value insights
# write.csv(dataFrame, file = "healthTimelineDataRichNonZero.csv")
summary(dataFrame)

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

# some statistics

# total number of insights per assessment per participant
# here we see participant 1 assessment 1 had n number of insights
aggregate(insight ~ assessment+participant, dataFrame, FUN=length)
# the same but with visualization
aggregate(insight ~ assessment+participant+visualization, dataFrame, FUN=length)


# min, max, median, mean and std of each assessment
base <- aggregate(value ~ case+visualization, dataFrame, FUN = function(z){ z } )
base["max"] <- aggregate(value ~ case+visualization, dataFrame, max )$value
base["min"] <- aggregate(value ~ case+visualization, dataFrame, min )$value
base["median"] <- aggregate(value ~ case+visualization, dataFrame, median )$value
base["mean"] <- aggregate(value ~ case+visualization, dataFrame, mean )$value
base["std"] <- aggregate(value ~ case+visualization, dataFrame, sd )$value
base["len"] <- aggregate(value ~ case+visualization, dataFrame, length )$value
base["sum"] <- aggregate(value ~ case+visualization, dataFrame, sum )$value

# mean value per assessment
tapply(base$mean, base$visualization, minMaxMeanMedianStdFun)
# cumulative value per assessment
tapply(base$sum, base$visualization, minMaxMeanMedianStdFun)
# total number of insights per assessment
tapply(base$len, base$visualization, minMaxMeanMedianStdFun)

#########################
# TIME TO FIRST INSIGHT #
#########################

# We can try to also use the Na or zero observations of the time to first insight ( value 1 to 5 )
# in this example we check insights value at least 2
# tmp <- aggregate(time ~ experiment+visualization, subset(dataFrame, value > 4), FUN=min)
for(valueToCheck in c( 0, 1, 2, 3, 4 ) ){
  print( paste( "time to first insight with value >", valueToCheck ) )
  base <- aggregate(value ~ case+visualization, dataFrame, FUN = function(z){ length( z[ z > valueToCheck ] ) } )
  base <- setNames( base, c("case", "visualization", "count") )
  tmp <- aggregate(time ~ case+visualization, subset( dataFrame, value > valueToCheck ), FUN = min )
  tmp <- merge(base, tmp, by = c("case", "visualization"), all.x = TRUE)
  tmp[is.na(tmp)] <- 181 # the three-minute limit we had per assessment
  
  tmp_table <- subset(tmp, visualization == 'table')
  tmp_timeline <- subset(tmp, visualization == 'timeline')
  tmp_table <- tmp_table[order(tmp_table$case),]
  tmp_timeline <- tmp_timeline[order(tmp_timeline$case),]
  
  # tmp <- aggregate(value~case+visualization, dataFrame, FUN=function(z){ length(z[z == valueToCheck]) })
  # tmp <- setNames(tmp, c("case", "visualization", "count"))
  
  # tmp_table <- setNames(tmp_table, c("case", "visualization", "count"))
  # tmp_timeline <- setNames(tmp_timeline, c("case", "visualization", "time"))
  
  p <- wilcox.test(tmp_table$time, tmp_timeline$count, PAIRED = TRUE)$p.value
  print( paste("Wilcox", p) )
  
  p <- kruskal.test(tmp_table$time, tmp_timeline$count, PAIRED = TRUE)$p.value
  print( paste("Kruskal", p) )
  
  # p <- kruskal.test(time~visualization, data = tmp)$p.value
  # print( paste("Kruskal ", p) )
  
  # p <- wilcox.test(time~visualization, data = tmp, p.value=TRUE)$p.value
  # print( paste("wilcox ", p) )
}



#####################################
# INISIGHTS BY VALUE PER ASSESSMENT #
#####################################
# Now we calculate the statistical significance of the number of insights ( value 1 to 5 ) per assessment

# to get the zero counts of insights of a given value use the next instruction
# aggregate(value~experiment+visualization, dataFrame, FUN=function(z){ length(z[z > 2]) })
# to see all the insight values per experiment
aggregate(value~experiment, dataFrame, FUN=function(z){ z }) # z refers to the list of values grouped by experiment

tmp <- aggregate(value~case+visualization, dataFrame, FUN=function(z){ z })
tmp <- aggregate(value~case+visualization, subset(dataFrame, value > 0), FUN=function(z){ z })

# assign to the tmp data frame the total number of insights value at least valueToCheck
for(valueToCheck in c(0, 1, 2, 3, 4, 5)){
  print( paste("total number of insights value", valueToCheck) )
  
  tmp <- aggregate(value~case+visualization, dataFrame, FUN=function(z){ length(z[z == valueToCheck]) })
  
  tmp_table <- subset(tmp, visualization == 'table')
  tmp_timeline <- subset(tmp, visualization == 'timeline')
  tmp_table <- tmp_table[order(tmp_table$case),]
  tmp_timeline <- tmp_timeline[order(tmp_timeline$case),]
  
  # tmp <- aggregate(value~case+visualization, dataFrame, FUN=function(z){ length(z[z == valueToCheck]) })
  # tmp <- setNames(tmp, c("case", "visualization", "count"))
  
  tmp_table <- setNames(tmp_table, c("case", "visualization", "count"))
  tmp_timeline <- setNames(tmp_timeline, c("case", "visualization", "count"))
  
  p <- wilcox.test(tmp_table$count, tmp_timeline$count, PAIRED = TRUE)$p.value
  print( paste("Wilcox", p) )
  
  p <- kruskal.test(tmp_table$count, tmp_timeline$count, PAIRED = TRUE)$p.value
  print( paste("Kruskal", p) )
  
  # p <- kruskal.test(count~visualization, data = tmp)$p.value
  # print( paste("Kruskal", p) )
  
  # p <- wilcox.test(count~visualization, data = tmp)$p.value
  # print( paste("Wilcox", p) )
}

# Total number of insights per assessment
print( "total number of insights" )

tmp <- aggregate(value~case+visualization, dataFrame, FUN=function(z){ length(z) })
tmp <- aggregate(value~case+visualization, subset(dataFrame, value > 0), FUN=function(z){ length(z) })
tmp <- setNames(tmp, c("case", "visualization", "count"))

p <- wilcox.test(count~visualization, data = tmp)$p.value
print( paste("Wilcox", p) )

p <- kruskal.test(count~visualization, data = tmp)$p.value
print( paste("Kruskal", p) )

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

