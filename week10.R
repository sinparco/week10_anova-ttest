library(tidyverse)

my.data <- read.csv("drugData.csv")
my.data$Group <- as.factor(my.data$Group)
psych::describeBy(x=my.data$Arousal, group=my.data$Group)
my.data.grouped <- group_by(my.data, Group)
my.data.grouped %>% summarise(M=mean(Arousal, na.rm=TRUE), SD=sd(Arousal, na.rm=TRUE)) #Create Columnns with the name summarizing the following

#install.packages("car", dep=T)

car::leveneTest(mdata$Arousal, group=my.data$Group, center="median") #When it is non significant, the variances are the same.

exp.group.rows <- my.data %>% filter(Group==0) #Filter gets the subset of rows, $ gives you subset for columns
control.group.rows <- my.data %>% filter (Group==1) #Groupby and Summarize really good for final exam!

t.test(x=exp.group.rows$Arousal, y=control.group.rows$Arousal, var.equal=TRUE) #Do a t-test with similar variance, IF var.equal=FALSE, it would have a very different value...it means the variance between the two groups are different

#In ANOVA, the distribution of sample means is more important than looking at the distribution of an independent sample.
#Assuming the variance is equal, take the total variance and divide it by sample size to get standard error. 
#When calculating the F-Ratio for ANOVA, use SE2 (sd2/n) in the denominator --- and observed variance of means on the top sum of(observed-average)^2/n-1
##sd2 is supposed to be the population variance, but since that is never known, we just take the average of the variance between ALL groups.

library(MBESS)

smd(Group.1=exp.group.rows$Arousal, Group.2=control.group.rows$Arousal) #calculates d values (difference between means in sd terms, ie., standardized mean differences)
smd(Mean.1=3.2, s.1=.8, Mean.2=2.45, s.2=2.91, n.1=10, n.2=10)
ci.smd(smd=0875, n.1=10, n.2=10)
