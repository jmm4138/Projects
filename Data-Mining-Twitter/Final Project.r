#read both #metoo and #blacklivesmatter csv files into R, and attach both datasets
metoo_data = read.csv(file="C:/Users/jmmis/Documents/Data Science/DS710 -- Programming/Final Project/metoo_data4.csv")
blm_data = read.csv(file="C:/Users/jmmis/Documents/Data Science/DS710 -- Programming/Final Project/blm_data4.csv")
attach(metoo_data)
attach(blm_data)

#created boxplots for Tweet.Length, Engagement and Passion
boxplot(metoo_data$Tweet.Length, blm_data$Tweet.Length, main="#metoo vs. #blacklivesmatter (Tweet Length)", names=c("#metoo", "#blacklivesmatter"), ylim=c(0,300))
boxplot(metoo_data$Engagement, blm_data$Engagement, main="#metoo vs. #blacklivesmatter (Engagement)", names=c("#metoo", "blacklivesmatter"))
boxplot(metoo_data$Passion, blm_data$Passion, main="#metoo vs. #blacklivesmatter (Passion)", names=c("#metoo", "blacklivesmatter"))

#determined the number of negative, positive and neutral tweets for #metoo
length(which(metoo_data$Sentiment<0))
[1] 182
length(which(metoo_data$Sentiment>0))
[1] 410
length(which(metoo_data$Sentiment==0))
[1] 501

#determined the number of negative, positive and neutral tweets for #blacklivesmatter
> length(which(blm_data$Sentiment<0))
[1] 295
> length(which(blm_data$Sentiment>0))
[1] 313
> length(which(blm_data$Sentiment==0))
[1] 457

#created variables for the counts of neutral, positive, negative tweets for both #metoo and #blacklivesmatter
metoo_neutral <- length(which(metoo_data$Sentiment == 0))
metoo_positive <- length(which(metoo_data$Sentiment > 0))
metoo_negative <- length(which(metoo_data$Sentiment < 0))

blm_neutral <- length(which(blm_data$Sentiment == 0))
blm_positive <- length(which(blm_data$Sentiment > 0))
blm_negative <- length(which(blm_data$Sentiment < 0))

#created barplots for sentiment counts for both #metoo and #blacklivesmatter
metoo_counts<-c(metoo_negative, metoo_neutral, metoo_positive)
cols=c("Negative","Neutral","Positive")
barplot(metoo_counts, names.arg = cols, main="#metoo Sentiment Analysis", col=c("red", "yellow", "green"), ylim=c(0,500))

blm_counts<-c(blm_negative, blm_neutral, blm_positive)
cols=c("Negative","Neutral","Positive")
barplot(blm_counts, names.arg = cols, main="#blacklivesmatter Sentiment Analysis", col=c("red", "yellow", "green"), ylim=c(0,500))

#t-tests for all four categories to determine potential differences between #metoo and #blacklivesmatter; results of t-tests also included

t.test(metoo_data$Engagement,blm_data$Engagement,alternative="two.sided")
#Welch Two Sample t-test
#data:  metoo_data$Engagement and blm_data$Engagement
#t = 0.45761, df = 1652.7, p-value = 0.6473
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
# -3.501391  5.632342
#sample estimates:
#mean of x mean of y 
# 8.371269  7.305793 
 
t.test(metoo_data$Tweet.Length,blm_data$Tweet.Length,alternative="two.sided")
#Welch Two Sample t-test
#data:  metoo_data$Tweet.Length and blm_data$Tweet.Length
#t = -2.5641, df = 2122.8, p-value = 0.01041
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
# -14.508953  -1.933321
#sample estimates:
#mean of x mean of y 
# 129.6241  137.8452 

t.test(metoo_data$Passion,blm_data$Passion,alternative="two.sided")
#Welch Two Sample t-test
#data:  metoo_data$Passion and blm_data$Passion
#t = -6.5641, df = 2051.5, p-value = 6.611e-11
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
# -5.301897 -2.862624
#sample estimates:
#mean of x mean of y 
# 11.03265  15.11491 

t.test(metoo_data$Sentiment,blm_data$Sentiment,alternative="two.sided")
#Welch Two Sample t-test
#data:  metoo_data$Sentiment and blm_data$Sentiment
#t = 7.292, df = 2113.2, p-value = 4.297e-13
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
# 0.06295952 0.10928169
#sample estimates:
# mean of x  mean of y 
#0.10296882 0.01684822 


