#####Built and tested under R-Studio v. Version 1.1.383
#####Specifically, you may need R-studio to use the "View" function later in the script

###Bring in libraries
library(ggplot2)
library(reshape2)

###Open the love dataset
lovedata = read.csv("C:/Users/jvand/OneDrive/Documents/Programming for Data Science/Lesson13/loveparsed.csv")
attach(lovedata)

###Open the hate dataset
hatedata = read.csv("C:/Users/jvand/OneDrive/Documents/Programming for Data Science/Lesson13/hateparsed.csv")
attach(hatedata)

###Take a look at our variables and first 6 rows
head(lovedata)
#############Output:
#X RetweetCount
#1 0            1
#2 1            0
# 3 2            0
# 4 3            2
# 5 4            0
# 6 5            0
# Tweet
# 1                        RT @BMEV209: I‚???Tm ready to receive all someone‚???Ts love shit wyaaaaaaaaa Y§∑Yèº‚???ç‚T???Ô∏èY'??? https://t.co/gnk6kIbUCE
# 2                                                                          Childish Gambino DESERVES a grammy. period. Awaken My Love is amazing
# 3                                                                          is it gay if i love both of my best friends and we're also all dating
# 4 RT @loganharting: so proud of my best friend for choosing Ohio University, can't wait to see you do big things and visit you!!! i love you!‚???¶
# 5                                                                                                             @hollynrawls_ Girl I love you :,-)
# 6 @MicNice ..put in. RT I've watched grow since being almost an OG member (I think, first thousand) and I obviously l‚???¶ https://t.co/6opztUiFUU
#   PronounFlag UnknownFlag AtSymbolFlag
# 1           0           0            1
# 2           0           0            0
# 3           0           0            0
# 4           1           0            1
# 5           1           0            1
# 6           0           0            1
################
head(hatedata)
###########Output:
# X RetweetCount
# 1 0          464
# 2 1            0
# 3 2        10579
# 4 3        10579
# 5 4         1374
# 6 5           17
# Tweet
# 1 RT @NicoleMartin__: Me in Kentucky: I haven‚???Tt breathed clearly in 5 years theres too much pollen I hate the ohio valley\nMe anywhere else: F‚???¶
# 2                                                                                                   #np: I Don‚???Tt Hate You -MaseWonder Feat. Yonko
# 3                                               RT @marie_brownsuga: i hate arguing through text. let's argue in person so, i can knock yo ass out.
# 4                                               RT @marie_brownsuga: i hate arguing through text. let's argue in person so, i can knock yo ass out.
# 5  RT @SieteWhite: Me in Texas: I hate this country ass hell hole can't wait to get OUT\n\nMe anywhere else: The Texas flag? ICONIC. I bleed swe‚???¶
# 6                                                                         RT @_ohthatstaay: i absolutely hate when people say  "its up to you" YT"
#   PronounFlag UnknownFlag AtSymbolFlag
# 1           0           0            1
# 2           1           0            0
# 3           0           0            1
# 4           0           0            1
# 5           0           0            1
# 6           0           0            1





###########################################################
#####Do some final data cleanup from transfer##############
###Looking at data to understand why I see 10,002 records on CSV output
View(hatedata)
View(hatedata) #looks OK.

which.nonnum <- function(x) { #This function will determine non-numbers (good stack-overflow find!: https://stackoverflow.com/questions/21196106/finding-non-numeric-data-in-an-r-data-frame-or-vector)
  which(is.na(suppressWarnings(as.numeric(as.character(x)))))
}#End nonnum function
#Run nonnumfunction
which.nonnum(hatedata[[1]]) #And I check for which records have a non-integer (by looking at data I know X should be integer)
###We get 4003, 4004 as records with a problem
#[1] 4003 4004

##Examine records (including before/after)
hatedata[4000:4006,]
###########Output:
# X RetweetCount
# 4000                     3999         1161
# 4001                     4000            0
# 4002                     4001            0
# 4003                  ALWAYS!           NA
# 4004  https://t.co/9ydZpDw3cI            0
# 4005                     4002            0
# 4006                     4003            0
# Tweet
# 4000 RT @666bitchcraft: Me in Brazil: jfc nothing works here I hate this fucking place\n\nMe anywhere else: my country is the fucking BEST, Gisele‚???¶
# 4001   I'm proud of my fellow students at UConn tonight standing up in the face of bigotry and hate to tell them that it h‚???¶ https://t.co/req13kdoYP
# 4002                                                                                                                  Love ALWAYS triumphs over hate.
# 4003                                                                                                                                                 
# 4004                                                                                                                                                0
# 4005                                                                                                                             i hate being ignored
# 4006                           I hate when a nigga running on in my chat. Ize want block &amp; delete  but then it‚???Ts like I want you see me online.
# PronounFlag UnknownFlag AtSymbolFlag
# 4000           0           0            1
# 4001           0           0            0
# 4002          NA          NA           NA
# 4003          NA          NA           NA
# 4004           0          NA           NA
# 4005           0           0            0
# 4006           0           0            0


#Get column names
names(hatedata)
########Output:
# [1] "X"            "RetweetCount" "Tweet"        "PronounFlag"  "UnknownFlag" 
# [6] "AtSymbolFlag"

hatedata.sub <- hatedata[4000:4010,] #Create a subset to look at
hatedata.sub$PronounFlag
#[1]  0  0 NA NA  0  0  0  0  1  0  0   #We see records 4002, 4003 have NAs (issue in CSV save b/c we don't see in Python)
hatedata.sub$UnknownFlag    
#[1]  0  0 NA NA NA  0  0  0  0  1  0
hatedata.sub$AtSymbolFlag
#[1]  1  0 NA NA NA  0  0  1  1  1  1

###I continue reviewing with the below, because I am mainly concerned with the NA for these:
###Upon review, I determine that only 4002, 4003, and 4004 have issues. By comparing
###records with my Python dataframe, I can see that Record 4002 missing values should
###be imputed to be all "zeroes". Records 4003 and 4004 are to be removed as they
###are erroneous due to some issue within the tweet converting to CSV.
###This also brings me to my dataset quanitity of 10,000 records.
hatedata[which(is.na(hatedata$PronounFlag)),]
#########Output:
# X RetweetCount                           Tweet PronounFlag UnknownFlag
# 4002    4001            0 Love ALWAYS triumphs over hate.          NA          NA
# 4003 ALWAYS!           NA                                          NA          NA
# AtSymbolFlag
# 4002           NA
# 4003           NA
hatedata[which(is.na(hatedata$UnknownFlag)),]
#########Output:
# X RetweetCount                           Tweet PronounFlag
# 4002                     4001            0 Love ALWAYS triumphs over hate.          NA
# 4003                  ALWAYS!           NA                                          NA
# 4004  https://t.co/9ydZpDw3cI            0                               0           0
# UnknownFlag AtSymbolFlag
# 4002          NA           NA
# 4003          NA           NA
# 4004          NA           NA
hatedata[which(is.na(hatedata$AtSymbolFlag)),]
#########Output:
# X RetweetCount                           Tweet PronounFlag
# 4002                     4001            0 Love ALWAYS triumphs over hate.          NA
# 4003                  ALWAYS!           NA                                          NA
# 4004  https://t.co/9ydZpDw3cI            0                               0           0
# UnknownFlag AtSymbolFlag
# 4002          NA           NA
# 4003          NA           NA
# 4004          NA           NA
hatedata[which(is.na(hatedata$RetweetCount)),]
########Output:
# X RetweetCount Tweet PronounFlag UnknownFlag AtSymbolFlag
# 4003 ALWAYS!           NA                NA          NA           NA

##############################################################
####################Data Cleansing############################

##To do the data cleansing, I run the following:
hatedata <- hatedata[-c(4003, 4004),] ###Removes these two records
##Then, for record 4002 we need to change the NA's to 0's:
hatedata$PronounFlag[4002] <- 0
hatedata$UnknownFlag[4002] <- 0
hatedata$AtSymbolFlag[4002] <- 0
###And, we look at a visual of the results
View(hatedata)

## By reviewing the View tab, I see that the values I made zero are there
## and the erroneous record is gone. Also, I see my dataset is at 10,000
## records in my environment pane.


##############Begin Analysis###############################
###Sum the number of tweets meeting condition for Pronoun
LoveProCount = sum(lovedata$PronounFlag, na.rm = TRUE )
LoveProCount
#[1] 1509
HateProCount = sum(hatedata$PronounFlag, na.rm = TRUE )
HateProCount
#[1] 1250


###I really thought here about whether to only do tweets that had pronoun also,
###ultimately decided just counting tweets with @ and love/hate would at least allow
###us to determine if our other hypothesis still held up when looking at the 
###mentions. It would be interesting to go back and compare pronouns and @ symbol
###to see if somehow we could fail to reject the null for some scenario.
LoveAtCount = sum(lovedata$AtSymbolFlag, na.rm=TRUE)
LoveAtCount
#[1] 7626
HateAtCount = sum(hatedata$AtSymbolFlag, na.rm=TRUE)
HateAtCount
#[1] 7006


###Test our hypothesis
##This is the proportion of tweets comparing "pronouns":
prop.test(c(LoveProCount, HateProCount), n = c(nrow(lovedata), nrow(hatedata)), alternative = "two.sided", conf.level=0.999)
#############Output:
# 2-sample test for equality of proportions with continuity correction
# 
# data:  c(LoveProCount, HateProCount) out of c(nrow(lovedata), nrow(hatedata))
# X-squared = 27.987, df = 1, p-value = 1.221e-07
# alternative hypothesis: two.sided
# 99.9 percent confidence interval:
#   0.009763815 0.042036185
# sample estimates:
#   prop 1 prop 2 
# 0.1509 0.1250 
#
# Above, we see that our p-value at the alpha=.001 significance level there is enough 
# evidence to claim a difference of proportions of the two-sided test. We see that
# proportion 1 is 15.09% and proportion 2 is 12.50%. The 99.9% confidence interval 
# estimate of the difference between the pronoun proportion of love tweets and 
# the pronoun proportion of hate tweets students is between 0.0098% and 0.0420%. 

##This is the proportion of tweets comparing "mentions":
prop.test(c(LoveAtCount,HateAtCount), n = c(nrow(lovedata), nrow(hatedata)), alternative = "two.sided", conf.level=0.999) 
######Output:
# 2-sample test for equality of proportions with continuity correction
# 
# data:  c(LoveAtCount, HateAtCount) out of c(nrow(lovedata), nrow(hatedata))
# X-squared = 97.565, df = 1, p-value < 2.2e-16
# alternative hypothesis: two.sided
# 99.9 percent confidence interval:
#   0.04132958 0.08267042
# sample estimates:
#   prop 1 prop 2 
# 0.7626 0.7006 
#
# Again, we see that our p-value at the alpha = .001 level is less than .001.
# We do not find it necessary to delve further into this aspect of the problem
# as the evidence already supports that there is still a difference when looking
# at mentions.



#############Here is a sanity check I performed early on to better grasp "Retweets"
###Define Retweets for Sanity Check on if Retweet Count being positive indicates a re-tweeted tweet
FirstTwoLove = substr(lovedata$Tweet, start = 1, stop = 2)

###The below proves there are positive retweet values for non-"RT" tweets. This tells me that if I
###wish to look at tweets that are unique (i.e. not retweeted, duplicated), that I should
###exclude any tweet with a positive retweet value.
retweeted_no_rt = lovedata$Tweet[lovedata$RetweetCount[which(FirstTwoLove != 'RT' & lovedata$RetweetCount>0)]]
length(retweeted_no_rt)
#[1] 191

###Sum the number of tweets meeting condition for Pronoun
LoveProCountExcludingRetweets = sum(lovedata$PronounFlag[which(lovedata$RetweetCount==0)], na.rm = TRUE )
HateProCountExcludingRetweets = sum(hatedata$PronounFlag[which(hatedata$RetweetCount==0)], na.rm = TRUE )
LoveProCountExcludingRetweets
#[1] 702
HateProCountExcludingRetweets
#[1] 445

###Need to redefine denominator since we are excluding retweets
LoveProCountExcludingRetweetsDenominator = length(lovedata$Tweet[which(lovedata$RetweetCount==0)])
HateProCountExcludingRetweetsDenominator = length(hatedata$Tweet[which(hatedata$RetweetCount==0)])
LoveProCountExcludingRetweetsDenominator
#[1] 4048
HateProCountExcludingRetweetsDenominator
#[1] 4206

# What was I doing above? We were determining if we excluded all the 'retweets'
# if we would still have the same outcome as we found above. Here is the outcome...
prop.test(c(LoveProCountExcludingRetweets,HateProCountExcludingRetweets), n = c(LoveProCountExcludingRetweetsDenominator, HateProCountExcludingRetweetsDenominator), alternative = "two.sided", conf.level=0.999) 
##########Output:
# 2-sample test for equality of proportions with continuity correction
# 
# data:  c(LoveProCountExcludingRetweets, HateProCountExcludingRetweets) out of c(LoveProCountExcludingRetweetsDenominator, HateProCountExcludingRetweetsDenominator)
# X-squared = 78.258, df = 1, p-value < 2.2e-16
# alternative hypothesis: two.sided
# 99.9 percent confidence interval:
#   0.04233603 0.09289944
# sample estimates:
#   prop 1    prop 2 
# 0.1734190 0.1058012 
# 
# We actually see that by eliminating "Retweets" from our samples, we have an even
# wider difference in the proportion, with love tweets going 17.3% of the time toward
# people and hate tweets going 10.6% of the time toward people. And, we find the same
# significance level being met.

# The above leads me to wonder, is it perhaps the beginning of another analysis. What
# I think is significant in this point change is that given what we know about our
# dataset, the Retweets were a larger proportion of the "love" tweets than the "hate"
# tweets. So, perhaps it could mean that people are more likely to Retweet a tweet
# about love for another person than they would be likely to Retweet a tweet about
# hate for another person. Again, this is a preliminary assessment of something that
# I think would require further analysis.


###Need to create table for barplot, and also factor the 1/0 flags to have nice labels.
lovetablefreq <- table(factor(lovedata$PronounFlag, levels = c(0, 1), labels = c('No, Not Present', 'Yes, Present')))
###Set Colors for Comparison
NY <- c("blue", "gray")
###Generate barplot for Love Pronouns
barplot(lovetablefreq[order(lovetablefreq, decreasing = FALSE)], horiz = TRUE,
        col = NY, #Use colors
        xlim = c(0, 11000), #Set larger X-Axis
        xlab = "Number of Tweets", #Title X-Axis
        ylab = "Pronoun Included?", #Title Y-Axis
        main = "Analysis of 10,000 Love Tweets for Personal Pronouns" #Title Graph
)
legend("bottomright", c("Present", "Not Present"), fill = NY, title = "Pronoun Presence")

###Need to create table for barplot, and also factor the 1/0 flags to have nice labels.
hatetablefreq <- table(factor(hatedata$PronounFlag, levels = c(0, 1), labels = c('No, Not Present', 'Yes, Present')))
###Generate barplot for Hate Pronouns
barplot(hatetablefreq[order(hatetablefreq, decreasing = FALSE)], horiz = TRUE,
        col = NY, #Use colors
        xlim = c(0, 11000), #Set larger X-Axis
        xlab = "Number of Tweets", #Title X-Axis
        ylab = "Pronoun Included?", #Title Y-Axis
        main = "Analysis of 10,000 Hate Tweets for Personal Pronouns" #Title Graph
)
legend("bottomright", c("Present", "Not Present"), fill = NY, title = "Pronoun Presence")
