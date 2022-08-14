
#Import Libraries
library("corpcor")
library("GPArotation")
library("psych")
library("IDPmisc")
library(dplyr)

studentSurvey1<- studentSurvey[,31:87]


#Sample size met



#First some Data Wrangling

#the category of the data
str(studentSurvey1)

#Changed the names of the columns that had strings in them
names(studentSurvey1)[names(studentSurvey1)=="Q6 [What are the best aspects of the program?]"] <- "Q6"

names(studentSurvey1)[names(studentSurvey1)=="Q7. In your opinion,the best aspect of the program is"] <- "Q7"
 
names(studentSurvey1)[names(studentSurvey1)=="Q8. In your opinion,the next best aspect of the program is"] <- "Q8"
 
names(studentSurvey1)[names(studentSurvey1)=="What aspects of the program could be improved?"] <- "Q9"
 
names(studentSurvey1)[names(studentSurvey1)=="Do you feel that the quality of education improved at EU over the last year?"] <- "Q10"


names(studentSurvey1)[names(studentSurvey1)=="Do you feel that the image of the University improved over the last year?"] <- "Q11"

#Had to fix this mistake from the recoding above 
names(studentSurvey2)[names(studentSurvey2)=="Q12"] <- "Q11"
names(studentSurvey2) 


#Dropping a column that is blank 
studentSurvey2 <- subset(studentSurvey1, select = -c(Username))

#Recoding variables of columns
str(studentSurvey2)
str(studentSurvey3)

print(select(studentSurvey2,Q7))
table(studentSurvey2$Q7)

studentSurvey2$Q7r <- NA
studentSurvey2$Q7r[studentSurvey2$Q7 == "Faculty"] <- 4
studentSurvey2$Q7r[studentSurvey2$Q7 == "Resources"] <- 3
studentSurvey2$Q7r[studentSurvey2$Q7 == "Teaching/Learning"] <- 2
studentSurvey2$Q7r[studentSurvey2$Q7 == "Other"] <- 1

print(select(studentSurvey2,Q8))
table(studentSurvey2$Q8)

studentSurvey2$Q8r <- NA
studentSurvey2$Q8r[studentSurvey2$Q8 == "Faculty"] <- 5
studentSurvey2$Q8r[studentSurvey2$Q8 == "Resources"] <- 4
studentSurvey2$Q8r[studentSurvey2$Q8 == "Teaching/Learning"] <- 3
studentSurvey2$Q8r[studentSurvey2$Q8 == "Overall learing environment"] <- 2
studentSurvey2$Q8r[studentSurvey2$Q8 == "Other"] <- 1

#Too large of an answer pole so dropping
print(select(studentSurvey2,Q9))
table(studentSurvey2$Q9)
studentSurvey3 <- subset(studentSurvey2, select = -c(Q9))

print(select(studentSurvey3,Q10))
table(studentSurvey2$Q10)

studentSurvey3$Q10r <- NA
studentSurvey3$Q10r[studentSurvey3$Q10 == "Yes"] <- 2
studentSurvey3$Q10r[studentSurvey3$Q10 == "No"] <- 1

print(select(studentSurvey3,Q11))
table(studentSurvey3$Q11)

studentSurvey3$Q11r <- NA
studentSurvey3$Q11r[studentSurvey3$Q11 == "Yes"] <- 2
studentSurvey3$Q11r[studentSurvey3$Q11 == "No"] <- 1

#Drop the columns that were recoded
str(studentSurvey3)
studentSurvey4 <- subset(studentSurvey3, select = -c(Q7, Q8, Q10, Q11))

#Dropping cells that are has NA
studentSurvey4[complete.cases(studentSurvey4),]


str(studentSurvey4)

#Absence of Multicollinearity
SS1matrix <- cor(studentSurvey4,  use = "complete.obs")

View(round(SS1matrix, 2))

#I see NA from mine. Not sure why


#Run the Bartlett's Test
cortest.bartlett(studentSurvey4)

#my p value is 0 and it needs to be significant

#Checking determinants
det(SS1matrix)

#It is not greater than 0.00001
#We will still continue for the lesson



# Now testing


# Determine the number of factors
pcModel1 <- principal(studentSurvey4, nfactors = 10, rotate = "none")
pcModel1
#You look at the SS load and we saw that PC1 and PC9 has 1 or more and so
# there is something to explore in there. It specifies there is 9 factors that correlate. 

#If you are visual you will use this scree plot
# Examine the scree Plot
plot(pcModel1$values, type="b")

#based on this graph seems like there are only 2 factors. 

#we will run another model this time using 2 as the nfactor
pcModel2 <- principal(studentSurvey4, nfactors = 9, rotate = "none")


#Examining Residual to determine Model fit
residuals <- factor.residuals(SS1matrix, pcModel2$loadings)


residuals <- as.matrix(residuals[upper.tri(residuals)])


largeResid <- abs(residuals) > .05

sum(largeResid)

sum(largeResid/nrow(residuals))

# 35% of the residuals are large and that is under 50%. So having
#9 factors is a pretty good model.



#Rotate the factor 
#Here we change the rotate to oblique rotation
#assumes the survey items are related
pcModel3 <- principal(studentSurvey4, nfactors = 9, rotate = "oblimin")
pcModel3

print.psych(pcModel3, cut = .3, sort=TRUE)
#When you look at the results it shows a pattern with the wording of
#the questions TC1 is more negative and TC2 questions are more positive


#Orthogonal Rotation
#It assumes the survey items are not related
pcModel4 <- principal(studentSurvey4, nfactors = 2, rotate = "varimax")
print.psych(pcModel4, cut=.3, sort=TRUE)

#Based on the analysis the last few items were not needed, Q6, , Q7r, Q8r, Q10r
# Q11r





#Reliability and Validity Analysis
library("psych")

studentSurvey5<- studentSurvey4

# Data wrangling
#dataframe per factor or a column for each factor

academia<- studentSurvey5[, c(1:12)]
policy <- studentSurvey5[, c(24,42:45)]
facilities  <- studentSurvey5[, c(29:34)]
mentors <- studentSurvey5[, c(23,26,35:41)]
expectation <- studentSurvey5[, c(46:50)]
learning <- studentSurvey5[, c(13:17,22,27:29)]


#Testing Reliability
alpha(academia)
alpha(policy)
alpha(facilities)
alpha(mentors)
alpha(expectation)
alpha(learning)
alpha(studentSurvey5)

#academia- raw alpha is above 0.8 as 0.92 and r drop is above 0.3 for all items
# so it very reliable

#policy- raw alpha is just above 0.8 as 0.88 and r drop is above 0.3 for all items
# so it is reliable

#facilities- raw alpha is just above 0.8 as 0.88 and r drop is above 0.3 for all items
# so it reliable

#mentors- raw alpha is just above 0.8 as 0.91 and r drop is above 0.3 for all items
# so it is reliable


#expectations- raw alpha is just below 0.8 as 0.79 but these are only 5 items
# so the Chronbach alpha can be lowered and r drop is above 0.3 for all items
# so it reliable

#learning- raw alpha is just above 0.8 as 0.9 and r drop is above 0.3 for all items
# so it is reliable



#studentSurvey5 the overall data set- raw alpha is just above 0.8 as 0.97
#and r drop is above 0.3 for all items above Q5
# so it is very reliable


