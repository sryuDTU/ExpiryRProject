#Selecting the working directory 
setwd("/Users/simray/Library/CloudStorage/OneDrive-DanmarksTekniskeUniversitet/ALLPreT/Courses/Introduction to Applied Statistics and R for PhD Students (Jan 2024)/Lecture 4")
#Making sure it is the right place
getwd()
# Load necessary library to read .xlsx documents directly
library(readxl)

#Loading the dataset
expiry <- read_excel("Expiry.xlsx")

#Looking at the data
head(expiry)
tail(expiry)
str(expiry)

#For Genre of food, it is split into 6 categories, A - Cereal & Bakery Products, B - Condiments, Dressings, Sauces & Spices,
#C - Ready-to-cook, D - Ready-to-eat, E - Soups, F - Confectionery

#For Status, 0 - Not expired (No), 1 - Expire within 7 days (Yes)


#1) Read in the data expiry.txt into a three-way table.
#This is the best - although chi-square is probably wrong 
#This is based on the order of importance, so status first (expire within 7 days or not expired, expiry, then genre)
mytable3 <- xtabs(Count ~  Date + Expiry + Genre, data=expiry)
ftable(mytable3) # print table
summary(mytable3) 

#2)	Calculate the OR and RR (with 95% confidence intervals) for products labeled 
#with a 'use-by' date being expired within 7 days compared to products labeled with a 'best-before' date.

Expiry_Status<-margin.table(mytable3,1:2)
ftable(Expiry_Status)
#Products with use-by dates are more likely to have expired within 7 days from the time of recall

library(epitools)
epitools::oddsratio(Expiry_Status,method="wald")
# The odds of products with use-by dates expiring within 7 days is 16.9 (95% CI 10.8 to 26.5) times 
# the odds for products with best-before dates 

epitools::riskratio(Expiry_Status)
#The risk of products with use-by dates expiring within 7 days is 5.31 (95% CI 4.07 to 6.95) times 
#the risk for products with best-before dates.

#3)	What is the probability of a food item expiring within 7 days the same for items 
#with 'use-by' and 'best-before' labels?

prop.table(Expiry_Status,1)
#The probability that products with use-by dates expiring within 7 days is 0.73, 
#while the probability products with best-before dates that do not past the date is 0.86.


#4)	Do all genres of food have the same probability of expiring within 7 days?
Genre_Status<-margin.table(mytable3,3:2)
ftable(Genre_Status)
#ROW PERCENTAGES
prop.table(Genre_Status,1)
#From this probability table, C and D (RTC and RTE) have highest probability of expiring within 7 days
#B and F (Condiments, Sugar-based confectionery) have lowest probability of expiring within 7 days

# TESTING FOR INDEPENDENCE BETWEEN Genre of Food AND Expiry Status?
#H0: There is no significant association between Expiry Status and Genre of Food
#H1: There is a significant association between Expiry Status and Genre of Food
chisq.test(Genre_Status, correct=FALSE) 
#As calculated χ²  (62.7) > tabular χ² (11.1), df=5, p < 0.00000000000332, reject H0 and accept H1




library(vcd)
mosaic(Genre_Status, shade=TRUE, legend=TRUE) 
#C and D (RTC and RTE) have highest probability of expiring within 7 days
#B and F (Condiments, Sugar-based confectionery) have lowest probability of expiring within 7 days

#5)	Do items with 'use-by' and 'best-before' labels have the same distribution across all genres of food?
Status_Genre<-margin.table(mytable3,c(3,1))
ftable(Status_Genre)

prop.table(Status_Genre,1)
#From this probability table, A and F (Cereal & Bakery Products and confectionery) have the most best-before dates, 
#while D and E (Ready-to-eat, Soups) have the most use-by dates which is logical 

chisq.test(Status_Genre, correct=FALSE) 
mosaic(Status_Genre, shade=TRUE, legend=TRUE) 
#A and F (Cereal & Bakery Products and confectionery) clearly have the most best-before dates, 
#while D and E (Ready-to-eat, Soups) have the most use-by dates which is logical 
