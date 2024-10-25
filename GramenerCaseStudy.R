#####################
#####################
## PACKAGE LOADING ##
#####################
#####################

##########################################################################################################
# THE PACKAGES "stringr", "data.table", "corrplot", "ggplot2", "dplyr", tidyr", "reshape2" and "mosaic"  #
# ARE TO BE INSTALLED BEFORE LOADING THE PACKAGES.                                                       #
##########################################################################################################

# LOAD THE PACKAGES, "stringr", "data.table", "corrplot", "ggplot2", "dplyr", tidyr", "reshape2" and "mosaic"
# TO THE R SESSION.
library(stringr)
library(data.table)
library(corrplot)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(lattice)
library(ggformula)
library(mosaic)

##################
##################
## DATA LOADING ##
##################
##################

##############################################################
# READING THE INPUT FILE AND LOADING INTO DATAFRAME, "loan"  #
##############################################################

# READING THE "loan.csv" FILE INTO "loan" DATAFRAME WITH FOLLOWING ARGUMENTS
# header = TRUE   TO MENTION THAT loan.csv FILE HAS HEADER ROW
# na.strings = c("", "NA") TO CONSIDER BOTH "" and "NA" AS NA
# stringsAsFactors = FALSE  TO NOT CONSIDER STRING VALUED COLUMNS AS FACTORS
loan <- read.csv("loan.csv", header = TRUE, na.strings = c("","NA"), stringsAsFactors = FALSE)

####################
####################
## DATE CLEANSING ##
####################
####################

###############
# NA HANDLING #
##########################################################################################################
# REMOVING COLUMNS WHICH HAS                                                                             # 
# a. ALL NA's                                                                                            #
# b. NA VALUES MORE THAN 50%                                                                             #
##########################################################################################################

# REMOVING THE COLUMNS WHICH HAS JUST 'NA'.
loan <- loan[,colSums(is.na(loan))<nrow(loan)]

# REMOVING THE COLUMNS HAVING MORE THAN 50% OF NA VALUES
loan <- loan <- loan[,colMeans(is.na(loan)) <= 0.50]

####################
# REMOVING COLUMNS #
############################################################################################################
# REMOVING THE COLUMNS WHICH ADDS NO VALUE FOR ANALYSIS, REDUNDANT COLUMNS AND COLUMNS HAVING SINGLE VALUE #
# FOLLOWING ARE THE COLUMNS WHICH ARE IDENTIFIED FOR DELETION                                              #
# 1. "url"  - adds no value for analysis                                                                   #
# 2. "desc" - adds no value for analysis                                                                   #
# 3. "id" - randomly generated, adds no value for analysis                                                 #
# 4. "member_id" - randomly generated, adds no value for analysis                                          #
# 5. "funded_amnt" - adds no value for analysis                                                            #
# 6. "funded_amnt_inv" - adds no value for analysis                                                        #
# 7. "issue_d" - adds no value for analysis                                                                #
# 8. "out_prncp" - adds no value for analysis                                                              #
# 9. "out_prncp_inv" - adds no value for analysis                                                          #
# 10. "total_pymnt" - adds no value for analysis                                                           #
# 11. "total_pymnt_inv" - adds no value for analysis                                                       #
# 12. "total_rec_prncp" - adds no value for analysis                                                       #
# 13. "total_rec_int" - adds no value for analysis                                                         #
# 14. "total_rec_late_fee" - adds no value for analysis                                                    #
# 15. "recoveries" - adds no value for analysis                                                            #  
# 16. "collection_recovery_fee" - adds no value for analysis                                               #
# 17. "last_pymnt_d" - adds no value for analysis                                                          #
# 18. "last_pymnt_amnt" - adds no value for analysis                                                       #
#                                                                                                          #
# 19. "sub_grade" - redundant as already contain "grade" column                                            #
# 20. "zip_code" - redundant as already we have "addr_state" column                                        #
#                                                                                                          #
# 21. "" - column having single value across dataframe                                                     #
############################################################################################################

# DELETING COLUMNS WHICH ADDS NO VALUE FOR ANALYIS
columnsAddingNoValueForAnalysis <- c("url", "desc","id","member_id",
                                     "funded_amnt", "funded_amnt_inv", "issue_d", "out_prncp", 
                                     "out_prncp_inv", "total_pymnt", "total_pymnt_inv", "total_rec_prncp", 
                                     "total_rec_int", "total_rec_late_fee", "recoveries", "collection_recovery_fee", 
                                     "last_pymnt_d", "last_pymnt_amnt")
loan <- loan[,!(colnames(loan) %in% columnsAddingNoValueForAnalysis)]

# DELETING COLUMNS WHICH ARE REDUNDANT
columnsRedundantForAnalysis <- c("sub_grade", "zip_code")
loan <- loan[,!(colnames(loan) %in% columnsRedundantForAnalysis)]

# DELETING COLUMNS WHICH HAS A SINGLE VALUE AS ANALYSING COLUMNS HAVING SAME VALUES ACROSS DATA MAKES NO VALUE #
loan <- Filter(function(x)(length(unique(x))>1), loan)

#########################
# DATE COLUMNS HANDLING #
########################################################################################################
# COLUMNS, "earliest_cr_line" and "last_credit_pull_d"                                                 #
# ARE APPENDED WITH DATE AS 01 (AS DATE IS MISSING) AND CONVERTED INTO DATE OBJECTS                    #
########################################################################################################

loan$earliest_cr_line <- strptime(paste("01", loan$earliest_cr_line, sep = "-"), format = "%d-%b-%y")
loan$last_credit_pull_d <- strptime(paste("01", loan$last_credit_pull_d , sep = "-"), format = "%d-%b-%y")

###############################
# PERCENTAGE COLUMNS HANDLING #
##############################################################################################################
# COLUMN "revol_util" HAS PERCENTAGE VALUES. HERE WE ARE GOING TO REMOVE '%' SYMBOL ACROSS THE COLUMN VALUES #
# AND JUST KEEP THE NUMERIC VALUES. CONVERT THIS COLUMN INTO NUMERIC TYPES                                   #
# # AND UPDATE NA VALUES TO 0                                                                                #
##############################################################################################################

loan$revol_util <- gsub("%.*","",loan$revol_util)
loan$revol_util <- as.numeric(loan$revol_util)
loan[which(is.na(loan$revol_util)), "revol_util"] <- 0

loan$int_rate <- gsub("%.*","",loan$int_rate)
loan$int_rate <- as.numeric(loan$int_rate)

################################
# 'emp_length' COLUMN HANDLING #
########################################################################################################
# HANDLING "emp_length" COLUMN BASED ON FOLLOWING ASSUMPTIONS                                          #
# <1 YEAR CONSIDERED AS 0 AS PER DATA DICTIONARY.XLSX                                                  #
# 10+ YEARS CONSIDERED AS 10 AS PER DATA DICTIONARY.XLSX                                               #
# n/a CONSIDERED AS 0 (ASSUMPTION MADE)                                                                #
########################################################################################################

loan$emp_length <- trimws(loan$emp_length)

loan$emp_length <- ifelse(loan$emp_length == "n/a", "0 year", loan$emp_length)
loan$emp_length <- ifelse(loan$emp_length == "10+ years", "10 years", loan$emp_length)
loan$emp_length <- ifelse(loan$emp_length == "< 1 year", "0 year", loan$emp_length)


######################
# NA VALUES HANDLING #
#################################################################
# NA VALUES IN FOLLOWING COLUMNS ARE HANDLED AS MENTIONED BELOW #
#                                                               #
# COLUMN NAME                 NA VALUE REPLACED WITH            #
# -----------                 ----------------------            #
# emp_title                   Self Employed                     #
# title                       others                            #
# collections_12_mths_ex_med  0                                 #
# pub_rec_bankruptcies        0                                 #
# tax_liens                   0                                 #
#################################################################

loan[which(is.na(loan[,"emp_title"])),"emp_title"]<-"Self Employed"
loan[which(is.na(loan[,"title"])),"title"] <- "Others"
loan[which(is.na(loan[,"collections_12_mths_ex_med"])),"collections_12_mths_ex_med"] <- 0
loan[which(is.na(loan[,"pub_rec_bankruptcies"])),"pub_rec_bankruptcies"] <- 0
loan[which(is.na(loan[,"tax_liens"])),"tax_liens"] <- 0

View(loan)
######################
######################
## DERIVED METRICES ##
######################
######################

# CREATE BINS FOR 'loan_amnt' COLUMN
# COLUMN NAME ::  loanAmountBins
loan$`loanAmountBins` <- cut(loan$loan_amnt, breaks=c(0, 1000, 2500, 5000, 7500, 10000, 12500, 15000, 17500, 20000, 22500, 25000, 27500, 30000, 32500, 35000), include.lowest =TRUE)
loan$`loanAmountBins` <- as.factor(loan$`loanAmountBins`)
levels(loan$`loanAmountBins`) <- c("0-1000", "1000-2500", "2500-5000", "5000-7500", "7500-10000", "10000-12500", "12500-15000", "15000-17500", "17500-20000", "20000-22500", "22500-25000", "25000-27500", "27500-30000", "30000-32500", "32500-35000")

# CREATE BINS FOR 'annual_inc' COLUMN
# COLUMN NAME ::  annualIncomeBins

loan$`annualIncomeBins` <- cut(loan$annual_inc, breaks=c(0, 5000, 10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000, 125000, 150000, 175000, 200000), include.lowest =TRUE)
loan$`annualIncomeBins` <- as.factor(loan$`annualIncomeBins`)
levels(loan$`annualIncomeBins`) <- c("0-5000", "5000-10000", "10000-20000", "20000-30000", "30000-40000", "40000-50000", "50000-60000", "60000-70000", "70000-80000", "80000-90000", "90000-100000", "100000-125000", "125000-150000", "150000-175000", "175000-200000")

# CREATE BINS FOR 'int_rate' COLUMN
# COLUMN NAME ::  intRateBins

loan$`intRateBins` <- cut(loan$int_rate, breaks=c(5, 7.5, 10, 12.5, 15, 17.5, 20, 22.5, 25), include.lowest =TRUE)
loan$`intRateBins` <- as.factor(loan$`intRateBins`)
levels(loan$`intRateBins`) <- c("0-5", "5-7.5", "7.5-10", "10-12.5", "12.5-15", "15-17.5", "17.5-20", "20-22.5", "22.5-25")

# CREATE BINS FOR 'revol_util' COLUMN
# COLUMN NAME ::  revolUtilRateBins

loan$`revolUtilRateBins` <- cut(loan$revol_util, breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100), include.lowest =TRUE)
loan$`revolUtilRateBins` <- as.factor(loan$`revolUtilRateBins`)
levels(loan$`revolUtilRateBins`) <- c("0-5", "5-10", "10-15", "15-20", "20-25", "25-30", "30-35", "35-40", "40-45", "45-50", "50-55", "55-60", "60-65", "65-70","70-75", "75-80", "80-85", "85-90", "90-95", "95-100")

write.csv(loan, "GramenerResults.csv", row.names = FALSE)


###################
###################
## DATA ANALYSIS ##
###################
###################

# SINCE OUR CURRENT FOCUS IS ONLY LOAN STATUS 'FULLY PAID' AND 'CHARGED OFF' 
# WE ARE GOING TO REMOVE THE DATA HAVING LOAN STATUS AS 'CURRENT.

loan <- subset(loan, loan$loan_status != 'Current')


#######################
# UNIVARIATE ANALYSIS #
#######################

# 1.
# COLUMN CONSIDERED :: loanAmountBins (DERIVED COLUMN FROM 'loan_amnt')
# INSIGHT           :: We can come to know the loan amounts distribution form the Lending Club

str(loan$loanAmountBins)

# RECORDS EACH LOAN AMOUNT
table(loan$loanAmountBins)

# PROPORTION OF EACH TYPE OF LOAN AMOUNT
prop.table(table(loan$loanAmountBins))

#THIS IS THE FREQUENCY BAR PLOT. WE GET TO KNOW THE DISTRIBUTION OF LOAN AMOUNTS IN THE DATA
barchart(prop.table(table(loan$loanAmountBins)))

#THIS IS THE BOX PLOT. WE GET TO KNOW THE DISTRUBUTION OF LOAN AMOUNTS IN THE DATA AND OUTLIERS
boxplot(loan$loan_amnt)

summary(loan$loan_amnt)

ggplot(loan, aes(x=loan$loanAmountBins)) + geom_bar(aes(fill = loan$loan_status), position = "dodge")

# OBSERVATION       ::  Observed that people having loan amounts in the range of "7500-10000" and "2500-5000" are heavily charged Off
#                       Observed that prople having loan amounts in the range of "7500-10000" and "2500-5000" are prompt in paying off their loans


# 2.
# COLUMN CONSIDERED :: term
# INSIGHT           :: We can come to know the tenure distribution for the loans

str(loan$term)

# RECORDS TERMS OF LOAN
table(loan$term)

# PROPORTION OF EACH TERM TYPE FOR LOAN RECORDS
prop.table(table(loan$term))

#THIS IS THE FREQUENCY BAR PLOT. WE GET TO KNOW THE DISTRIBUTION OF TERMS IN THE DATA
barchart(prop.table(table(loan$term)))

ggplot(loan, aes(x=loan$term)) + geom_bar(aes(fill=loan$loan_status), position = "dodge")

# OBSERVATION       ::  Observed that people having loan tenure as 36 months are heavily charging off than people having loan tenure as 60 months
#                       Observed that people having loan tenure as 36 months have promptly paid off their loans compared to people having 60 months as loan tenure


#3.
# COLUMN CONSIDERED :: grade
# INSIGHT           :: We can come to know the loan percentages based on their grades

str(loan$grade)

# RECORDS GRADES OF LOAN DATA
table(loan$grade)

# PROPORTION OF EACH GRADE TYPE FOR LOAN RECORDS
prop.table(table(loan$grade))

#THIS IS THE FREQUENCY BAR PLOT. WE GET TO KNOW THE DISTRIBUTION OF GRADES IN THE DATA
barchart(prop.table(table(loan$grade)))

ggplot(loan, aes(x=loan$grade)) + geom_bar(aes(fill=loan$loan_status), position = "dodge")

# OBSERVATION       ::  Observed that 'B' Grade loans are having more chances of getting Charged Off
#                       Observed that 'B' Grade loans are having more chances of getting Paid Completely


#4.
# COLUMN CONSIDERED :: emp_length
# INSIGHT           :: We can come to know individual with what experiences are demanding loans more

str(loan$emp_length)

# RECORDS LOAN APPLICANT'S EXPERIENCE IN LOAN DATA
table(loan$emp_length)

# PROPORTION OF LOAN APPLICANT'S EXPERIENCE TYPE FOR LOAN RECORDS
prop.table(table(loan$emp_length))

#THIS IS THE FREQUENCY BAR PLOT. WE GET TO KNOW THE DISTRIBUTION OF LOAN APPLICANT'S EXPERIENCE IN THE DATA
barchart(prop.table(table(loan$emp_length)))

ggplot(loan, aes(x=loan$emp_length)) + geom_bar(aes(fill=loan$loan_status), position = "dodge")

# OBSERVATION       ::  Observed that people having 10 or 10+ Years of experience are more in defaulting the loan.
#                       Observed that people having 10 or 10+ Years of experience are more promt in paying the loan completely.


#5.
# COLUMN CONSIDERED :: home_ownership
# INSIGHT           :: We can come to know the residential status of loan applicants

str(loan$home_ownership)

# RECORDS PERSON HOME OWNERSHIP STATUS IN LOAN DATA
table(loan$home_ownership)

# PROPORTION OF PERSON HOME OWNERSHIP STATUS TYPE FOR LOAN RECORDS
prop.table(table(loan$home_ownership))

#THIS IS THE FREQUENCY BAR PLOT. WE GET TO KNOW THE DISTRIBUTION OF PERSON HOME OWNERSHIP STATUS IN THE DATA
barchart(prop.table(table(loan$home_ownership)))

ggplot(loan, aes(x=loan$home_ownership)) + geom_bar(aes(fill=loan$loan_status), position = "dodge")

# OBSERVATION       ::  Observed that people staying in RENT followed by MORTGAGE homes are more in defaulting the loan followed by MORTGAGE homes
#                       Observed that people staying in RENT followed by MORTGAGE homes are more promt in paying the loan completely followed by MORTGAGE homes


#6.
# COLUMN CONSIDERED :: annualIncomeBins (DERIVED COLUMN FROM 'annual_inc')
# INSIGHT           :: We can come to know income ranges of people applying for loans

str(loan$annualIncomeBins)

# RECORDS PERSON ANNUAL INCOME BINS IN LOAN DATA
table(loan$annualIncomeBins)

# PROPORTION OF PERSON ANNUAL INCOME BINS FOR LOAN RECORDS
prop.table(table(loan$annualIncomeBins))

#THIS IS THE FREQUENCY BAR PLOT. WE GET TO KNOW THE DISTRIBUTION OF ANNUAL INCOME BINS IN THE DATA
barchart(prop.table(table(loan$annualIncomeBins)))

#THIS IS THE BOX PLOT. WE GET TO KNOW THE DISTRUBUTION OF ANNUAL INCOME  IN THE DATA AND OUTLIERS
boxplot(loan$annual_inc)

summary(loan$annual_inc)

ggplot(loan, aes(x=loan$annualIncomeBins)) + geom_bar(aes(fill=loan$loan_status), position = "dodge")

# OBSERVATION       ::  Observed that people having annual incomes in range of 30,000-40,000 are more in defaulting the loan
#                       Observed that people having annual incomes in range of 40,000-50,000 are more promt in paying the loan completely


#7.
# COLUMN CONSIDERED :: purpose
# INSIGHT           :: We can see the purpose of crowd for loans

str(loan$purpose)

# RECORDS PERSON PURPOSE IN LOAN DATA
table(loan$purpose)

# PROPORTION OF PERSON PURPOSE TYPE FOR LOAN RECORDS
prop.table(table(loan$purpose))

#THIS IS THE FREQUENCY BAR PLOT. WE GET TO KNOW THE DISTRIBUTION OF PERSON PURPOSE IN THE DATA
barchart(prop.table(table(loan$purpose)))

ggplot(loan, aes(x=loan$purpose)) + geom_bar(aes(fill=loan$loan_status), position = "dodge")

# OBSERVATION       ::  Observed that people taking loans for debt consolidation are more in defaulting the loan
#                       Observed that people taking loans for debt consolidation are more promt in paying the loan completely


# 8.
# COLUMN CONSIDERED :: intRateBins (DERIVED COLUMN FROM 'int_rate')
# INSIGHT           :: We can come to know the interest rates distribution form the Lending Club
str(loan$intRateBins)

# RECORDS PERSON LOAN INEREST RATE  IN LOAN DATA
table(loan$intRateBins)

# PROPORTION OF PERSON LOAN INEREST RATE BIN FOR LOAN RECORDS
prop.table(table(loan$intRateBins))

#THIS IS THE FREQUENCY BAR PLOT. WE GET TO KNOW THE DISTRIBUTION OF PERSON LOAN INEREST RATE IN THE DATA
barchart(prop.table(table(loan$intRateBins)))

#THIS IS THE BOX PLOT. WE GET TO KNOW THE DISTRUBUTION OF LOAN INEREST RATE IN THE DATA AND OUTLIERS
boxplot(loan$int_rate)

summary(loan$int_rate)

ggplot(loan, aes(x=loan$intRateBins)) + geom_bar(aes(fill = loan$loan_status), position = "dodge")

# OBSERVATION       ::  Observed that people having interest rates in the range of "10%-12.5%" are heavily charged Off
#                       Observed that prople having interest rates in the range of "7.5%-10%"  are prompt in paying off their loans


# 9.
# COLUMN CONSIDERED :: revolUtilRateBins (DERIVED COLUMN FROM 'revol_util')
# INSIGHT           :: We can come to know the revol util rates distribution form the Lending Club

str(loan$revolUtilRateBins)

# RECORDS PERSON EXPENDITURE PERCENTAGE IN LOAN DATA
table(loan$revolUtilRateBins)

# PROPORTION OF PERSON EXPENDITURE PERCENTAGE BIN FOR LOAN RECORDS
prop.table(table(loan$revolUtilRateBins))

#THIS IS THE FREQUENCY BAR PLOT. WE GET TO KNOW THE DISTRIBUTION OF EXPENDITURE PERCENTAGE IN THE DATA
barchart(prop.table(table(loan$revolUtilRateBins)))

# SEGMENTED UNIVARAITE ANALYSIS WIH THE TARGET VARIABLE

ggplot(loan, aes(x=loan$revol_util)) + geom_bar(aes(fill = loan$loan_status), position = "dodge")

# OBSERVATION       ::  Observed that people having revol util rates in the range of "70%-75%" are heavily charged Off
#                       Observed that prople having interest rates in the range of "0-5%"  are prompt in paying off their loans


###########################################
# CORELATION BETWEEN CONTINUOUS VARIABLES #
###########################################

corLoanData <- round(cor(loan[,unlist(lapply(loan, is.numeric))]),2)
head(corLoanData)

reshapedLoanData <- melt(head(corLoanData))
head(reshapedLoanData)

ggplot(data = reshapedLoanData, aes(x=Var1, y=Var2, fill=value)) + geom_tile()


# OBSERVATION :: Positive correlation exists between Loan amount and installment followed by interest rate and annual income
#                Negative correlation between dti and annual_income



######################
# BIVARIATE ANALYSIS #
######################

# 1.
# COLUMNS CONSIDERED :: loanAmountBins & term
# INSIGHT            :: We compare Loan Amount Bins with Term to see the results

jitterPosObj <- position_jitter(width=1)
ggplot(loan, aes(x=loan$term, y=loan$loanAmountBins, col=loan$loan_status)) + geom_jitter(position=jitterPosObj)   


# OBSERVATION       ::  In 36 months below 5000 higher the defaulters
#                       In 60 months no standard pattern observed but defaulters are high with high amount in few brackets (22500-25000 and 32500-35000)
#                       In 60 months there are more defaulters than 36 months almost across all brackets

# 2.
# COLUMNS CONSIDERED :: loanAmountBins Vs annualIncomeBins
# INSIGHT            :: We compare Loan Amount Bins with Annual Income Bins to see the results

jitterPosObj <- position_jitter(width=1)
ggplot(loan, aes(x=loan$annualIncomeBins, y=loan$loanAmountBins, col=loan$loan_status)) + geom_jitter(position=jitterPosObj)  


# OBSERVATION       ::  Lesser Annual Income people are observed more defaulters comparatively

# 3.
# COLUMNS CONSIDERED :: loanAmountBins Vs home_ownership
# INSIGHT            :: We compare Loan Amount Bins with Home ownership to see the results

jitterPosObj <- position_jitter(width=1)
ggplot(loan, aes(x=loan$home_ownership, y=loan$loanAmountBins, col=loan$loan_status)) + geom_jitter(position=jitterPosObj)  


# OBSERVATION       ::  Under RENT/ MORTGAGE higher the loan amount more number of defaulters observed


# 4.
# COLUMNS CONSIDERED :: loanAmountBins Vs purpose
# INSIGHT            :: We compare Loan Amount Bins with Purpose of loan to see the results

jitterPosObj <- position_jitter(width=1)
ggplot(loan, aes(x=loan$purpose, y=loan$loanAmountBins, col=loan$loan_status)) + geom_jitter(position=jitterPosObj)  


# OBSERVATION       ::  Higher the loan amount in debt consolidation bracket more number of defaulters found.


# 5.
# COLUMNS CONSIDERED :: loanAmountBins Vs intRateBins
# INSIGHT            :: We compare Loan Amount Bins with Interest Rate Bins to see the results

jitterPosObj <- position_jitter(width=1)
ggplot(loan, aes(x=loan$intRateBins, y=loan$loanAmountBins, col=loan$loan_status)) + geom_jitter(position=jitterPosObj) 


# OBSERVATION       ::  Higher the interest across all brackets of loan amounts are observed to be more defaulters 
#                       Lesser the interest across all brackets of loan amount less chances of defaulters

# 6.
# COLUMNS CONSIDERED :: loanAmountBins Vs revolUtilRateBins
# INSIGHT            :: We compare Loan Amount Bins with Revol Util Rate Bins to see the results

jitterPosObj <- position_jitter(width=1)
ggplot(loan, aes(x=loan$revolUtilRateBins, y=loan$loanAmountBins, col=loan$loan_status)) + geom_jitter(position=jitterPosObj) 


# OBSERVATION       ::  More the loan amount more the Revol Util rate we see more the defaulters

#########################
# MULTIVARIATE ANALYSIS #
#########################

#1.
# COLUMNS CONSIDERED :: loanAmountBins Vs annualIncomeBins Vs home_ownership

jitterPosObj <- position_jitter(width=1)
ggplot(loan, aes(x=loan$annualIncomeBins, y=loan$loanAmountBins, shape =loan$loan_status, col = loan$home_ownership)) + geom_jitter(position=jitterPosObj)  


#2.
# COLUMNS CONSIDERED :: loanAmountBins Vs intRateBins Vs term

jitterPosObj <- position_jitter(width=1)
ggplot(loan, aes(x=loan$intRateBins, y=loan$loanAmountBins, col = loan$loan_status, shape = loan$term)) + geom_jitter(position=jitterPosObj)  


