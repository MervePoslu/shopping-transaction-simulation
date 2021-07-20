#' @importClassesFrom arules transactions
library(arules)
library(entropy) 
library(sqldf)
########################## #### Data Preparation #### ##########################

DH <- read.csv("/Users/merve.poslu/Downloads/transaction_data.csv") 
head(DH)
## Create a transactions data object for Arules
DH_list <- split(DH[,"PRODUCT_ID"], # ItemID
DH[,"BASKET_ID"])  # TransID 
DH_transactions <- as(DH_list,"transactions")
class(DH_transactions)

########################### #### Data Partitioning #### ###########################

# Randomly select a week for hold out
#WeekID <- sample(unique(DH$WEEK_NO),1) 
HoldoutWeekID <- 12 # hold-out week of our choice
# List of all transaction IDs in the same order of DH_transactions 
DH_transIDs <- names(DH_list)
# First, we will find the week-transID correspondence
TransWeek <- sqldf("SELECT DISTINCT WEEK_NO, BASKET_ID FROM DH")
# Then, we subset all transactions that happened in a particular week 
HoldoutBaskets <- subset(TransWeek, WEEK_NO==HoldoutWeekID)
nrow(HoldoutBaskets) # number of hold-out baskets 
nrow(TransWeek)-nrow(HoldoutBaskets) # number of training baskets
# ----- Splitting transactions into the training and Hold-out Sample Baskets ----- 
# İşlemleri eğitim ve hold-out örneklerine bölme---
#Holdout Baskets
SelectedTrans <- which(DH_transIDs %in% HoldoutBaskets$BASKET_ID) 
DH_trans_holdout <- DH_transactions[SelectedTrans]
#Training Baskets
UnselectedTrans <- setdiff(1:length(DH_transactions), SelectedTrans) 
DH_trans_train <- DH_transactions[UnselectedTrans]

######## Data Simulation for Holdout Week #### 

# number of unique items possible -- all possible items considered 
numitems.all <- nrow(DH_transactions@itemInfo)
# number of transactions to simulate -- equals # trans in the holdout week 
bSizes_actual <- size(DH_trans_holdout)
numtrans.ho <- length(bSizes_actual)
# ----- Independent Method -----
independentEXAMPLE <- random.transactions(numitems.all, numtrans.ho,method="independent") 
#Assuming default values
# ----- Agrawal Method -----
patterns <- random.patterns(numitems.all) #Assuming default values
agrawalEXAMPLE <- random.transactions(numitems.all, numtrans.ho, method="agrawal", patterns=patterns)
# ----- Two Step Method -----
# Step 1: simulate basket sizes
# Step 2: draw items for each basket
#Find out basket size distribution in training set 
bSizes_train <- size(DH_trans_train)
itemFreq_train <- itemFrequency(DH_trans_train, type="absolute") #Frequencies of the items in the holdout 
itemFreq_train_prob <- itemFreq_train / sum(itemFreq_train) #Calculate probabilities of the items in the training data
#Sizes of each basket (to be simulated)
bSizes_2step <- sample(bSizes_train, numtrans.ho, replace=T) 
simu_df <- NULL
for(i in 1:numtrans.ho){
  pick_items <- bSizes_2step[i]
  #Randomly pick these many items
  item_idx <- sample(1:length(itemFreq_train), pick_items,
                     prob=itemFreq_train_prob, replace=T)
  #Create a date frame of the randomly selected items and their corresponding transactions 
  twostep <- data.frame(BASKET_ID=i,
  PRODUCT_ID=item_idx) #Combine the dataset
simu_df <- rbind(simu_df,twostep) 
} 
#Put the simulation into the transactions format
twostep_list <- split(simu_df[,"PRODUCT_ID"], # ItemID
simu_df[,"BASKET_ID"]) # TransID 
twostep_trans <- as(twostep_list,"transactions")

 #### Evaluations ####
# ----- Item Frequency Comparisons -----
#Create vectors of item frequencies
itemFreq_actual <- itemFrequency(DH_trans_holdout, type="absolute") 
itemFreq_simuI <- itemFrequency(independentEXAMPLE, type="absolute") 
itemFreq_simuA <- itemFrequency(agrawalEXAMPLE, type="absolute") 
itemFreq_simu2 <- itemFrequency(twostep_trans, type="absolute")

#Comparisons of the distributions of sorted item frequencies 
par(mfrow=c(1,4))
barplot(sort(itemFreq_simuI), main="Independent") 
barplot(sort(itemFreq_simuA), main="Agrawal") 
barplot(sort(itemFreq_simu2), main="Two Step") 
barplot(sort(itemFreq_actual), main="Holdout") 
par(mfrow=c(1,1))

#Determine breaks for the KL Divergence computations
all_freqs <- c(itemFreq_actual,itemFreq_simuI,itemFreq_simuA,itemFreq_simu2) 
breaks <- seq(min(itemFreq_actual),max(itemFreq_actual),length.out=50)

#Change the item frequecies into probabilities by the breaks
itemFreq_actual_distr <- hist(itemFreq_actual, breaks=breaks, plot = FALSE)$counts 
itemFreq_simuI_distr <- hist(itemFreq_simuI, breaks=breaks, plot = FALSE)$counts 
itemFreq_simuA_distr <- hist(itemFreq_simuA, breaks=breaks, plot = FALSE)$counts 
itemFreq_simu2_distr <- hist(itemFreq_simu2, breaks=breaks, plot = FALSE)$counts

# ----- Basket Size Comparisons ----- 
#Create vectors of basket sizes
bSizes_indep <- size(independentEXAMPLE) 
bSizes_agrawal <- size(agrawalEXAMPLE) 
bSizes_2step <- size(twostep_trans)
bSizes_actual <- size(DH_trans_holdout) 
#Comparisons of the distributions of sorted basket sizes 
par(mfrow=c(1,4))
barplot(sort(bSizes_indep), main="Independent") 
barplot(sort(bSizes_agrawal), main="Agrawal") 
barplot(sort(bSizes_2step), main="Two Step") 
barplot(sort(bSizes_actual), main="Holdout") 
par(mfrow=c(1,1))


# ----- Basket Value Comparisons -----
#Create Prices dataset for items
DH$Price <- DH$SALES_VALUE/DH$QUANTITY #Create price for each item individually
DH$Price[is.nan(DH$Price)] <- 0.01 #Change any value of NaN to a penny
DH$Price[is.infinite(DH$Price)] <- 0.01 #Change any value of infinity to a penny
Prices <- aggregate(Price~PRODUCT_ID, data=DH,mean) #Create general prices by the mean of all the individual prices

#Convert Agrawal and Independent items IDs to UPCs 
item_labels <- DH_transactions@itemInfo
iLabels <- itemLabels(DH_transactions)
list <- LIST(DH_trans_holdout, decode = FALSE)
baskets <- list
list <- decode(list, itemLabels = iLabels)
baskets <- decode(baskets, itemLabels = iLabels) 
HO_baskets <- as(baskets,"matrix")

list2 <- LIST(agrawalEXAMPLE, decode = FALSE) 
baskets2 <- list2
list2 <- decode(list2, itemLabels = iLabels) 
baskets2 <- decode(baskets2, itemLabels = iLabels) 
AGR_baskets <- as(baskets2,"matrix")

list3 <- LIST(independentEXAMPLE, decode = FALSE) 
baskets3 <- list3
list3 <- decode(list3, itemLabels = iLabels)
baskets3 <- decode(baskets3, itemLabels = iLabels) 
IND_baskets <- as(baskets3,"matrix")

list4 <- LIST(twostep_trans, decode = FALSE) 
baskets4 <- list4
list4 <- decode(list4, itemLabels = iLabels) 
baskets4 <- decode(baskets4, itemLabels = iLabels) 
TwoStep_baskets <- as(baskets4,"matrix")

#Get Basket Prices
price_ho <- c()
HObaskettotals <- c()
for(i in 1:length(HO_baskets)){
  D <- (HO_baskets[i,])
  D <- unlist(D)
  for(d in 1:length(D)){
    itemnum <- which(Prices$PRODUCT_ID == D[d])
    price_ho[d] <- Prices$Price[itemnum]
    }
  HObaskettotals[i] <- sum(price_ho) 
}

price_agr <- c()
agrbaskettotals <- c()
for(i in 1:length(AGR_baskets)){
  D <- (AGR_baskets[i,]) 
  D <- unlist(D)
  for(d in 1:length(D)) {
    itemnum <- which(Prices$PRODUCT_ID == D[d])
    price_agr[d] <- Prices$Price[itemnum]
    }
  agrbaskettotals[i] <- sum(price_agr)
}

price_ind <- c()
indbaskettotals <- c()
for(i in 1:length(IND_baskets)){
  D <- (IND_baskets[i,]) 
  D <- unlist(D)
  for(d in 1:length(D)){
    itemnum <- which(Prices$PRODUCT_ID == D[d])
    price_ind[d] <- Prices$Price[itemnum]
    }
  indbaskettotals[i] <- sum(price_ind)
}

price_twostep <- c() 
twostepbaskettotals <- c()
for(i in 1:length(TwoStep_baskets)){
  D <- (TwoStep_baskets[i,]) 
  D <- unlist(D)
  for(d in 1:length(D)){
    itemnum <- which(Prices$PRODUCT_ID == D[d])
    price_twostep[d] <- Prices$Price[itemnum] }
  twostepbaskettotals[i] <- sum(price_twostep) }


#Comparisons of the distributions of sorted basket prices 
par(mfrow=c(1,4))
barplot(sort(indbaskettotals), main="Independent") 
barplot(sort(agrbaskettotals), main="Agrawal") 
barplot(sort(twostepbaskettotals), main="Two Step")
barplot(sort(HObaskettotals), main="Holdout") 
par(mfrow=c(1,1))

#Number of unique items possible in the holdout 
numitems.ho <- nrow(DH_trans_holdout@itemInfo)

#Number of transactions to simulate (equals # trans in the holdout week) 
bSizes.ho_actual <- size(DH_trans_holdout)
numtrans.ho <- length(bSizes.ho_actual)

#Number of transactions in the training 
bSizes_train <- size(DH_trans_train)

#Frequencies of the items in the holdout
itemFreq_train <- itemFrequency(DH_trans_train, type="absolute") 
itemFreq_train_prob <- itemFreq_train / sum(itemFreq_train)

################### #### Item Freq #### ###################
#Independent Item Frequency
item_frequency_ind <- function(num_items,num_trans,actual_item_freqs){
  ind <- random.transactions(num_items,num_trans,method="independent") 
  numitemsI <- itemFrequency(ind, type="absolute")
  breaks <- seq(min(actual_item_freqs),max(actual_item_freqs),length.out=30) 
  itemFreq_actual_distr <- hist(actual_item_freqs, breaks=breaks, plot = FALSE)$counts 
  itemFreq_simuI_distr <- hist(numitemsI, breaks=breaks, plot = FALSE)$counts 
  KL_item_ind <- KL.plugin(sort(itemFreq_simuI_distr),sort(itemFreq_actual_distr)) 
  KL_item_ind
}

#Agrawal Item Frequency, lPats = 2
item_frequency_agrawal2 <- function(num_items,num_trans,actual_item_freqs, corr){
  patterns <- random.patterns(num_items,corr=corr, lPats=2)
  agr <- random.transactions(num_items,num_trans,method="agrawal",patterns=patterns) 
  numitemsA <- itemFrequency(agr, type="absolute")
  breaks <- seq(min(actual_item_freqs),max(actual_item_freqs),length.out=30) 
  itemFreq_actual_distr <- hist(actual_item_freqs, breaks=breaks, plot = FALSE)$counts 
  itemFreq_simuA_distr <- hist(numitemsA, breaks=breaks, plot = FALSE)$counts 
  KL_item_agr <- KL.plugin(sort(itemFreq_simuA_distr),sort(itemFreq_actual_distr)) 
  KL_item_agr
}

#Agrawal Item Frequency, lPats = 4
item_frequency_agrawal4 <- function(num_items,num_trans,actual_item_freqs, corr){
  patterns <- random.patterns(num_items,corr=corr, lPats=4)
  agr <- random.transactions(num_items,num_trans,method="agrawal",patterns=patterns) 
  numitemsA <- itemFrequency(agr, type="absolute")
  breaks <- seq(min(actual_item_freqs),max(actual_item_freqs),length.out=30) 
  itemFreq_actual_distr <- hist(actual_item_freqs, breaks=breaks, plot = FALSE)$counts
  itemFreq_simuA_distr <- hist(numitemsA, breaks=breaks, plot = FALSE)$counts 
  KL_item_agr <- KL.plugin(sort(itemFreq_simuA_distr),sort(itemFreq_actual_distr)) 
  KL_item_agr
}

#Two Step Item Frequency
item_frequency_twostep <- function(training_basket_sizes,ho_trans,actual_item_freqs){ 
  bSizes_2step <- sample(training_basket_sizes, ho_trans, replace=T)
simu_df <- NULL
for(i in 1:ho_trans){
  pick_items <- bSizes_2step[i]
  item_idx <- sample(1:length(itemFreq_train), pick_items,
                     prob=itemFreq_train_prob, replace=F) 
  twostep <- data.frame(BASKET_ID=i,
             PRODUCT_ID=item_idx) 
  simu_df <- rbind(simu_df,twostep)
}
twostep_list <- split(simu_df[,"PRODUCT_ID"], # ItemID
                      simu_df[,"BASKET_ID"]) # TransID
twostep_trans <- as(twostep_list,"transactions")
numitems2 <- itemFrequency(twostep_trans, type="absolute")
breaks <- seq(min(actual_item_freqs),max(actual_item_freqs),length.out=30)
itemFreq_actual_distr <- hist(actual_item_freqs, breaks=breaks, plot = FALSE)$counts 
itemFreq_simu2_distr <- hist(numitems2, breaks=breaks, plot = FALSE)$counts 
KL_item_2step <- KL.plugin(sort(itemFreq_simu2_distr),sort(itemFreq_actual_distr)) 
KL_item_2step
itemFreq_actual <- itemFrequency(DH_trans_holdout, type="absolute")
#Independent Item Frequency KL Divergence
KL0 <- item_frequency_ind(numitems.ho,numtrans.ho,itemFreq_actual)
#Calculate KL Divergence for different correlations of Agrawal
x <- c(seq(-0.9,-0.1,0.1),-0.05,-0.01,-0.001,-0.0001,0.0001,0.001,0.01,0.05,seq(0.1,0.9,0.1))
y <- c()
for(i in 1:length(x)){
  cor_val <- x[i]
  y[i] <- item_frequency_agrawal2(numitems.ho,numtrans.ho,itemFreq_actual,cor_val)
}

yy <- c()
for(i in 1:length(x)){
  cor_val <- x[i]
  yy[i] <- item_frequency_agrawal4(numitems.ho,numtrans.ho,itemFreq_actual,cor_val)
}

y4 <- c()
for(i in 1:length(x)){
  cor_val <- x[i]
  y4[i] <- basket_size_agr4(numitems.ho,numtrans.ho,bSizes_actual,cor_val)
}

#Two Step Basket Size KL Divergence
KLB2step <- basket_size_twostep(bSizes_train,numtrans.ho,bSizes_actual)
#Plot the Basket Size KL Divergence for comparison
plot(x,y2,xlab="Correlation",ylab="KL Divergence",ylim=c(0.0,0.50),xlim = c(-1,1),type="o", col=3) 
lines(x,y4,type="o",col="purple")
lines(c(-1.1,1.1),c(KLB0,KLB0),col="red",lty=2) 
lines(c(-1.1,1.1),c(KLB2step,KLB2step),col=4,lty=2)
legend("topleft",c("Agarwal lPats=2","Agarwal lPats=4","Indpendent","Two Step"),
       lty=c(1,1,2,2), pch=c(1,1,NA,NA), col=c(3,"purple",2,4))


