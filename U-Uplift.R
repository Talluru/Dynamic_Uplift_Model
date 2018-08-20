# libraries---------------------------
library(rgl)
library(car)
library(uplift)
library(glmnet)

library(rpart) 
library(partykit)       # for visualizing trees
library(rattle)      		# fancy tree plot
library(rpart.plot)			# enhanced tree plots
library(randomForest)   #for Random Forests
library(neuralnet)
library(nnet)


# Tuning parameters-----------------------------------------------------


#uplift_cutoff = 0.2
percent_of_cust_per_time = 0.02


#Fixed******************************************
# Probability distribution 
custAttrMean=6
custAttrSD=2

weightMean =0
weightSD=0.05

purchase_cutoff_prob =0.5
purchase_cycle_time_factor = 5


# Model Parameters
noOfCustomers = 1000
attrPerCust = 30
noOfTimeSteps = 300

loyaltyCycle=30
addictionCycle=30




# Probability plot function --------------------------------
plot_prob <-function(cusNo){
  
  
  offers=array()
  j=1
  
  for(i in 1:noOfTimeSteps){
    if(Time_Df[cusNo,n_common+2*i-1]==1) { offers[j]=i; j=j+1;}
    
  }
  print(offers)
  
  
  plot( 1:noOfTimeSteps, dynamic_control_prob[cusNo,1:noOfTimeSteps], col="red", pch =16, ylim = c(0,1),
        main = paste("Customer Number:", cusNo),
        xlab="Time", ylab="Purchase Probability",
        cex.lab=2, cex.main=2, cex.axis=2)
  points(1:noOfTimeSteps, dynamic_treatment_prob[cusNo,1:noOfTimeSteps], col="green", pch=17)
  points(offers, dynamic_treatment_prob[cusNo,offers], pch=11, cex=1)
  abline(h=purchase_cutoff_prob)
  
  # legend(0.8,1.2,
  #        c("Offer-1", "Offer-2", "Offer-3"),
  #        pch ="#",
  #        col=c("black","black","black"),
  #        pt.cex=2, cex=2, box.col = "black")
  
  # legend(0.8,1.2,
  #        c("Control", "Treatment-1","Treatment-2","Treatment-3", "Offer-1","Offer-2","Offer-3"),
  #        pch =c("0","15","16","17","11","15","#"),
  #        col=c("red", "green","brown","blue","black","black","black"),
  #        pt.cex=2, cex=1, box.col = "black")
  
}

# Customer Data frame ----------------------------
set.seed(1)
customer_Df <- data.frame(id = 1:noOfCustomers)

for(i in 1:attrPerCust){
  
  customer_Df[, ncol(customer_Df) + 1] <- as.integer( rnorm(noOfCustomers,custAttrMean,custAttrSD))
  names(customer_Df)[ncol(customer_Df)] <- paste("attr_", i, sep="")
}


# Weights Matrix----------------------
# Control Weights
set.seed(1)
Weights=data.frame(control=rnorm(attrPerCust,weightMean, weightSD))
Weights$treatment1= Weights$control
Weights$treatment2= Weights$control
Weights$treatment3= Weights$control

temp_n = as.integer(attrPerCust/3)

#Treatment-1 weights
Weights$treatment1[1:temp_n]= Weights$control[1:temp_n]+rnorm(temp_n,0.01,0.01)

Weights$treatment1[(2*temp_n+1):(3*temp_n)]= 
  Weights$control[(2*temp_n+1):(3*temp_n)]-rnorm(temp_n,0.005,0.01)

#Treatment-2 weights
Weights$treatment2[temp_n+1:(2*temp_n)]= Weights$control[temp_n+1:(2*temp_n)]+
  rnorm(temp_n,0.01,0.01)
Weights$treatment2[(2*temp_n+1):(3*temp_n)]= 
  Weights$control[(2*temp_n+1):(3*temp_n)]-rnorm(temp_n,0.005,0.01)

#Treatment-3 weights
Weights$treatment3[1:temp_n]= Weights$control[1:temp_n]+rnorm(temp_n,0.01,0.01)
Weights$treatment3[(2*temp_n+1):(3*temp_n)]= 
  Weights$control[(2*temp_n+1):(3*temp_n)]-rnorm(temp_n,0.005,0.01)



# Static Control and treatment probability-----------------------------------
Purchase_odds <- as.matrix(customer_Df[,2:(attrPerCust+1)]) %*% as.matrix(Weights)
Purchase_odds2 <- as.data.frame(Purchase_odds) 

Purchase_odds2$control_prob <- exp(Purchase_odds2$control)/(1+exp(Purchase_odds2$control))
Purchase_odds2$treatmnt_prob1 <- exp(Purchase_odds2$treatment1)/(1+exp(Purchase_odds2$treatment1))
Purchase_odds2$treatmnt_prob2 <- exp(Purchase_odds2$treatment2)/(1+exp(Purchase_odds2$treatment2))
Purchase_odds2$treatmnt_prob3 <- exp(Purchase_odds2$treatment3)/(1+exp(Purchase_odds2$treatment3))


par(mfrow=c(3,2))
par(mar=c(5,5,5,5))
hist(Purchase_odds2$control, main = "Control Odds", col="red", 
     breaks = 15, xlab = NULL, cex.lab=1.75, cex.main=1.75)

hist(Purchase_odds2$treatment1, main = "Treatment-1 Odds", col="green", 
     breaks = 15, xlab = NULL, cex.lab=1.75, cex.main=1.75)

hist(Purchase_odds2$control_prob, main = "Control Probability", col="red", 
     breaks = 15, xlab = NULL, cex.lab=1.75, cex.main=1.75)

hist(Purchase_odds2$treatmnt_prob1, main = "Treatment-1 Probability", col="green", 
     breaks = 15, xlab = NULL, cex.lab=1.75, cex.main=1.75)

hist(Weights$control, col="red", main="Control Weights", 
     breaks = 7, xlab = NULL, cex.lab=1.75, cex.main=1.75)

hist(Weights$treatment1, col="green", main="Treatment-1 Weights", 
     breaks = 8, xlab = NULL, cex.lab=1.75, cex.main=1.75)

print( paste("Mean Control Probability:", mean(Purchase_odds2$control_prob)))
print( paste("Treatment Probability:", mean(Purchase_odds2$treatmnt_prob1)))

print( paste("Mean Control Weight:", mean(Weights$control)))
print( paste("Mean Treatment Weight:", mean(Weights$treatment1)))




# Time data frame and Summary Data Frame---------------------------------------------
Time_Df <- data.frame(id = 1:noOfCustomers)
Cost_Df <- data.frame(time = 1:noOfTimeSteps)
Campaign_Df <- data.frame()


Time_Df$last_purchase_time = 0
Time_Df$No_of_purchases =0
Time_Df$No_of_offers=0
Time_Df$loyalty_score = 0
Time_Df$addiction_score = 0
Time_Df$purchase_cycle = (customer_Df$attr_5 + customer_Df$attr_10)*purchase_cycle_time_factor

# Above 6 plus one for id
n_common = 6+1

for(i in 1:noOfTimeSteps){
  
  Time_Df[, ncol(Time_Df) + 1] <- 0
  names(Time_Df)[ncol(Time_Df)] <- paste("offer_", i, sep="")
  
  Time_Df[, ncol(Time_Df) + 1] <- 0
  names(Time_Df)[ncol(Time_Df)] <- paste("purchase_", i, sep="")
  
  
}

# Summary Data frame initialization

Summary_Df <- data.frame(time = 1:noOfTimeSteps)
Summary_Df$Total_offers <- percent_of_cust_per_time * noOfCustomers
Summary_Df$Cum_total_offers <- 0
Summary_Df$Control_purchases <- 0
Summary_Df$Treatment_purchases <-0
Summary_Df$Cum_Control_purchases <- 0
Summary_Df$Cum_Treatment_purchases <-0


# Taking the model through time steps-----------------------------
set.seed(1)


dynamic_control_prob = data.frame(id = 1:noOfCustomers)
dynamic_treatment_prob = data.frame(id = 1:noOfCustomers)
Dynamic_Campaign_Df = data.frame()

for(i in 1:noOfTimeSteps){
  
  
  if (i==TRUE){
    Time_Df[,n_common+(2*i)-1] = sample(c(0,1), 
                                        size=noOfCustomers, 
                                        replace=TRUE, 
                                        prob=c((1-percent_of_cust_per_time),
                                               percent_of_cust_per_time))
  }else{
    
    Time_Df[, n_common+(2*i)-1] =0
    #Time_Df[next_subgroup,n_common+(2*i)-1] =1
  }
  
  #Dependense of control and treatment probability on dynamic parameters
  control_prob= array()
  treatment_prob=array()
  
  for(j in 1:noOfCustomers){
    control_prob[j] = Purchase_odds2$control[j]*
      ((i- Time_Df$last_purchase_time[j])/Time_Df$purchase_cycle[j])*
      (1.01**(6*Time_Df$loyalty_score[j])/(1+1.01**(6*Time_Df$loyalty_score[j])))
    
    treatment_prob[j] = Purchase_odds2$treatment1[j]* 
      ((i- Time_Df$last_purchase_time[j])/Time_Df$purchase_cycle[j])*
      (1.1-(1.01**(Time_Df$addiction_score[j]) /(1+1.01**(Time_Df$addiction_score[j]))))
  }
  
  dynamic_control_prob[,i] = control_prob
  dynamic_treatment_prob[,i] = treatment_prob
  
  Time_Df[,n_common+(2*i)] = ifelse(Time_Df[,n_common+(2*i)-1]==1, 
                                    as.numeric(dynamic_treatment_prob[,i]>purchase_cutoff_prob),
                                    as.numeric(dynamic_control_prob[,i]>=purchase_cutoff_prob))
  
  #Updating Summary Df
  Summary_Df$Control_purchases[i] = sum( !Time_Df[,n_common+(2*i)-1] & Time_Df[,n_common+(2*i)])
  Summary_Df$Treatment_purchases[i] = sum(Time_Df[,n_common+(2*i)-1] & Time_Df[,n_common+(2*i)] )
  
  if(i>1){
    
    Summary_Df$Cum_total_offers[i] = Summary_Df$Cum_total_offers[i-1] + 
      Summary_Df$Total_offers[i]
    
    Summary_Df$Cum_Control_purchases[i] = Summary_Df$Cum_Control_purchases[i-1] +
      Summary_Df$Control_purchases[i]
    
    Summary_Df$Cum_Treatment_purchases[i] = Summary_Df$Cum_Treatment_purchases[i-1] +
      Summary_Df$Treatment_purchases[i]
    
  }
  
  # Record of last purchase time of customer
  Temp_last_purchase = Time_Df$last_purchase_time
  
  Time_Df$last_purchase_time = ifelse(Time_Df[,n_common+(2*i)]==1,
                                      i, Time_Df$last_purchase_time)
  
  # Record of number of purchases and number of offers
  Time_Df$No_of_purchases = ifelse(Time_Df[,n_common+(2*i)]==1,
                                   Time_Df$No_of_purchases+1, Time_Df$No_of_purchases)
  
  Time_Df$No_of_offers = ifelse(Time_Df[,n_common+(2*i)-1]==1,
                                Time_Df$No_of_offers+1, Time_Df$No_of_offers)
  
  # Loyalty score update ( attribute-1)
  Time_Df$loyalty_score = ifelse(Time_Df[,n_common+(2*i)]==1, 
                                 Time_Df$loyalty_score+customer_Df$attr_1,
                                 Time_Df$loyalty_score)
  
  Time_Df$loyalty_score= ifelse(Time_Df$loyalty_score> loyaltyCycle,
                                Time_Df$loyalty_score-loyaltyCycle, Time_Df$loyalty_score)
  
  # Addiction score update (attribute-2)
  Time_Df$addiction_score = ifelse(Time_Df[,n_common+(2*i)-1]==1, 
                                   Time_Df$addiction_score+customer_Df$attr_2,
                                   Time_Df$addiction_score)
  
  Time_Df$addiction_score= ifelse(Time_Df$addiction_score>addictionCycle,
                                  Time_Df$addiction_score-addictionCycle, Time_Df$addiction_score)
  
  #Adding rows to Campaign Df
  temp_df= data.frame()
  x= c(Time_Df[,n_common+(2*i)]==1|| Time_Df[,n_common+(2*i)]==0)
  temp_df =customer_Df[x,]
  temp_df$No_of_purchases=Time_Df$No_of_purchases[x]
  temp_df$No_of_offers=Time_Df$No_of_offers[x]
  
  temp_df$time_diff= ifelse(Time_Df[x,n_common+(2*i)]==1, i-Temp_last_purchase[x],
                            i-Time_Df$last_purchase_time[x])
  
  temp_df$offer= Time_Df[x,n_common+(2*i-1)]
  temp_df$purchase = Time_Df[x,n_common+(2*i)]
  
  Dynamic_Campaign_Df = rbind(Dynamic_Campaign_Df,temp_df)
  
  #next_subgroup = Dtree_Uplift(Dynamic_Campaign_Df, i)
  
  print(paste("Time Step", i))
  #print(next_subgroup)
  print("\n\n\n")
  
  
} # End of for

# End of modelling ----------------------


# Unified uplit Uplift model----------------------------------------------
U_Uplift <- function(Current_Campg_Df, Current_TimeStep){
  

  
  U_model = upliftRF(purchase~.+ trt(offer), 
                 data = Current_Campg_Df[,c(2:36)],
                 ntree = 10,
                 mtry =5,
                 split_method = "KL",
                 interaction.depth =2,
                 verbose = TRUE)
  
  
  predict_DF <- customer_Df[,2:31]
  predict_DF$No_of_purchases <- Time_Df$No_of_purchases
  predict_DF$No_of_offers <- Time_Df$No_of_offers
  predict_DF$time_diff <- (Current_TimeStep - Time_Df$last_purchase_time)
  
  predict_DF$offer =0
  
  predict.upliftRF(U_model, predict_DF, n.trees = U_model$ntree, predict.all = TRUE)
  
  
  pd = data.frame(id=1:noOfCustomers, uplift= (p_t-p_c))
  
  pd = pd[order(-pd$uplift),]
  
  limit = as.integer(percent_of_cust_per_time*noOfCustomers)
  next_subgroup = pd$id[1:limit]
  
  return (next_subgroup)
  
}

U_Uplift2 <- function(Current_Campg_Df, Current_TimeStep){
  
  
  
  U_model = ccif(purchase~.+ trt(Current_Campg_Df$offer), 
                 data = Current_Campg_Df[,c(2:34,36)],
                 ntree = 20,
                 split_method = "ED",
                 mtry= 33,
                 minsplit=5000)
  
  predict_DF <- customer_Df[,2:31]
  predict_DF$No_of_purchases <- Time_Df$No_of_purchases
  predict_DF$No_of_offers <- Time_Df$No_of_offers
  predict_DF$time_diff <- (Current_TimeStep - Time_Df$last_purchase_time)
  
  predict_DF$offer =1
  
  predict.ccif(U_model, predict_DF, n.trees = U_model$ntree, predict.all = TRUE)
  
  
  pd = data.frame(id=1:noOfCustomers, uplift= (p_t-p_c))
  
  pd = pd[order(-pd$uplift),]
  
  limit = as.integer(percent_of_cust_per_time*noOfCustomers)
  next_subgroup = pd$id[1:limit]
  
  return (next_subgroup)
  
}

U_Uplift3 <- function(Current_Campg_Df, Current_TimeStep){
  
  
  
  U_model = upliftKNN(purchase~.+ trt(offer), 
                     data = Current_Campg_Df[,c(2:36)],
                     ntree = 10,
                     mtry =5,
                     split_method = "KL",
                     interaction.depth =2,
                     verbose = TRUE)
  
  
  predict_DF <- customer_Df[,2:31]
  predict_DF$No_of_purchases <- Time_Df$No_of_purchases
  predict_DF$No_of_offers <- Time_Df$No_of_offers
  predict_DF$time_diff <- (Current_TimeStep - Time_Df$last_purchase_time)
  
  predict_DF$offer =0
  
  predict.upliftRF(U_model, predict_DF, n.trees = U_model$ntree, predict.all = TRUE)
  
  
  pd = data.frame(id=1:noOfCustomers, uplift= (p_t-p_c))
  
  pd = pd[order(-pd$uplift),]
  
  limit = as.integer(percent_of_cust_per_time*noOfCustomers)
  next_subgroup = pd$id[1:limit]
  
  return (next_subgroup)
  
}



Copy_data <- function(){
  
  return(list(Summary_Df, Time_Df, dynamic_control_prob, dynamic_treatment_prob))
  
}

# Dynamic probability plots----------------------------
#pdf('test_0.01.pdf')
par(mfrow=c(1,1))
for(i in 1:200){
  plot_prob(i)
  
}
#dev.off()


# Summary data------------------------------
Dt_0.02 
Dt_0.05
Dt_0.1
Dt_0.2 
Dt_0.5 = Copy_data()
# Base case Cum purchases plot--------------------------
t<- 1:noOfTimeSteps

par(mfrow=c(1,1))
plot(t,
     Zero_Treatment_Summary_Df$Cum_Treatment_purchases+Zero_Treatment_Summary_Df$Cum_Control_purchases, 
     type="l", col= "red", lwd=2, xlab="Time", ylab="Cumulative Purchases",
     main="Cum. Purchases with change in depth of treatment \n Double Decision Tree Uplift Model",
     cex.lab=2, cex.main=2, cex.axis=2, ylim=c(0,6000))

points(t,
       Dt_0.02[[1]]$Cum_Treatment_purchases+Dt_0.02[[1]]$Cum_Control_purchases, 
       type="l", col= rgb(0,1,0), lwd=2)

points(t,
       Dt_0.05[[1]]$Cum_Treatment_purchases+Dt_0.05[[1]]$Cum_Control_purchases, 
       type="l", col= rgb(0,0.8,0), lwd=2)

points(t,
       Dt_0.1[[1]]$Cum_Treatment_purchases+Dt_0.1[[1]]$Cum_Control_purchases, 
       type="l", col= rgb(0,0.5,0), lwd=2)

points(t,
       Dt_0.2[[1]]$Cum_Treatment_purchases+Dt_0.2[[1]]$Cum_Control_purchases, 
       type="l", col= rgb(0,0.25,0), lwd=2)

points(t,
       Dt_0.5[[1]]$Cum_Treatment_purchases+Dt_0.5[[1]]$Cum_Control_purchases, 
       type="l", col= "black", lwd=2)

#plot(-2:1,-2:1)

legend(0,6000,
       c("0%", "2%", "5%", "10%", "20%", "50%"),
       lty =1, lwd=5,
       col=c("red",rgb(0,1,0),rgb(0,0.7,0), rgb(0,0.4,0),rgb(0,0.2,0), "black"),
       cex=1, text.font = 40, box.col = "black")

#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# Trend plot-----------------------------------------------------
depth = c(0,2,5,10,20,30,40,50)
purc = c(4632,4994,5187,5361,5558,5648,5715,5741)


plot(depth, purc, xlab="Depth of Treatment %", ylab="Cumlative purchases",
     col=rgb(0,1,0), pch=15, type="b", lwd=3,
     cex.lab=2, cex.main=2, cex.axis=2, cex=2,
     main="Cumulative Purchases vs Depth of Treatment")
abline(v=10, col="red", lwd=3)

depth_logistic = c(0,2,5,10,20,50)
purc_logistic = c(4632, 4860, 4946, 5123, 5217, 5521)

points(depth_logistic,
       purc_logistic, 
       type="b", col= rgb(0,0.3,0), lwd=3, pch = 16, 
       cex.lab=2, cex.main=2, cex.axis=2, cex=2)

legend(21,5200,
       c("Random model", "Double Decision \n Tree"),
       lty =1, lwd=5, 
       col=c(rgb(0,1,0), rgb(0,0.3,0)),
       cex=1.5, text.font = 25, box.col = "white")
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

# Shift plot------------------------------------------------

#layout(matrix(c(1,2,3,4), 2,2, byrow=TRUE), widths = c(1,1), heights = c(1,1))

par(mar=c(5,5,5,5))

t<- 1:noOfTimeSteps

plot(t,
     Random_Case_0.1_Summary_Df$Cum_Treatment_purchases+
       Random_Case_0.1_Summary_Df$Cum_Control_purchases, 
     type="l", col=rgb(0,1,0), lwd=3, 
     xlab="Time", ylab="Cumulative Purchases", pch=16,
     main="Cum. Purchases with different models",
     cex.lab=2, cex.main=2, cex.axis=2)

points(t,
       Zero_Treatment_Summary_Df$Cum_Treatment_purchases+Zero_Treatment_Summary_Df$Cum_Control_purchases, 
       type="l", col="red", lwd=3, pch=16)

polygon(c(t, rev(t)), 
        c(Random_Case_0.1_Summary_Df$Cum_Treatment_purchases+ Random_Case_0.1_Summary_Df$Cum_Control_purchases,
          rev(Zero_Treatment_Summary_Df$Cum_Treatment_purchases+ Zero_Treatment_Summary_Df$Cum_Control_purchases)), 
        col = rgb(0,1,0, alpha = 0.3), border = NA) 

points(t,
       Dt_0.1[[1]]$Cum_Control_purchases+ Dt_0.1[[1]]$Cum_Treatment_purchases, 
       type="l", col=rgb(0,0.3,0), lwd=3, pch=16)

polygon(c(t, rev(t)), 
        c(Dt_0.1[[1]]$Cum_Control_purchases+ Dt_0.1[[1]]$Cum_Treatment_purchases,
          rev(Random_Case_0.1_Summary_Df$Cum_Treatment_purchases+ Random_Case_0.1_Summary_Df$Cum_Control_purchases)), 
        col = rgb(0,0.3,0, alpha = 0.3), border = NA) 

legend(0,5000,
       c("0% Treatment","10% Random", "10% Decision Tree"),
       lty =1, lwd=c(4,4,4),
       col=c("red", rgb(0,1,0), rgb(0,0.3,0)),
       cex=1.5, text.font = 30, box.col = "white")

# End of shift plot

