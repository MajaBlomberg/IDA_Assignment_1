############### Task 3 ##############

### A ###
#Firstly I will set a seed for simplicity 
set.seed(970903)

#Now I generate the standard normal Z's
Z_1= rnorm(500, mean=0, sd=1)
Z_2= rnorm(500, mean=0, sd=1)
Z_3= rnorm(500, mean=0, sd=1)

#I then define our variables and keep one as Y_2comp this will be complete Y_2 
Y_1 = 1+Z_1
Y_2 = 5+2*Z_1+Z_2
Y_2comp = 5+2*Z_1+Z_2
#Define the constants
a=2
b=0
#Make a for loop for the missingness in Y_2
for(i in 1:length(Y_2)){
  if(a*(Y_1[i]-1)+b*(Y_2[i]-5)+Z_3[i]<0){
    Y_2[i] = NA
  }
}
#I created a data frame with our values in it 
df <- data.frame(Y1=Y_1, Y2=Y_2, Ycomp=Y_2comp)

#Now lets see the marginal density plot for Y_2 both observed and completed data
plot(density(df$Y2[-which(is.na(df$Y2) == TRUE)]), col = "blue", ylim = c(0,0.30),
     main = "Marginal distribution", xlab = "Y_2", ylab = "Density")
lines(density(df$Ycomp), col = "red")
legend(7.5, 0.27, legend = c("Obs data", "Compl. data"), col = c("blue", "red"),
       lty = c(1, 1), bty = "n")

### B ###
#Lets move on and impute using  stochastic regression imputation
#firstly I fit the model using a linear model 
fit <- lm(Y_2 ~ Y_1, data = df)
summary(fit)

#in the summary you can find the coefficient but for simplicity
fit$coefficients                #B0=3.05305 and B1=1.990057

#Now I predict for the missing values and make a new variable with the regression imputs
stoch_reg_prd <- predict(fit, newdata = df) + rnorm(nrow(df), 0, sigma(fit))
Y2_stoch <- ifelse(is.na(df$Y2), stoch_reg_prd, df$Y2)

#Plotting the original complete data and the new complete data 
plot(density(Y2_stoch), col = "blue", ylim = c(0,0.25),
     main = "Marginal distribution", xlab = "Y_2", ylab = "Density")
lines(density(df$Ycomp), col = "red")
legend(6.8, 0.26, legend = c("Imputed", "Complete"), col = c("blue", "red"),
       lty = c(1, 1), bty = "n")

### C ###
#Now i will change the constants a and b to see how the missingness is affected
a=0
b=2
#I'm gonna regenerate the original values for Y_2 to make new na values in Y_2.
Y_2 = 5+2*Z_1+Z_2

for(i in 1:length(Y_2)){
  if(a*(Y_1[i]-1)+b*(Y_2[i]-5)+Z_3[i]<0){
    Y_2[i] = NA
  }
}
#lets place them in the data frame 
df.1 <- data.frame(Y1=Y_1, Y2=Y_2, Ycomp=Y_2comp)

plot(density(df$Y2[-which(is.na(df.1$Y2) == TRUE)]), col = "blue", ylim = c(0,0.35),
     xlim=c(-1,12), main = "Marginal distribution", xlab = "Y_2", ylab = "Density")
lines(density(df.1$Ycomp), col = "red")
legend(7, 0.35, legend = c("Obs data", "Compl. data"), col = c("blue", "red"),
       lty = c(1, 1), bty = "n")

### D ###
#Lets do the same process as in B but with our new Y_2.
fit <- lm(Y_2 ~ Y_1, data = df.1)
summary(fit)

#in the summary you can find the coefficient but for simplicity
fit$coefficients                #B0=4.233651 and B1=1.497142

#Now I predict for the missing values
stoch_reg_prd <- predict(fit, newdata = df.1) + rnorm(nrow(df.1), 0, sigma(fit))
Y2_stoch <- ifelse(is.na(df.1$Y2), stoch_reg_prd, df.1$Y2)

#Plotting the original complete data and the new complete data 
plot(density(Y2_stoch), col = "blue", ylim = c(0,0.30),
     main = "Marginal distribution", xlab = "Y_2", ylab = "Density")
lines(density(df.1$Ycomp), col = "red")
legend(6.8, 0.26, legend = c("Imputed", "Complete"), col = c("blue", "red"),
       lty = c(1, 1), bty = "n")

############### Task 4 ###############

#open the databp data set

### A ###
#first we will do a CCA 

ind <- which(is.na(databp$recovtime) == FALSE) #we now now that we have 3 NA values (25-22)
mean(databp$recovtime, na.rm = TRUE) #our mean is 19.27273
sd(databp$recovtime, na.rm = TRUE)/sqrt(length(ind)) # our sd is 2.603013

#lets find the pearson correlation, first between recovtime and logdose
cor(databp$recovtime, databp$logdose, use = 'complete.obs', method = c("pearson")) # 0.2391256
#now for recovtime and bloodp
cor(databp$recovtime, databp$bloodp, use = 'complete.obs', method = c("pearson")) #-0.01952862

### B ###
#preforming mean inputation
mean(databp$recovtime, na.rm = TRUE)
recovtime_mi <- ifelse(is.na(databp$recovtime), 19.27273, databp$recovtime)
sd(recovtime_mi)  #11.42068

#lets find the pearson correlation, first between recovtime and logdose
cor(recovtime_mi, databp$logdose, method = c("pearson"))  # 0.2150612
#now for recovtime and bloodp
cor(recovtime_mi, databp$bloodp, method = c("pearson"))   #-0.01934126

### C ###
fit1 <- lm(databp$recovtime ~ databp$logdose + databp$bloodp, data = databp)
summary(fit1)   #B0=15.2159, B1=11.4290 and B2=-0.2769
predicted_mri <- predict(fit1, newdata = databp)
recovtime_mri <- ifelse(is.na(databp$recovtime), predicted_mri, databp$recovtime)
mean(recovtime_mri)   #19.44428
sd(recovtime_mri)       # 11.56422

#lets find the pearson correlation, first between recovtime and logdose
cor(recovtime_mri, databp$logdose, method = c("pearson"))  # 0.2801835
#now for recovtime and bloodp
cor(recovtime_mri, databp$bloodp, method = c("pearson"))   #-0.0111364

### D ###

predicted_sri <- predict(fit1, newdata = databp) + rnorm(nrow(databp), 0, sigma(fit1))
recovtime_sri <- ifelse(is.na(databp$recovtime), predicted_sri, databp$recovtime)

mean(recovtime_sri) #19.13798
sd(recovtime_sri) #11.46561

#lets find the pearson correlation, first between recovtime and logdose
cor(recovtime_sri, databp$logdose, method = c("pearson"))  # 0.2190227
#now for recovtime and bloodp
cor(recovtime_sri, databp$bloodp, method = c("pearson"))   #-0.01356628


### E ###
