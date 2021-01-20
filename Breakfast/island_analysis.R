#Ordinal Logistic Regression


#STEP 1. data pre-processing
dat$energylevel[dat$energy <= 4] <- "low"
dat$energylevel[dat$energy == 5] <- "medium"
dat$energylevel[dat$energy == 6] <- "medium"
dat$energylevel[7 <= dat$energy] <- "high"
dat$breakfast = ifelse(dat$breakfast == "yes", 1, 0)
dat$sex = ifelse(dat$sex == "male", 0, 1)
dat2 = dat[, -c(1,3,4,8)]


#convert to factor
dat2$energylevel = as.factor(dat2$energylevel)
dat2$breakfast = as.factor(dat2$breakfast)
dat2$sex = as.factor(dat2$sex)

#make response ordered 
dat2$energylevel = as.ordered(dat2$energylevel)
levels(dat2$energylevel) ##high low medium

#make the levels: low < medium < high 
dat2$energylevel = factor(dat2$energylevel, levels = c("low", "medium", "high"))




#STEP 2. ordinal logistic regression
library(MASS)

#all x variables 
mm = polr(energylevel ~ breakfast + sex + exercise.hr + sleep.hr, 
         data = dat2, Hess = TRUE)
summary(mm)

#only breakfast model
mm1 = polr(energylevel ~ breakfast,data = dat2, Hess = TRUE)
summary(mm1) 
anova(mm1, mm, test = "Chisq") ##not significant 



#STEP 3. p-value calculation

#all x variables
(ctable <- coef(summary(m)))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE)*2
(ctable <-cbind(ctable, "p value" =p)) 


#only breakfast model
(ctable1 <- coef(summary(m1)))
p <- pnorm(abs(ctable1[, "t value"]), lower.tail = FALSE)*2
(ctable <-cbind(ctable1, "p value" =p)) 





##STEP 4. prediction
round(predict(m1, data.frame(breakfast = "1"), type = "p", data = dat2),3) #have breakfast 
round(predict(m1, data.frame(breakfast = "0"), type = "p", data = dat2),3) ##not have breakfast

m1$zeta ##intercept 

##no breakfast cumulative probability
nobrf= exp(m1$zeta)/(1+exp(m1$zeta))
round(nobrf,3)
##higher cum prob of low than breakfast group

##breakfast cumulative probability
brf= exp(m1$zeta - m1$coefficients)/(1 + exp(m1$zeta - m1$coefficients))
round(brf,3) 




##no breakfast odds
nobrf.odd = nobrf/(1-nobrf)
round(nobrf.odd,3)
##breakfast odds
brf.odd= brf/(1-brf)
round(brf.odd, 3)
##odd ratio
or = nobrf.odd/brf.odd ##odd ratio = 1.908
log(or) ## same as breakfast coefficient 
or


##assumption check(comparing with multinomial model)
library(nnet)
mlm <- multinom(energylevel ~ breakfast, data=dat2)

M1 <- logLik(m1)
M2 <- logLik(mlm)
(G <- -2*(M1[1] - M2[1]))

pchisq(G,3,lower.tail = FALSE)

