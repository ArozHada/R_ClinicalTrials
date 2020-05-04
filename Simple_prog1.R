# simulated input values
 n = 100
 mu = 100
 sd = 10
 mu.d = 20
 age.mu = 50
 age.sd = 10
 
 # fix the seed for random number generation
  set.seed(123)
  # use "rnorm" to generate random normal placebo
 age = rnorm(n, age.mu, age.sd)
 bp.base = rnorm(n,mu,sd)
 bp.end = rnorm(n,mu,sd)
 # take the difference between endpoint and baseline
 bp.diff = bp.end-bp.base
 # put the data together using "cbind" to column-bind
dat4placebo = round(cbind(age,bp.base,bp.end,bp.diff))

#simulate data for new drug
age = rnorm(n, age.mu, age.sd)
bp.base = rnorm(n,mu,sd)
bp.end = rnorm(n,mu-mu.d,sd)
bp.diff = bp.end-bp.base
dat4drug = round(cbind(age,bp.base,bp.end,bp.diff))

 # make a dataframe to hold all data
   dat = data.frame(rbind(dat4placebo,dat4drug))

   # call boxplot
 boxplot(dat4placebo, las=1, main="Placebo")
 boxplot(dat4drug, las=1, main="Drug")
 
 library(lattice)
 # call xyplot function and print it
 print(xyplot(bp.diff~age|trt, data=dat,xlab="Age",
                strip=strip.custom(bg="white"),
                ylab="Blood Pressure Difference",lwd=3,cex=1.3,pch=20,
                type=c("p", "r")))
 ##Data Analysis
 #Fitting a linear model using lm and summary with ANOVA
 lm1 = lm(bp.diff~trt*age, data=dat)
 summary(lm1)
 
 library(xtable)
 # call xtable to make the table
   print(xtable(lm1, caption="ANOVA Table for Simulated
Clinical Trial Data", label = "tab4RI.coef"),
         table.placement = "htbp",caption.placement = "top")
   plot(lm1)
