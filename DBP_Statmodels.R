#read txt data file 
dat <- read.csv("C:/Users/eiju/Desktop/DBP.txt", sep="")
View(DBP)
# create the difference
 dat$diff = dat$DBP5-dat$DBP1
# print the first a few observation
 head(dat)
 # call R function "boxplot" for distribution plot
 boxplot(diff~TRT, dat, xlab="Treatment",
           ylab="DBP Changes", las=1)
 # Perform t-test with equal variance
  t.test(diff~TRT, dat, var.equal=T)
  
  # If assumption of equal variance is not true , Welch t-test
  t.test(diff~TRT, dat, var.equal=F)
  
  #F-test for variance to statistically test the null hypothesis of equal variances
  var.test(diff~TRT, dat)
  
  # Wilcox-test for assumptions of normal dist. not met
  wilcox.test(diff~TRT, dat)
  
  #One-sided T-test
  # data from treatment A
  diff.A = dat[dat$TRT=="A",]$diff
  # data from treatment B
  diff.B = dat[dat$TRT=="B",]$diff
  # call t.test for one-sided test
  t.test(diff.A, diff.B,alternative="less")
  
  # Bootstrapping in R 
  library(bootstrap)
  # define a function to calculate the mean difference between treatment groups A to B:
    mean.diff = function(bn,dat)
      diff(tapply(dat[bn,]$diff, dat[bn,]$TRT,mean))
  # number of bootstrap
    nboot = 1000
  # call "bootstrap" function
    boot.mean = bootstrap(1:nrow(dat), nboot, mean.diff,dat)
    
    # extract the mean differences
     x = boot.mean$thetastar
    # calculate the bootstrap quantiles
     x.quantile = quantile(x, c(0.025,0.5, 0.975))
    # show the quantiles
     print(x.quantile)
    
     # make a histogram
      hist(boot.mean$thetastar, xlab="Mean Differences", main="")
     # add the vertical lines for the quantiles
      abline(v=x.quantile,lwd=2, lty=c(4,1,4))
      
  #One-way ANOVA
      aggregate(dat[,3:7], list(TRT=dat$TRT), mean)
      
       # call reshape
      Dat = reshape(dat, direction="long",
                        varying=c("DBP1","DBP2","DBP3","DBP4","DBP5"),
                        idvar = c("Subject","TRT","Age","Sex","diff"),sep="")
      colnames(Dat) = c("Subject","TRT","Age","Sex","diff","Time","DBP")
      Dat$Time = as.factor(Dat$Time)
      # show the first 6 observations
      head(Dat)
      
      # test treatment "A"
       datA = Dat[Dat$TRT=="A",]
       test.A = aov(DBP~Time, datA)
       summary(test.A)
       
       #Test treatment B
       datB = Dat[Dat$TRT == "B",]
       test.B = aov(DBP~Time, datB)
        summary(test.B)
        
        TukeyHSD(test.A)
        TukeyHSD(test.B)
        
    #Two-way ANOVA for Interaction
        mod2 = aov(DBP~ TRT*Time, Dat)
         summary(mod2)
         
    #plot of interations  
         par(mfrow=c(2,1),mar=c(5,3,1,1))
          with(Dat,interaction.plot(Time,TRT,DBP,las=1,legend=T))
          with(Dat,interaction.plot(TRT,Time,DBP,las=1,legend=T))    
    
          #Tukey test
          TukeyHSD(aov(DBP ~ TRT*Time,Dat))    
          
  #MANOVA Analysis
  # attached the data into this R session
    attach(dat)
  # create the changes from baseline
    diff2to1 = DBP2-DBP1
    diff3to1 = DBP3-DBP1
    diff4to1 = DBP4-DBP1
    diff5to1 = DBP5-DBP1
    
  # calculate the correlations
     cor(cbind(diff2to1,diff3to1,diff4to1,diff5to1))  
  # calculate the mean changes
     MCh =aggregate(cbind(diff2to1,diff3to1,diff4to1,diff5to1),
                      list(TRT=TRT), mean)
    # print the chanhe
    print(MCh)   
    
    # call "manova" to fit a manova
    maov1=manova(cbind(diff2to1,diff3to1,diff4to1,diff5to1)~TRT,dat)
   # then F-test with Pillai (default in R)
    summary(maov1)  
    
    # F-test with Hotelling-Lawley
    summary(maov1, test="Hotelling-Lawley")
    # F-test with Wilks
    summary(maov1, test="Wilks")
    # F-test with Roy
    summary(maov1, test="Roy")
    