#Practice Code for Shiny

# practice new regression tab plots

misX = c(runif(20,1,20))
misY = c(runif(20, 5, 10))
qplot(miscX,miscY)+
  geom_abline(intercept = ll$coefficients[1], slope = ll$coefficients[2]) + geom_point(aes(x = 0, y = 6, color = "red"), shape = 9)
ll = lm(miscY~miscX)
ll$intercept
ll$V1
head(ll)
sign(-2)
sign(5)
sign(-66)
n = c(3,6,4,7)
m = c(5,2,6,3)
mean(n)
mean(m)
# mean diff = 1
mean(n-m)

library(ggplot2)
# Datasets for the outlier section



# Outlier, large residual

dataX1= c(seq(1,20, 1), 20)
dataY1 =  c(0.05, 0.40, 0.94, 1.69, 1.83, 3.06, 3.86, 4.14, 5.63, 8.69, 10.65, 11.16, 
            11.72, 12.69, 13.05, 14.38, 16.06, 17.75, 18.52, 19.55, 0)
datXY1 = data.frame(dataX, dataY1)
ggplot(data = datXY1,aes(x = dataX1,y = dataY1)) +geom_point()+
  geom_point(aes(x = 15, y = 5), color= "dodgerblue") 

dat1.LM = lm(dataY1~dataX1)
summary(dat1.LM)

dat1.LM.noOutlier = lm(dataY1[-21]~dataX1[-21])
summary(dat1.LM.noOutlier)


ggplot(data = datXY1,aes(x = dataX1,y = dataY1)) +geom_point()+
  geom_point(aes(x = 20, y = 0), color= "dodgerblue") +
 geom_abline(slope = dat1.LM$coefficients[2],intercept = dat1.LM$coefficients[1]) +
 geom_abline(slope = dat1.LM.noOutlier$coefficients[2],intercept = dat1.LM.noOutlier$coefficients[1])

# High leverage: doesn't affect the placement of the line
dataX2 = c(seq(1,20, 1),30)
dataY2 =  c(0.05, 0.40, 0.94, 1.69, 1.83, 3.06, 3.86, 4.14, 5.63, 7.69, 9.65, 10.16, 
            11.72, 12.69, 13.05, 14.38, 16.06, 17.75, 18.52, 19.55, 30)
datXY2 = data.frame(dataX2,dataY2)

ggplot(data = datXY2, aes(x = dataX2, dataY2)) + geom_point() +
  geom_point(aes(x = 30, y = 30), color= "dodgerblue") 



dat2.LM = lm(dataY2~dataX2)
dat2.LMSum = summary(dat2.LM)

dat2.LM.noHighLev = lm(dataY2[-21]~dataX2[-21])
summary(dat2.LM.noHighLev)


ggplot(data = datXY2, aes(x = dataX2, dataY2)) + geom_point() +
  geom_point(aes(x = 30, y = 30), colour= "darkred") +
  geom_abline(slope = dat2.LM$coefficients[2],intercept = dat2.LM$coefficients[1], colour = "royalblue", size = 1) +
  geom_abline(slope = dat2.LM.noHighLev$coefficients[2],intercept = dat2.LM.noHighLev$coefficients[1], linetype = "dashed", colour="darkseagreen1")

# High leverage

dataX3 = c(seq(1,20, 1),7)
dataY3 =  c(0.05, 0.40, 0.94, 1.69, 1.83, 3.06, 3.86, 4.14, 5.63, 7.69, 9.65, 10.16, 
            11.72, 12.69, 13.05, 14.38, 16.06, 17.75, 18.52, 19.55, 8)
datXY3 = data.frame(dataX3,dataY3)

ggplot(data = datXY3, aes(x = dataX3, dataY3)) + geom_point() +
  geom_point(aes(x = 7, y = 8), color= "dodgerblue") 





dat3.LM = lm(dataY3~dataX3)
summary(dat3.LM)

dat3.LM.noHighLev = lm(dataY3[-21]~dataX3[-21])
summary(dat3.LM.noHighLev)


ggplot(data = datXY3, aes(x = dataX3, dataY3)) + geom_point() +
  geom_point(aes(x = 20, y = 50), colour= "darkred") +
  geom_abline(slope = dat3.LM$coefficients[2],intercept = dat3.LM$coefficients[1], colour = "royalblue", size = 1) +
  geom_abline(slope = dat3.LM.noHighLev$coefficients[2],intercept = dat3.LM.noHighLev$coefficients[1], linetype = "dashed", colour="darkseagreen1")

####### trying to shorten proportion code
ones = c(1,0,0,1,1,1,1)
ones
qplot(ones)
qplot(as.character(ones))


######
x = c(1:10)
y = c(3,4,5,3,2,4,4,4,4,4)
b = line(x,y)
b
