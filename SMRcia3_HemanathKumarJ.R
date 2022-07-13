SwedishMotorInsurance <- read_excel("C:/Users/Hk/Downloads/SwedishMotorInsurance.xlsx")
View(SwedishMotorInsurance)
summary(SwedishMotorInsurance)

par(mar=c(1,1,1,1))


cor(SwedishMotorInsurance$Claims,SwedishMotorInsurance$Payment)
plot(xlab="NumberofClaims",ylab="Payment",x=SwedishMotorInsurance$Claims, y=SwedishMotorInsurance$Payment) 
abline(lm(SwedishMotorInsurance$Payment~SwedishMotorInsurance$Claims), col="red")

cor(SwedishMotorInsurance$Insured,SwedishMotorInsurance$Payment)
plot(xlab="NumberofInsured",ylab="Payment",x=SwedishMotorInsurance$Insured, y=SwedishMotorInsurance$Payment)
abline(lm(SwedishMotorInsurance$Payment~SwedishMotorInsurance$Insured), col="red")


op1=lm(SwedishMotorInsurance$Payment~.,data=SwedishMotorInsurance)
summary(op1)
plot(op1)

tapply(SwedishMotorInsurance$Payment, SwedishMotorInsurance$Insured, mean)

q3zone<-apply(SwedishMotorInsurance[,c(5,6,7)], 2, function(x) tapply(x, SwedishMotorInsurance$Zone, mean)) 
summary(q3zone)
plot(q3bonus)

q3km<-apply(SwedishMotorInsurance[,c(5,6,7)],2,function(x)tapply(x,SwedishMotorInsurance$Kilometres,mean))
q3km
plot(q3km)

q3bonus<-apply(SwedishMotorInsurance[,c(5,6,7)],2,function(x)tapply(x,SwedishMotorInsurance$Bonus,mean))
q3bonus
plot(q3bonus)

cor(SwedishMotorInsurance$Claims,SwedishMotorInsurance$Kilometres)
cor(SwedishMotorInsurance$Claims,SwedishMotorInsurance$Zone)
cor(SwedishMotorInsurance$Claims,SwedishMotorInsurance$Bonus)
cor(SwedishMotorInsurance$Claims,SwedishMotorInsurance$Make)
cor(SwedishMotorInsurance$Claims,SwedishMotorInsurance$Insured)

q4=lm(Claims~Kilometres+Zone+Bonus+Make+Insured,data=SwedishMotorInsurance)
summary(q4)
plot(q4)
