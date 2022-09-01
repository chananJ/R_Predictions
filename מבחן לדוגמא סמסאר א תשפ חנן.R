#------מבחן דוגמא תש"פ סמסטר א 2020---------
#-----------------question_1----------------
data=read.csv(file.choose(),header = T)
head(data)
data=data[!is.na(data$age),]
model1=glm(I(survived==1)~as.factor(data$pclass),data,family = binomial())
summary(model1)
b0=model1$coefficients[1]
b1=model1$coefficients[2]
b0
b1
#-----בי אפס מסמל את החיתוך של פונקציית החיזוי ובי 1 מסמל את שיפוע הפונקציה----
#----במשתנה קטוגריאלי ערך השיפוע של המשתנה הראשון שווה 0 הוא נקודת הייחוסולכן יהיה שיפוע לפי הספירה של הקטוגריות----
#------------(q2)----------
b2=model1$coefficients[3]
logLik(model1)
p=1/(1+exp(-1*(b0+b1*(data$pclass=='2nd')+b2*(data$pclass=='3rd'))))
sum(log(dbinom(data$survived,size=1,p)))
#----------------שאלה 3 הסברתי בוורד-------
#--------שאלה 4----------------------------
#model1=glm(I(survived==1)~as.factor(data$pclass),data,family = binomial())
model2=glm(I(survived==1)~age+as.factor(sex),data,family = binomial())
# TN -- True==1 died happend, Negative==predict no die
#----------המודל בודק כמה פעמים תשרוד---
TP_model1=sum(predict(model1,data)>0 & data$survived==1)
FP_model1=sum(predict(model1,data)>0 & data$survived==0)
TN_model1=sum(predict(model1,data)<=0 & data$survived==1)
FN_model1=sum(predict(model1,data)<=0 &data$survived==0)
#---------------
TP_model2=sum(predict(model2,data)>0 & data$survived==1)
FP_model2=sum(predict(model2,data)>0 & data$survived==0)
TN_model2=sum(predict(model2,data)<=0 & data$survived==1)
FN_model2=sum(predict(model2,data)<=0 &data$survived==0)
#---------------
#---ספציפי סנסטיבי מודל 1---------------------
Spe_model1=TN_model1/(TN_model1+FP_model1)
Spe_model1
Sen_model1=TP_model1/(TP_model1+FN_model1)
Sen_model1
#--------------ספציפי סנסטיבי מודל 2-----------------------
Spe_model2=TN_model2/(TN_model2+FP_model2)
Spe_model2
Sen_model2=TP_model2/(TP_model2+FN_model2)
Sen_model2
#___________________שאלה 2___________________
#_____________________סעיף 1__________
data_slips=read.csv(file.choose(),header = T)
head(data_slips)
data65=data_slips[data_slips$age<65,]
slips_65=tapply(data65$slip,data65$year, sum)

data_new=data.frame(slips=slips_65,
                    poulation=tapply(data65$pop,data65$year, sum),
                    year=2001:2016)
plot(data_new$slips~data_new$year,xlab="year",ylab = "slips")
text(data_new$year,data_new$slip+10,labels =data_new$slip,cex=1)
#להבין את הקטע של פקודת טקסט
#________שאלה 2____________________

model1=glm(slips~poulation +year,data=data_new[data_new$year<2013,],family=poisson())
summary(model1)
#----------------------מודל פואסוני------------------
##E(y)=e^(1022+0.0002243*pop-0.5472*year)
#----------------------רווח סמך ------------------
prediction=predict(model1,newdata=data_new,type="response",se=TRUE) 
#-----------רווח סמך קוד מפה---------
plot(data_new$slip~data_new$year,pch=10,
     xlab='year',ylab='slip',
     cex=3,cex.lab=1.4,cex.axis=1,ylim=c(0,2000))+
  arrows(data_new$year, prediction$fit-1.96*prediction$se.fit, 
         data_new$year, prediction$fit+1.96*prediction$se.fit,code=3,col="blue")
#---------------------- מודל פואסוני לוג האוכולוסיה------------------

model2=glm(slips~log(poulation),data=data_new,family=poisson())
#---------------------- רווח סמך לפי מודל 2------------
prediction2=predict(model2,newdata=data_new,type="response",se=TRUE) 
plot(data_new$slip~data_new$year,pch=10,
     xlab='year',ylab='slip',
     cex=3,cex.lab=1.4,cex.axis=1,ylim=c(0,2000))+
  arrows(data_new$year, prediction$fit-1.96*prediction$se.fit, 
         data_new$year, prediction$fit+1.96*prediction$se.fit,code=3,col="red")+
arrows(data_new$year, prediction2$fit-1.96*prediction2$se.fit, 
       data_new$year, prediction2$fit+1.96*prediction2$se.fit,code=3,col="red")

#________שיטה בסייאנית מבחנים לפני אחרי שינוי מובהק?_______
befora_after=function(lamda,pai,var_pai){
  sigma=abs(lamda-pai)
  var_sigma=lamda+var_pai
  theta = (lamda/pai)/(1+var_pai/(pai)^2)
  var_theta=theta^2*(1/lamda+var_pai/(pai)^2)/(1+var_pai/(pai)^2)^2
  return(cat("Reduction is",sigma,", with reduction sd of",round(var_sigma^0.5,1), "\n",
             "Reduction percent is",round((1-theta)*100,1),"%","with reduction sd percent of",round(var_theta^0.5,3),"%","\n"))
  
}
##example1 
befora_after(7,6.118,4.2)
