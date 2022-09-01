#------------------פתרון בוחן אמצע-----------
#------------------------שאלה 1-------
data=read.csv(file.choose(),header = T)
head(data)
#-----------------------------------------------
##avrage in R is called mean! do not forget in test please
#-----------------------------------------------
mean_Accidents=mean(data$Accidents)
mean_Incidents=mean(data$Incidents)
#מיידע את הפונקציה שיש ערך ריק בעמודה הזו ולא כופה את כל הקובץ נתונים להשתנות
#-----------------------------------------------
mean_SafetyScore=mean(data$SafetyScore,na.rm = T)
#-------שאלה 2-----
#logR לממוצע מספר תאונות בשנה
#------------------------ככה מוצאים מספר מקרים-----
n=length(data$Incidents)
x=mean_Incidents
#--------------תוחלת---------
sd_x=sd(data$Incidents)
#---------------ככה מוצאים טי --------
t=qt(0.975,n-1)
#-----די זה מה שמוסיפים ומחסרים מאיקס גג------
d=t*(sd_x/sqrt(n))
#-----------נציג logR בתור וקטור איקס פלוס מינוס-----
c(x+d,x-d)
#-----------------------------------------------
#-------------------------שאלה 3----------------
model1=glm(I(Incidents>=1) ~ Accidents  , data=data,family=binomial)
model1
summary(model1)
b0=model1$coefficients[1]
b1=model1$coefficients[2]
b0
b1
#------------------שאלה 4-------------------
Accidents=c(10,20,30,40,50,60,70,80,90,100)
1/(1+exp(-1*(b0+b1*Accidents)))
#-----בפרדיקט תמיד שים לב שהכותרת של העמודה שבה התלות:זהה לשם התלות במודל-----------
#***********************************************************
data_new=data.frame(Accidents)
head(data_new)
predict_1=predict(model1,data_new,type = "response")
#------ליצור גרף של התחזית תכניס מודל לפרדיקט---
#----כשבפרדיקט יש טבלה עם כותרת שמתאימה לערך התלות במודל המקורי--
#----ואז בפלוט פשוט להכניס וקטור הערכים, עם השם שבו קראנו לו--
#--להכניס את את הפרדיקט ואת טווח ערכי התחזית האפשריים---
plot(Accidents,predict_1,ylim=c(0,1),type="l",ylab="הסיכוי לתאונה ",xlab = "מספר תקריות",col="yellow")
#-----שאלה 5------
model2<-glm(I(Incidents>=1) ~ I(Accidents>50), data,family=binomial)
#----------שאלה 6------------------
TP_model1=sum(predict(model1)>0 & data$Incidents>=1)
FP_model1=sum(predict(model1)>0 & data$Incidents==0)
TN_model1=sum(predict(model1)<=0 & data$Incidents>=1)
FN_model1=sum(predict(model1)<=0 & data$Incidents>=1)
TP_model1
FP_model1
TN_model1
FN_model1


### Model 2
##predict = 0 is probilaty equal 0.5
TP_model2=sum(predict(Model2)>0 & titanic_data$survived==1)
FP_model2=sum(predict(Model2)>0 & titanic_data$survived==0)
TN_model2=sum(predict(Model2)<=0 & titanic_data$survived==0)
FN_model2=sum(predict(Model2)<=0 & titanic_data$survived==1)
TP_model2
FP_model2
TN_model2
FN_model2

##Q4
##model1
Sen_model1=TP_model1/(TP_model1+FN_model1)
Sen_model1
Spe_model1=TN_model1/(TN_model1+FP_model1)
Spe_model1
PPV_model1=TP_model1/(TP_model1+FP_model1)
PPV_model1
NPV_model1=TN_model1/(TN_model1+FN_model1)
NPV_model1



##Q4
##model2
Sen_model2=TP_model2/(TP_model2+FN_model2)
Sen_model2
Spe_model2=TN_model2/(TN_model2+FP_model2)
Spe_model2
PPV_model2=TP_model2/(TP_model2+FP_model2)
PPV_model2
NPV_model2=TN_model2/(TN_model2+FN_model2)
NPV_model2
