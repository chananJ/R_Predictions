#____________________________טקסט פה___________________________
#____________________________שאלה 1___________________________
#____________________________שאלה 1 סעיף 1___________________________
data=read.csv(file.choose(),header = T)
head(data)

model1=glm(cbind(Deaths,Cases-Deaths)~Day,data,family = binomial())
summary(model1)
day2=data$Day^2
model2=glm(cbind(Deaths,Cases-Deaths)~Day+day2,data,family = binomial())
summary(model2)
#____________________________שאלה 1 סעיף 2___________________________
#Ps1=1/(1+exp(-1*(-3.328403-0.008821*day))) 
#ps2=1/(1+exp(-1*(--2.9590065-0.0570506*day+0.0013961*day^2))) 
#____________________________שאלה 1 סעיף 3___________________________
ps_data=data$Deaths/data$Cases
par("mar")
par(mar=c(1,1,1,1))
par(mar = c(5, 4, 4, 2) + 0.1)
plot(ps_data~data$Day,ylab = "סיכוי למוות",xlab= "יום")
points(predict(model1,data,type="response"),col="blue")
points(predict(model2,data,type="response"),col="yellow")
summary(model1)
summary(model2)
logLik(model1)
logLik(model)
#____________________________שאלה 1 סעיף 5___________________________
data_new=data.frame(Day=c(50))
predict(model1,data_new,type = "response")
data_new1=data.frame(Day=c(50),day2=c(50^2))
predict(model2,data_new1,type = "response")
#____________________________שאלה 2 סעיף 1___________________________
data=read.csv(file.choose(),header=T)
head(data)
## - create model 1
model1=glm(Offences ~ Duration,data= data,family=poisson())
summary(model1)
logLik(model1)

##- create a model 2 
model2=glm(Offences ~ log(Duration),data= data,family=poisson())
summary(model2)
logLik(model2)
##seif2
##model2 is better by loglik
data2=data.frame(Duration=c(20))
model20_m=predict(model2,data2,type="response",se=TRUE)
#---ערך צפוי $fit, סטיית תקן se.fit
#--רווח סמך---
c(model20_m$fit-1.96*model20_m$se.fit,model20_m$fit+1.96*model20_m$se.fit)/20
#____________________________שאלה 2 סעיף 4___________________________
model3=glm(Offences ~ Duration+Experience,data= data,family=poisson())
#predict _-____הסתברות פואסון למקרה 1 במשך 10 דקות מבחן 2020 מועד ב שאלה 2 סעיהף 4----
data4=data.frame(Duration=c(10),Experience=c(5))
preict_seif_4=predict(model3,data4,type="response",se=TRUE)
dpois(1,preict_seif_4$fit/10)
#____________________________שאלה 3___________________________
accs_before=c(6,2,4,4,7,4,2,3)
accs_after=c(2,3)
num_years_before=8
num_years_after=2
adt_before=c(1100,1200,1250,1300,1400,1350,1300,1350)
acc_model=0.015*adt_before^0.62
w=1/(1+sum(acc_model/1.1))
em=w+acc_model+(1-w)*accs_before
vm=em*(1-w)
pai=mean(accs_before)
lamda=mean(accs_after)
var_pai=sum(accs_before)/num_years_before^2
var_lamda=sum(accs_after)/num_years_after^2
sigma=pai-lamda
theta=lamda/pai/(1+var_pai/pai^2)
var_sigma=var_pai+var_lamda
var_theta=theta^2*(pai/pai^2+lamda/lamda^2)/(1+var_pai/pai^2)
#----שיפור באחוזים---
theta*100
#------רווח סמך---
c(theta-1.96*sqrt(var_theta),theta+1.96*sqrt(var_theta))
#--------ביסיאני-----
eb_per_year=accs_before/sum(accs_before)*em
#------נרמול של השנה---
pai=mean(eb_per_year)
lamda=mean(accs_after)
var_pai=sum(eb_per_year)/num_years_before^2
var_lamda=sum(accs_after)/num_years_after^2
sigma=pai-lamda
theta=lamda/pai/(1+var_pai/pai^2)
var_sigma=var_pai+var_lamda
var_theta=theta^2*(pai/pai^2+lamda/lamda^2)/(1+var_pai/pai^2)
#__________________________סעיף 3_______
1-(dpois(0,lamda)+dpois(1,lamda)+dpois(2,lamda))