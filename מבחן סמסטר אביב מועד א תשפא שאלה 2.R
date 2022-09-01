#נוסחא קבועה אל תיגע חבר
##### Sample from beta
SampleFromBeta<-function(n=1000,alpha=1,beta=1,a=0,c=1)
{
  rbeta(n,shape1=alpha,shape2=beta)*(c-a)+a
} 
#יצירת אלף דגימות מכל סוג
אלפא, בטא- נתונים מתוך השאלה (פרמטרים, בד"כ 3 ו3)
#A – מינימום שורה, C- מקסימום שורה
#30 דגימות 
#אלפה וביתא שונה לכל שורה  
a<-SampleFromBeta(30,alpha=0.2,beta=0.2,a=4,c=8)
b<-SampleFromBeta(30,alpha=0.5,beta=0.5,a=5,c=11)
c<-SampleFromBeta(30,alpha=0.5,beta=0.5,a=5,c=7)
d<-SampleFromBeta(30,alpha=1.6,beta=0.2,a=6,c=14)
e<-SampleFromBeta(30,alpha=0.1,beta=0.1,a=5,c=7)
f<-SampleFromBeta(30,alpha=0.1,beta=0.1,a=7,c=9)
g<-SampleFromBeta(30,alpha=1.6,beta=0.2,a=4,c=12)

A_C_F_G=a+c+f+g
A_D_F_G=a+d+f+g
A_E_G=a+e+g
B_E_G=b+e+g
project_data<-data.frame(A_C_F_G, A_D_F_G, A_E_G,B_E_G)
#בודק מה הערך הכי גבוה בכל שורה!
project_data$project_length<-apply(project_data,MARGIN=1,max)
project_data$project_length 

### ממוצע אורך פרויקט
Mean_Length<-mean(project_data$project_length)
#תוחלת משך פרויקט ==ממוצע משך פרויקט
### Standard Deviation סטיית תקן
SD_Length<-sd(project_data$project_length)
### Confidence intervals רווח סמך 
n<-30
Mean_Length+qt(1-0.05/2,n-1)*SD_Length/sqrt(n) ## Upper
Mean_Length-qt(1-0.05/2,n-1)*SD_Length/sqrt(n) ## Lower
#סעיף 2
new_d<-SampleFromBeta(30,alpha=1.6,beta=0.2,a=6,c=13)
A_D_F_G_new=a+new_d+f+g
project_data_new<-data.frame(A_C_F_G,A_D_F_G_new, A_E_G,B_E_G)
project_data_new$project_length<-apply(project_data_new,MARGIN=1,max)
project_data_new$project_length 

t.test(project_data_new$project_length,project_data$project_length )
1000*(mean(project_data$project_length)-mean(project_data_new$project_length))-500
library('fitdistrplus')
x<-project_data$project_length
f<-fitdist(x,"norm")
ks.test(x,mean(x),sd(x))
#הפי ואליו יוצא 0.96 ולכן זה גדול מ0.05 ולכן זה אכן מתפלג נורמלית (מקבלים השערת 0)
sample_mean<-function()
{
  n=30
  A<-SampleFromBeta(n,alpha=0.22,beta=0.22,a=4,c=8)
  B<-SampleFromBeta(n,alpha=0.5,beta=0.5,a=5,c=11)
  C<-SampleFromBeta(n,alpha=0.5,beta=0.5,a=5,c=7)
  D<-SampleFromBeta(n,alpha=1.6,beta=0.2,a=6,c=14)
  E<-SampleFromBeta(n,alpha=0.1,beta=0.1,a=5,c=7)
  F<-SampleFromBeta(n,alpha=0.1,beta=0.1,a=7,c=9)
  G<-SampleFromBeta(n,alpha=1.6,beta=0.2,a=4,c=12)
  
  ## Build projet's paths
  A_C_F_G<-A+C+F+G
  B_D_F_G<-B+D+F+G
  A_E_G<-A+E+G
  B_E_G<-B+E+G
  
  ## Calculate the project length
  project_data<-data.frame(A_C_F_G,
                           B_D_F_G,
                           A_E_G,
                           B_E_G) 
  project_data$project_length<-apply(project_data,MARGIN=1,max) 
  mean(project_data$project_length)
}
x<-replicate(30,sample_mean())
ks.test(x,mean(x),sd(x))
#בדקנו קלמגורוב סמירנוב 
