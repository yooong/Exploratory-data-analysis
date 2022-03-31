##############################데이터 정리####################################
#데이터 불러오기
setwd("C:/Users/이용휘/Desktop/탐자/프로젝트")
by_year<-read.csv("감염병_발생현황_연도별.csv")
by_month<-read.csv("감염병_발생현황_월별.csv")
by_agen<-read.csv("감염병_발생현황_연령및성별.csv")
influ<-read.csv("신종감염병증후군_월별.csv")
covid_m<-read.csv("코로나19_월별.csv")
covid_d<-read.csv("코로나19_일별.csv")

#데이터에서 결측치 0으로 바꾸기
for(i in c(2,4,5)){
by_year[by_year[,i]=="-",i]=0
by_month[by_month[,i]=="-",i]=0;
by_agen[by_agen[,(i+2)]=="-",(i+2)]=0
}
by_month[by_month[,9]=="-",9]=0
by_month[by_month[,10]=="-",10]=0

#데이터에서 숫자형이 문자형으로 되어있는 부분 숫자형으로 바꾸기
for (i in c(2,4,5)){
by_year[,i]<-as.numeric(by_year[,i])
by_agen[,i+2]<-as.numeric(by_agen[,i+2])
by_month[,i]<-as.numeric(by_month[,i])
}
by_month[,9]<-as.numeric(by_month[,9])
by_month[,10]<-as.numeric(by_month[,10])
str(by_year); str(by_agen); str(by_month)
###########################################################################


########################성과 연령별 감염자 수 그래프############################
.libPaths("C:/Users/Public/Rlib")
library("R.utils")
by_agen2<-by_agen
by_agen2<-aggregate(. ~ 연령+성별,by_agen2,sum)
for (i in 1:8){
by_agen2[i,1]=i
by_agen2[i+8,1]=i
}

coplot(A형간염~연령|성별, panel=function(x,y,pch,col){
	points(x,y,pch=pch,col=col); lines(y,col=y)},
	xlab=c("연령(1=0~9세, 2=10~19세, 3=20~29세, ···, 8=70세 이상)",
	"2011년~2019년까지 연령 별, 성 별 총 A형간염 감염자 수"),
	ylab="2011년~2019년까지 연령별 총 A형간염 감염자 수")  

coplot(유행성이하선염~연령|성별, panel=function(x,y,pch,col){
	points(x,y,pch=pch,col=col); lines(y,col=y)},
	xlab=c("연령(1=0~9세, 2=10~19세, 3=20~29세, ···, 8=70세 이상)",
	"2001년~2019년까지 연령 별, 성 별 총 유행성이하선염 감염자 수"),
	ylab="2001년~2019년까지 연령별 총 유행성이하선염 감염자 수")

coplot(B형간염.급성.~연령|성별, panel=function(x,y,pch,col){
	points(x,y,pch=pch,col=col); lines(y,col=y)},
	xlab=c("연령(1=0~9세, 2=10~19세, 3=20~29세, ···, 8=70세 이상)",
	"2011년~2019년까지 연령 별, 성 별 총 B형간염(급성) 감염자 수"),
	ylab="2001년~2019년까지 연령별 총 B형간염(급성) 감염자 수")

coplot(수두~연령|성별, panel=function(x,y,pch,col){
	points(x,y,pch=pch,col=col); lines(y,col=y)},
	xlab=c("연령(1=0~9세, 2=10~19세, 3=20~29세, ···, 8=70세 이상)",
	"2005년~2019년까지 연령 별, 성 별 총 수두 감염자 수"),
	ylab="2001년~2019년까지 연령별 총 수두 감염자 수")

coplot(성홍열~연령|성별, panel=function(x,y,pch,col){
	points(x,y,pch=pch,col=col); lines(y,col=y)},
	xlab=c("연령(1=0~9세, 2=10~19세, 3=20~29세, ···, 8=70세 이상)",
	"2001년~2019년까지 연령 별, 성 별 총 성홍열 감염자 수"),
	ylab="2001년~2019년까지 연령별 총 성홍열 감염자 수")

coplot(쯔쯔가무시증~연령|성별, panel=function(x,y,pch,col){
	points(x,y,pch=pch,col=col); lines(y,col=y)},
	xlab=c("연령(1=0~9세, 2=10~19세, 3=20~29세, ···, 8=70세 이상)",
	"2001년~2019년까지 연령 별, 성 별 총 쯔쯔가무시증 감염자 수"),
	ylab="2001년~2019년까지 연령별 총 쯔쯔가무시증 감염자 수")

coplot(신증후군출혈열~연령|성별, panel=function(x,y,pch,col){
	points(x,y,pch=pch,col=col); lines(y,col=y)},
	xlab=c("연령(1=0~9세, 2=10~19세, 3=20~29세, ···, 8=70세 이상)",
	"2001년~2019년까지 연령 별, 성 별 총 신증후군출혈열 감염자 수"),
	ylab="2001년~2019년까지 연령별 총 신증후군출혈열 감염자 수")

coplot(결핵~연령|성별, panel=function(x,y,pch,col){
	points(x,y,pch=pch,col=col); lines(y,col=y)},
	xlab=c("연령(1=0~9세, 2=10~19세, 3=20~29세, ···, 8=70세 이상)",
	"2001년~2019년까지 연령 별, 성 별 총 결핵 감염자 수"),
	ylab="2001년~2019년까지 연령별 총 결핵 감염자 수")

coplot(후천성면역결핍증~연령|성별, panel=function(x,y,pch,col){
	points(x,y,pch=pch,col=col); lines(y,col=y)},
	xlab=c("연령(1=0~9세, 2=10~19세, 3=20~29세, ···, 8=70세 이상)",
	"2001년~2019년까지 연령 별, 성 별 총 후천성면역결핍증 감염자 수"),
	ylab="2001년~2019년까지 연령별 총 후천성면역결핍증 감염자 수")
############################################################################




#################################감염재생산지수################################
infect=data.frame(by_month[2:228,1])
for(j in 2:10){
   for(i in 1:length(by_month$시점)-1){
      infect[i,j]=by_month[i+1,j]/by_month[i,j]
      }
   }

for(i in 2:10){
   infect[,i]=as.character(infect[,i])
   }

for(i in 2:10){
   for(j in 1:226){
      if(infect[j+1,i]=="NaN"){
         infect[j,i]="NaN"
         }
      }
   }
for(i in 2:10){
   infect[infect[,i]=="Inf",i]="NaN"
   infect[,i]=as.numeric(infect[,i])
   }
names(infect)=names(by_month[,1:10])
infect<-infect[,1:9]
############################################################################



############################감염재생산지수 박스플랏#############################
par(mfrow=c(1,1))
boxplot(infect[,2:9],xlab="질병",ylab="감염재생산지수",
	  main="각 질병의 월별 감염재생산지수",
	  col=c("skyblue1","skyblue2","deepskyblue1","deepskyblue2",
		  "deepskyblue3","deepskyblue4","cadetblue1","cadetblue2"))
boxplot(infect[,2:9],xlab="질병",ylab="감염재생산지수",
	  main="각 질병의 월별 감염재생산지수",ylim=c(0,6),
	  col=c("skyblue1","skyblue2","deepskyblue1","deepskyblue2",
		  "deepskyblue3","deepskyblue4","cadetblue1","cadetblue2"))
############################################################################





###########################감염재생산지수 히스토그램#############################
names(infect[2:9])
par(mfrow=c(4,2))
for(i in 2:5){
hist(infect[,i],breaks=15,xlab="감염재생산지수",
     main=paste(names(infect[2:9])[i-1],"의 감염재생산지수",sep=""),col=i)
}

par(mfrow=c(2,2))
for(i in 6:9){
hist(infect[,i],breaks=15,xlab="감염재생산지수",
     main=paste(names(infect[2:9])[i-1],"의 감염재생산지수",sep=""),col=i-4)
}
############################################################################




################################correlation##################################
cor_mon=matrix(nrow=8,ncol=8,
   dimnames=list(names(by_month[,2:9]),names(by_month[,2:9])))
for(i in 2:9){
   for(j in 2:9){
      cor_mon[i-1,j-1]=cor(by_month[145:228,i],by_month[145:228,j])
      }
   }
cor_mon

zs=by_month[,7:8]
zs=zs[order(zs[,2]),]
loessmod70=loess(쯔쯔가무시증 ~ 신증후군출혈열,zs,span=0.7,degree=2)
predict7=predict(loessmod70)
plot(zs[,2],zs[,1])
lines(zs[,2],predict7,col="red")
#############################################################################



#################################코로나########################################
by_month$index<-rep(c(1:12),19)
covid_m$index<-c(2:11)

par(mfrow=c(3,3))
for (i in 2:9){
plot(by_month[206:215,11],by_month[206:215,i],type="l",xlab="2018년 2월~11월",
     ylab="감염자 수",main=names(by_month[i]))
axis(1,seq(1,11,by=2))
}
plot(covid_m[,3],covid_m[,2],type="l",xlab="2020년 2월~11월",
     ylab="감염자 수",main="코로나19")
axis(1,seq(1,11,by=2))

par(mfrow=c(3,3))
for (i in 2:9){
plot(by_month[218:227,11],by_month[206:215,i],type="l",xlab="2019년 2월~11월",
     ylab="감염자 수",main=names(by_month[i]))
axis(1,seq(1,11,by=2))
}
plot(covid_m[,3],covid_m[,2],type="l",xlab="2020년 2월~11월",
     ylab="감염자 수",main="코로나19")
axis(1,seq(1,11,by=2))
##############################################################################