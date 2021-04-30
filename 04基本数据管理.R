#创建leadership数据框
manager<-c(1,2,3,4,5)
date<-c("10/24/08","10/28/08","10/01/08",
        "10/12/08","5/1/09")
country<-c("US","US","UK","UK","UK")
gender<-c("M","F","F","M","F")
age<-c(32,45,25,39,99)
q1<-c(5,3,3,3,2)
q2<-c(4,5,5,3,2)
q3<-c(5,2,5,4,1)
q4<-c(5,5,5,NA,2)
q5<-c(5,5,2,NA,1)
leadership<-data.frame(manager,date,country,
                       gender,age,q1,q2,q3,
                       q4,q5,stringsAsFactors = FALSE)

#创建新变量
mydata<-data.frame(x1=c(2,2,6,4),
                   x2=c(3,4,2,8))
#fa1
mydata$sumx<-mydata$x1+mydata$x2
mydata$meanx<-(mydata$x1+mydata$x2)/2

#fa2
attach(mydata)
mydata$sumx<-x1+x2
mydata$meanx<-(x1+x2)/2

#fa3
mydata<-transform(mydata,
                  sumx=x1+x2,
                  meanx=(x1+x2)/2)
?transform

#变量的重编码
#fa1
 leadership$age[leadership$age==99]<-NA
leadership$agecat[leadership$age>75]<-"Elder"
leadership$agecat[leadership$age<=75&
                  leadership$age>=55]<-"Middle Aged"
leadership$agecat[leadership$age<55]<-"Young"
#fa2
leadership<-within(leadership,{
  agecat<-NA
  agecat[age>75]<-"Elder"
  agecat[age<=75&age>=55]<-"Middle Aged"
  agecat[age<55]<-"Young"})

#变量的重命名
#法1
fix(leadership)
#法2
names(leadership)[2]<-"testdate"
leadership
names(leadership)[6:10]<-c("item1","item2",
                           "item3","item4","item5"
                           )
leadership
#fa3
install.packages("plyr")
library(plyr)
leadership<-rename(leadership,
                   c(manager="managerid",
                     date="testdate"))
leadership

#缺失值
 y<-c(1,2,3,NA)
is.na(y)
is.na(leadership[,6:10])
#重编码某些值为缺失值
leadership$age[leadership$age==99]<-NA
 #分析中排除缺失值
x<-c(1,2,NA,3)
y<-sum(x,na.rm = TRUE)
#na.omit()删除不完整的观测
leadership
newdata<-na.omit(leadership)
newdata

#日期值
#定义日期输入格式
strdate<-c("01/05/1965","08/16/1975")
dates<-as.Date(strdate,"%m/%d/%Y")
dates

leadership
myformat<-"%m/%d/%y"
leadership$testdate<-as.Date(leadership$testdate
                             ,myformat)
Sys.Date()
date()

today<-Sys.Date()
format(today,format="%B/%d/%Y")
format(today,format="%A")

#日期相减
startdate<-as.Date("2004-02-13")
enddate<-as.Date("2011/01/22")
days<-enddate-startdate
days

today<Sys.Date()
dob<-as.Date("1996/01/08")
difftime(today,dob,units="hours")

#将日期转换为字符型变量
strdate
strdate<-as.character(dates)


#类型转换
a<-c(1,2,3)
is.numeric(a)
is.vector(x)
a<-as.character(a)
a
is.numeric(a)
is.character(a)
is.vector(a)

#数据排序
leadership
newdata<-leadership[order(leadership$age),]

attach(leadership)
newdata<-leadership[order(gender,age),]
newdata<-leadership[order(gender,-age),]

#数据集的合并
#向数据框添加列
total<-merge(dataframeA,dataframeB,by="id")
total<-merge(dataframeA,dataframeB,by=c("id","country"))
total<-cbind(a,b)
#向数据框添加行
total<-rbind(dataframA,dataframeB)

#数据集取子集
#选入保留变量
names(leadership)[6:10]<-c("q1","q2","q3","q4","q5")
#法1
newdata<-leadership[,c(6:10)]
#法2
myvars<-c("q1","q2","q3","q4","q5")
newdata<-leadership[myvars]
#法3
myvars<-paste("q",1:5,sep="")
myvars
newdata<-leadership[myvars]

##剔除(丢弃)变量
#fa1
myvars<-names(leadership)%in%c("q3","q4")
newdata<-leadership[!myvars]
#fa2
leadership[c(-8,-9)]

##选入观测
#选入1-3观测
newdata<-leadership[1:3,]
#选入所有30岁以上的男性
newdata<-leadership[leadership$age>30&
                    leadership$gender=="M",]
attach(leadership)
newdata<-leadership[age>30&
                      gender=="M",]
 
#研究范围固定在20091009-20091031之间
leadership<-as.Date(leadership$date,"%m/%d/%y")
startdate<-as.Date("2009-01-01")
enddate<-as.Date("2009-10-10")
newdate<-leadership[which(leadership$date>=startdate&leadership$date<=enddate),]
???????????
#subset()函数选取变量和和观测
newdata<-subset(leadership,age>=35|age<24,
                select=c(q1,q2,q3,q4))
newdata<-subset(leadership,gender="M"&age>25,
                select = gender:q4)
##随机抽样sample()函数
#从leadership数据集中随机抽取一个样本为3
mysample<-leadership[sample(1:nrow(leadership),3,replace = FALSE),]

#sql语句操作数据框
install.packages("sqldf")
mtcars
library(sqldf)
newdf<-sqldf("select*from mtcars 
             where carb=1
             order by mpg",
             row.names=TRUE)
newdf
sqldf("select avg(mpg) as avg_mpg,
             avg(disp) as  avg_disp,
             gear from mtcars
             where cyl in (4,6)
             group by gear")
