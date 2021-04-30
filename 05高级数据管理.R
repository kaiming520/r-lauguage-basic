#均数和标准差的计算
x<-c(1,2,3,4,5,6,7,8)
mean(x)
sd(x)
?transform
#数据的标准化
newdata<-scale(mydata)#M=0,SD=1
newdata<-scale(mydata)*SD+M#任意标准差=SD,均数=M的标准化
#对myvar这个变量进行SD=10,M=50的标化
newdata<-transform(mydata,myvar=scale(myvar)*10+50)

#正态分布函数
#在区间[-3,3]上绘制标准正态分布
?pretty
x<-pretty(c(-3,3),30)
y<-dnorm(x)
plot(x,y,
     type="l",
     xlab = "Normal Deviate",
     ylab="Density",
     yaxs="i")
pnorm(1.96)#分布函数，计算1.96标准正态分布左侧面积
qnorm(.9,mean=0,sd=1)#分位数函数，计算90%分位数值
rnorm(50,mean=50,sd=10)#随机数生成函数

#设定随机数种子
runif(5)
runif(5)
set.seed(1234)
runif(5)

#生成服从多元正态分布的数据
library(MASS)
options(digits = 3)
set.seed(1234)
mean<-c(230.7,146.7,3.6)
sigma<-matrix(c(15360.8,6721.2,-47.1,
              6721.2,4700.9,-16.5,
              -47.1,-16.5,0.3),nrow=3,ncol=3)
sigma
mydata<-mvrnorm(500,mean,sigma)
mydata<-as.data.frame(mydata)
names(mydata)<-c("y","x1","x2")
dim(mydata)#返回数据框有几行几列
head(mydata,n=10)

#字符处理函数
x<-c("ab","cde","fghij")
length(x)
nchar(x[3])#计算x中第三个数据的字符数

x<-"abcdef"
substr(x,2,4)#提取
substr(x,2,4)<-"22222"#替换x为“a222ef”
x

grep("a",c("b","a","c"),fixed = TRUE)
sub("\\s",".","hello there")
#"\\s"在这里是一个查找空格的表达式
strsplit("abc","")#将abc分割成a,b,c

paste("x",1:3,sep="")
paste("x",1:3)
paste("x",1:3,sep="M")#连接字符串，sep是分隔符
paste("Today is",date())

toupper("abc")#大写转换，返回ABC
tolower("ABC")#小写转换，返回abc

#其他实用函数
x<-c(2,5,6,9)
length(x)#计算变量x的长度

#seq(from,to,by)生成一个序列
indices<-seq(1,10,2)
indices

#rep(x,n)将x重复n次
y<-rep(1:3,2)
y

firstname<-c("Jane")
cat("Hello",firstname,"\n")
 

#将函数应用于数据对象
a<-5
sqrt(a)#开平方根
b<-c(1.243,5.654,2.99)
round(b)#四舍五入取整
c<-matrix(runif(12),nrow = 3)
c
log(c)
mean(c)

#apply()函数应用到一个矩阵的行列
mydata<-matrix(rnorm(30),nrow=6)
mydata
apply(mydata,1,mean)#计算每一行均值
apply(mydata,2,mean)#计算每一列均值
apply(mydata,2,mean,trim=0.2)#计算每一列截尾均值


#一组学生成绩根据姓氏和名字的首字母排序
#并给出A-F的成绩，一个例子
options(digits = 2)
student<-c("John Davis","Angela Williams","Bullwinkle Moose",
           "Davis Jones","Janice Markhammer","Cheryl Cushing",
           "Reuven Ytzrhak","Greg Knox","Joel England","Mary Rayburn")
Math<-c(502,600,412,358,495,512,410,625,573,522)
Science<-c(95,99,80,82,75,85,80,95,89,86)
English<-c(25,22,18,15,20,28,15,30,27,18)
roster<-data.frame(student, Math,Science,English,stringsAsFactors = FALSE)
#计算综合评分
z<-scale(roster[,2:4])
score<-apply(z,1,mean)
roster<-cbind(roster,score)
#对学生评分
y<-quantile(score,c(.8,.6,.4,.2))
roster$grade[score>=y[1]]<-"A"
roster$grade[score<y[1]&score>=y[2]]<-"B"
roster$grade[score<y[2]&score>=y[3]]<-"C"
roster$grade[score<y[3]&score>=y[4]]<-"D"
roster$grade[score<y[4]]<-"F"
#抽取姓氏和名字
name<-strsplit(roster$student," ")
lastname<-sapply(name,"[",2)
firstname<-sapply(name,"[",1)
#排序
roster<-roster[order(lastname,firstname),]

#循环语句
#for结构
for(i in 1:10) print("Hello")
#while结构
i<-10
while(i>0) {print("hello");i<-i-1}

#条件执行语句
 #if else结构
grade<-c("22","34")
if (is.character(grade)) grade<-as.factor(grade)
if(!is.factor(grade)) grade<-as.factor(grade) else print("grade already is a factor")

#ifelse结构
score<-0.6
ifelse(score>0.5,print("passed"),
       print("failed"))
outcome<-ifelse(score>0.5,"passed","failed")
outcome

#switch结构
 feeling<-c("sad","afraid")
for (i in feeling) 
        print(
                switch(i,
                       happy="I am glad you are happy",
                       afraid="There is nothing to fear",
                       sad="Cheer up",
                       angry="Calm down now"
                        
                )
        )
  
#用户自编函数
 #示例1
mystats<-function(x,parametric=TRUE,print=FALSE){
        if(parametric){
                center<-mean(x);spread<-sd(x)
        } else {
                center=median(x);spread=mad(x)
        }
        result=list(center=center,spread=spread)
        return(result)
}
set.seed(1234) 
x<-rnorm(500) 
y<-mystats(x) 
y 
y<-mystats(x,parametric = FALSE,print = TRUE) 
y 


#示例2 
mydate<-function(type="long"){
        switch(type,
               long=format(Sys.time(),"%A %B %d %Y"),
               short=format(Sys.time(),"%m-%d-%y"),
               cat(type,"is not a recognized type\n"))
}
mydate("long") 
mydate("short")
mydate()
mydate("medium")

#转置
cars<-mtcars[1:5,1:4]
cars
t(cars)

#整合数据
 options(digits = 3)
attach(mtcars)
aggdata<-aggregate(mtcars,by=list(cyl,gear),
                   FUN = mean,na.rm=TRUE)
aggdata
#reshape2
install.packages("reshape2")
library(reshape2)
id<-c(1,1,2,2)
time<-c(1,2,1,2)
x1<-c(5,3,6,2)
x2<-c(6,5,1,4)
mydata<-data.frame(id,time,x1,x2)
mydata
#融合
md<-melt(mydata,id=c("id","time"))
md
#重铸
a<-dcast(md,id~variable,mean)
a
b<-dcast(md,time~variable,mean)
b
c<-dcast(md,id~time,mean)
c
d<-dcast(md,id+time~variable)
d
e<-dcast(md,id+variable~time)
e
f<-dcast(md,id~variable+time)
f
